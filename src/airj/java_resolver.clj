(ns airj.java-resolver
  (:require [airj.expr-walker :as expr-walker]
            [airj.imported-interfaces :as imported-interfaces]
            [airj.java-members :as java-members]
            [airj.java-types :as java-types]
            [airj.patterns :as patterns]
            [airj.type-checker :as type-checker]))

(defn- fail!
  [message data]
  (throw (ex-info message (assoc data :phase :java-resolve))))

(defn- decl-map
  [module]
  (merge
   (into {}
         (mapcat (fn [[_ interface]]
                   (keep (fn [decl]
                           (when (contains? #{:data :enum :union} (:op decl))
                             [(:name decl) decl]))
                         (:decls interface))))
         (:interfaces module))
   (into {}
         (keep (fn [[symbol {:keys [decl]}]]
                 (when (contains? #{:data :enum :union} (:op decl))
                   [symbol decl])))
         (imported-interfaces/imported-decls module))
   (into {} (map (juxt :name identity) (:decls module)))))

(defn- imported-java-classes
  [module]
  (->> (:imports module)
       (filter #(= :java-import (:op %)))
       (map :class-name)
       set))

(defn- require-java-import!
  [class-name imports]
  (when-not (contains? imports class-name)
    (fail! "Missing Java import."
           {:class-name class-name})))

(defn- load-class
  [class-name]
  (try
    (java-types/load-class class-name)
    (catch ClassNotFoundException _
      (fail! "Unknown Java class."
             {:class-name class-name}))))

(defn resolve-type
  [type-expr]
  (or (java-types/resolve-type type-expr)
      (fail! "Unsupported Java type mapping."
             {:type type-expr})))

(defn- java-class-type?
  [type-expr]
  (java-types/java-type-expr? type-expr))

(defn- target-class
  [expr ctx decls imports]
  (let [target-type (type-checker/infer-expr-type expr ctx decls)]
    (when-not (java-class-type? target-type)
      (fail! "Expected Java receiver type."
             {:type target-type}))
    (require-java-import! (second target-type) imports)
    (resolve-type target-type)))

(defn- validate-java-new
  [expr ctx decls imports]
  (require-java-import! (:class-name expr) imports)
  (load-class (:class-name expr))
  (let [arg-types (mapv #(resolve-type (type-checker/infer-expr-type % ctx decls))
                        (:args expr))]
    (when-not (java-members/resolve-constructor (:class-name expr) arg-types)
      (fail! "Unknown Java constructor."
             {:class-name (:class-name expr)
              :arg-types arg-types}))))

(defn- validate-java-call
  [expr ctx decls imports]
  (let [receiver-type (type-checker/infer-expr-type (:target expr) ctx decls)
        klass (target-class (:target expr) ctx decls imports)]
    (when-not (java-members/resolve-instance-method receiver-type
                                                    (:member-id expr)
                                                    (:signature expr))
      (fail! "Unknown Java method."
             {:class-name (.getName klass)
              :member-id (:member-id expr)
              :signature (:signature expr)}))))

(defn- validate-java-static-call
  [expr _ctx _decls imports]
  (require-java-import! (:class-name expr) imports)
  (when-not (java-members/resolve-static-method (:class-name expr)
                                                (:member-id expr)
                                                (:signature expr))
    (let [klass (load-class (:class-name expr))]
      (fail! "Unknown Java method."
             {:class-name (:class-name expr)
              :member-id (:member-id expr)
              :signature (:signature expr)}))))

(defn- validate-java-get-field
  [expr ctx decls imports]
  (let [receiver-type (type-checker/infer-expr-type (:target expr) ctx decls)
        klass (target-class (:target expr) ctx decls imports)]
    (when-not (java-members/resolve-instance-field receiver-type
                                                   (:field-name expr)
                                                   (:field-type expr))
      (fail! "Unknown Java field."
             {:class-name (.getName klass)
              :field-name (:field-name expr)
              :field-type (:field-type expr)}))))

(defn- validate-java-set-field
  [expr ctx decls imports]
  (let [receiver-type (type-checker/infer-expr-type (:target expr) ctx decls)
        klass (target-class (:target expr) ctx decls imports)]
    (when-not (java-members/resolve-instance-field receiver-type
                                                   (:field-name expr)
                                                   (:field-type expr))
      (fail! "Unknown Java field."
             {:class-name (.getName klass)
              :field-name (:field-name expr)
              :field-type (:field-type expr)}))))

(defn- validate-java-static-get-field
  [expr imports]
  (require-java-import! (:class-name expr) imports)
  (let [klass (load-class (:class-name expr))]
    (when-not (java-members/resolve-static-field (:class-name expr)
                                                 (:field-name expr)
                                                 (:field-type expr))
      (fail! "Unknown Java field."
             {:class-name (.getName klass)
              :field-name (:field-name expr)
              :field-type (:field-type expr)}))))

(defn- validate-java-static-set-field
  [expr imports]
  (require-java-import! (:class-name expr) imports)
  (let [klass (load-class (:class-name expr))]
    (when-not (java-members/resolve-static-field (:class-name expr)
                                                 (:field-name expr)
                                                 (:field-type expr))
      (fail! "Unknown Java field."
             {:class-name (.getName klass)
              :field-name (:field-name expr)
              :field-type (:field-type expr)}))))

(defn- bind-local
  [ctx name type-expr]
  (assoc-in ctx [:locals name] type-expr))

(defn- imported-fn-locals
  [module]
  (into {}
        (keep (fn [[symbol {:keys [decl]}]]
                (when (= :fn (:op decl))
                  [symbol {:op :fn-type
                           :params (mapv :type (:params decl))
                           :return-type (:return-type decl)
                           :effects (vec (:effects decl))}])))
        (imported-interfaces/imported-decls module)))

(defn- bind-pattern-local
  [ctx pattern target-type _decls]
  (bind-local ctx (:name pattern) target-type))

(defn- bind-pattern-locals
  [ctx pattern target-type decls]
  (patterns/bind-pattern ctx
                         pattern
                         target-type
                         decls
                         {:bind-binder bind-pattern-local
                          :bind-literal (fn [current _pattern _target-type] current)
                          :fail! fail!}))

(defn- binding-ctx
  [ctx binding decls]
  (bind-local ctx
              (:name binding)
              (type-checker/infer-expr-type (:expr binding) ctx decls)))

(defn- let-handler
  [expr ctx decls walk]
  (let [body-ctx (reduce (fn [current binding]
                           (walk (:expr binding) current)
                           (binding-ctx current binding decls))
                         ctx
                         (:bindings expr))]
    (walk (:body expr) body-ctx)))

(defn- seq-handler
  [expr ctx walk]
  (reduce (fn [current part]
            (walk part current)
            (if (= :var (:op part))
              (bind-local current (:name part) (:type part))
              current))
          ctx
          (:exprs expr))
  nil)

(defn- loop-handler
  [expr ctx decls walk]
  (let [body-ctx (reduce (fn [current binding]
                           (walk (:expr binding) current)
                           (binding-ctx current binding decls))
                         ctx
                         (:bindings expr))]
    (walk (:body expr) body-ctx)))

(defn- handlers
  [decls imports]
  {:java-new (fn [expr ctx walk]
               (doseq [arg (:args expr)]
                 (walk arg ctx))
               (validate-java-new expr ctx decls imports))
   :java-call (fn [expr ctx walk]
                (walk (:target expr) ctx)
                (doseq [arg (:args expr)]
                  (walk arg ctx))
                (validate-java-call expr ctx decls imports))
   :java-static-call (fn [expr ctx walk]
                       (doseq [arg (:args expr)]
                         (walk arg ctx))
                       (validate-java-static-call expr ctx decls imports))
   :java-get-field (fn [expr ctx walk]
                     (walk (:target expr) ctx)
                     (validate-java-get-field expr ctx decls imports))
   :java-set-field (fn [expr ctx walk]
                     (walk (:target expr) ctx)
                     (walk (:expr expr) ctx)
                     (validate-java-set-field expr ctx decls imports))
   :java-static-get-field (fn [expr _ctx _walk]
                            (validate-java-static-get-field expr imports))
   :java-static-set-field (fn [expr ctx walk]
                            (walk (:expr expr) ctx)
                            (validate-java-static-set-field expr imports))
   :call (fn [expr ctx walk]
           (walk (:callee expr) ctx)
           (doseq [arg (:args expr)]
             (walk arg ctx)))
   :int-add (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-sub (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-mul (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-div (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-mod (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-eq (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :int-lt (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :int-le (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :int-gt (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :int-ge (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :bool-eq (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :int-ne (fn [expr ctx walk]
             (doseq [arg (:args expr)]
               (walk arg ctx)))
   :string-eq (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :string-concat (fn [expr ctx walk]
                    (doseq [arg (:args expr)]
                      (walk arg ctx)))
   :string-split-on (fn [expr ctx walk]
                      (doseq [arg (:args expr)]
                        (walk arg ctx)))
   :string-char-at (fn [expr ctx walk]
                     (doseq [arg (:args expr)]
                       (walk arg ctx)))
   :string-substring (fn [expr ctx walk]
                       (doseq [arg (:args expr)]
                         (walk arg ctx)))
   :int->string (fn [expr ctx walk]
                  (walk (:arg expr) ctx))
   :json-parse (fn [expr ctx walk]
                 (walk (:arg expr) ctx))
   :json-write (fn [expr ctx walk]
                 (walk (:arg expr) ctx))
   :env-get (fn [expr ctx walk]
              (walk (:arg expr) ctx))
   :process-run (fn [expr ctx walk]
                  (doseq [arg (:args expr)]
                    (walk arg ctx)))
   :http-listen (fn [expr ctx walk]
                  (walk (:arg expr) ctx))
   :http-port (fn [expr ctx walk]
                (walk (:arg expr) ctx))
   :http-accept (fn [expr ctx walk]
                  (walk (:arg expr) ctx))
   :http-respond (fn [expr ctx walk]
                   (doseq [arg (:args expr)]
                     (walk arg ctx)))
   :http-close (fn [expr ctx walk]
                 (walk (:arg expr) ctx))
   :string->int (fn [expr ctx walk]
                  (walk (:arg expr) ctx))
   :string-length (fn [expr ctx walk]
                    (walk (:arg expr) ctx))
   :string-trim (fn [expr ctx walk]
                  (walk (:arg expr) ctx))
   :string-empty? (fn [expr ctx walk]
                    (walk (:arg expr) ctx))
   :seq-empty? (fn [expr ctx walk]
                 (walk (:arg expr) ctx))
   :seq-length (fn [expr ctx walk]
                 (walk (:arg expr) ctx))
   :seq-first (fn [expr ctx walk]
                (walk (:arg expr) ctx))
   :seq-get (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :io-read-line (fn [_expr _ctx _walk] nil)
   :io-print (fn [expr ctx walk]
               (walk (:arg expr) ctx))
   :bool-not (fn [expr ctx walk]
               (walk (:arg expr) ctx))
   :bool-and (fn [expr ctx walk]
               (doseq [arg (:args expr)]
                 (walk arg ctx)))
   :bool-or (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :io-println (fn [expr ctx walk]
                 (walk (:arg expr) ctx))
   :construct (fn [expr ctx walk]
                (doseq [arg (:args expr)]
                  (walk arg ctx)))
   :variant (fn [expr ctx walk]
              (doseq [arg (:args expr)]
                (walk arg ctx)))
   :record-get (fn [expr ctx walk]
                 (walk (:target expr) ctx))
   :if (fn [expr ctx walk]
         (walk (:test expr) ctx)
         (walk (:then expr) ctx)
         (walk (:else expr) ctx))
   :match (fn [expr ctx walk]
            (walk (:target expr) ctx)
            (let [target-type (type-checker/infer-expr-type (:target expr) ctx decls)]
              (doseq [case (:cases expr)]
                (walk (:body case)
                      (bind-pattern-locals ctx
                                           (:pattern case)
                                           target-type
                                           decls)))))
   :let (fn [expr ctx walk]
          (let-handler expr ctx decls walk))
   :seq (fn [expr ctx walk]
          (seq-handler expr ctx walk))
   :lambda (fn [expr ctx walk]
             (walk (:body expr)
                   (reduce (fn [current param]
                             (bind-local current (:name param) (:type param)))
                           ctx
                           (:params expr))))
   :try (fn [expr ctx walk]
          (walk (:body expr) ctx)
          (doseq [catch (:catches expr)]
            (walk (:body catch)
                  (bind-local ctx (:name catch) (:type catch))))
          (when-let [finally-expr (:finally expr)]
            (walk finally-expr ctx)))
   :var (fn [expr ctx walk]
          (walk (:init expr) ctx))
   :set (fn [expr ctx walk]
          (walk (:expr expr) ctx))
   :loop (fn [expr ctx walk]
           (loop-handler expr ctx decls walk))
   :recur (fn [expr ctx walk]
            (doseq [arg (:args expr)]
              (walk arg ctx)))
   :raise (fn [expr ctx walk]
            (walk (:expr expr) ctx))})

(defn- fn-ctx
  [module decl decls]
  {:locals (merge
            (into {}
                  (map (fn [param]
                         [(:name param) (:type param)])
                       (:params decl)))
            (into {}
                  (map (fn [fn-decl]
                         [(:name fn-decl)
                          {:op :fn-type
                           :params (mapv :type (:params fn-decl))
                           :return-type (:return-type fn-decl)
                           :effects (vec (:effects fn-decl))}])
                       (filter #(= :fn (:op %)) (vals decls))))
            (imported-fn-locals module))
   :mutable #{}
   :available-modules (:available-modules module)
   :imported-decls (imported-interfaces/imported-decls module)})

(defn- check-fn-decl
  [module decl decls imports]
  (expr-walker/walk-expr (:body decl)
                         (fn-ctx module decl decls)
                         (handlers decls imports)))

(defn- check-host
  [module imports]
  (when-let [host (:host module)]
    (require-java-import! (:class-name host) imports)
    (load-class (:class-name host))))

(defn check-module
  [module]
  (let [decls (decl-map module)
        imports (imported-java-classes module)]
    (check-host module imports)
    (doseq [decl (:decls module)]
      (when (= :fn (:op decl))
        (check-fn-decl module decl decls imports)))
    module))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T13:33:12.577269-05:00", :module-hash "1822976168", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 7, :hash "-1087743549"} {:id "defn-/fail!", :kind "defn-", :line 9, :end-line 11, :hash "710273930"} {:id "defn-/decl-map", :kind "defn-", :line 13, :end-line 28, :hash "-2115152470"} {:id "defn-/imported-java-classes", :kind "defn-", :line 30, :end-line 35, :hash "1642403295"} {:id "defn-/require-java-import!", :kind "defn-", :line 37, :end-line 41, :hash "-900571965"} {:id "defn-/load-class", :kind "defn-", :line 43, :end-line 49, :hash "-546840002"} {:id "defn/resolve-type", :kind "defn", :line 51, :end-line 55, :hash "-2002111120"} {:id "defn-/java-class-type?", :kind "defn-", :line 57, :end-line 59, :hash "-1231681719"} {:id "defn-/target-class", :kind "defn-", :line 61, :end-line 68, :hash "1738177563"} {:id "defn-/validate-java-new", :kind "defn-", :line 70, :end-line 79, :hash "1111292230"} {:id "defn-/validate-java-call", :kind "defn-", :line 81, :end-line 91, :hash "311048070"} {:id "defn-/validate-java-static-call", :kind "defn-", :line 93, :end-line 103, :hash "325162171"} {:id "defn-/validate-java-get-field", :kind "defn-", :line 105, :end-line 115, :hash "25960113"} {:id "defn-/validate-java-set-field", :kind "defn-", :line 117, :end-line 127, :hash "-434496599"} {:id "defn-/validate-java-static-get-field", :kind "defn-", :line 129, :end-line 139, :hash "373500458"} {:id "defn-/validate-java-static-set-field", :kind "defn-", :line 141, :end-line 151, :hash "1207156350"} {:id "defn-/bind-local", :kind "defn-", :line 153, :end-line 155, :hash "1658319653"} {:id "defn-/imported-fn-locals", :kind "defn-", :line 157, :end-line 166, :hash "1754142719"} {:id "defn-/bind-pattern-local", :kind "defn-", :line 168, :end-line 170, :hash "-357983377"} {:id "defn-/bind-pattern-locals", :kind "defn-", :line 172, :end-line 180, :hash "-954573492"} {:id "defn-/binding-ctx", :kind "defn-", :line 182, :end-line 186, :hash "959377042"} {:id "defn-/let-handler", :kind "defn-", :line 188, :end-line 195, :hash "1021191117"} {:id "defn-/seq-handler", :kind "defn-", :line 197, :end-line 206, :hash "176826092"} {:id "defn-/loop-handler", :kind "defn-", :line 208, :end-line 215, :hash "282383021"} {:id "defn-/handlers", :kind "defn-", :line 217, :end-line 399, :hash "1362802050"} {:id "defn-/fn-ctx", :kind "defn-", :line 401, :end-line 419, :hash "535391672"} {:id "defn-/check-fn-decl", :kind "defn-", :line 421, :end-line 425, :hash "1515794091"} {:id "defn-/check-host", :kind "defn-", :line 427, :end-line 431, :hash "659982747"} {:id "defn/check-module", :kind "defn", :line 433, :end-line 441, :hash "-556436869"}]}
;; clj-mutate-manifest-end
