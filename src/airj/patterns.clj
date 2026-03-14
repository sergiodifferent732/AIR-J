(ns airj.patterns)

(declare bind-pattern)

(defn declared-type-name
  [type-expr]
  (if (seq? type-expr)
    (first type-expr)
    type-expr))

(defn type-bindings
  [decl type-expr]
  (zipmap (:type-params decl)
          (if (seq? type-expr)
            (rest type-expr)
            [])))

(defn instantiate-type
  [type-expr bindings]
  (cond
    (symbol? type-expr) (get bindings type-expr type-expr)
    (seq? type-expr) (apply list (map #(instantiate-type % bindings) type-expr))
    :else type-expr))

(defn decl-for-type
  [decls type-expr]
  (get decls (declared-type-name type-expr)))

(defn field-type
  ([decl field-name]
   (field-type decl field-name (:name decl)))
  ([decl field-name type-expr]
   (some (fn [field]
           (when (= field-name (:name field))
             (instantiate-type (:type field)
                               (type-bindings decl type-expr))))
         (:fields decl))))

(defn union-variant
  ([decl variant-name]
   (union-variant decl variant-name (:name decl)))
  ([decl variant-name type-expr]
   (some (fn [variant]
           (when (= (:name variant) variant-name)
             (update variant
                     :fields
                     (fn [fields]
                       (mapv (fn [field]
                               (update field
                                       :type
                                       instantiate-type
                                       (type-bindings decl type-expr)))
                             fields)))))
         (:variants decl))))

(defn enum-variant?
  [decl variant-name]
  (boolean (some #{variant-name} (:variants decl))))

(defn enum-pattern?
  [pattern target-type decls]
  (let [decl (decl-for-type decls target-type)]
    (and (= :binder-pattern (:op pattern))
         (= :enum (:op decl))
         (enum-variant? decl (:name pattern)))))

(defn- options
  [bind-binder bind-literal fail!]
  {:bind-binder bind-binder
   :bind-literal bind-literal
   :fail! fail!})

(defn- bind-union-pattern
  [ctx pattern target-type decls opts]
  (let [decl (decl-for-type decls target-type)]
    (when-not (= :union (:op decl))
      ((:fail! opts) "Expected union type."
                    {:pattern pattern
                     :type target-type}))
    (let [variant (union-variant decl (:name pattern) target-type)
          expected-count (count (:fields variant))
          actual-count (count (:args pattern))]
      (when-not variant
        ((:fail! opts) "Unknown variant."
                      {:pattern pattern
                       :type target-type}))
      (when-not (= expected-count actual-count)
        ((:fail! opts) "Arity mismatch."
                      {:pattern pattern
                       :type target-type
                       :expected expected-count
                       :actual actual-count}))
              (reduce (fn [acc [field nested-pattern]]
                (bind-pattern acc
                              nested-pattern
                              (:type field)
                              decls
                              opts))
              ctx
              (map vector (:fields variant) (:args pattern))))))

(defn- bind-record-field
  [ctx record-decl field-pattern decls opts target-type]
  (let [matched-type (field-type record-decl (:name field-pattern))]
    (when-not matched-type
      ((:fail! opts) "Unknown field."
                    {:type target-type
                     :field (:name field-pattern)}))
    (bind-pattern ctx
                  (:pattern field-pattern)
                  matched-type
                  decls
                  opts)))

(defn- bind-record-pattern
  [ctx pattern target-type decls opts]
  (when-not (= (:type pattern) target-type)
    ((:fail! opts) "Type mismatch."
                  {:pattern pattern
                   :expected target-type
                   :actual (:type pattern)}))
  (let [record-decl (decl-for-type decls target-type)]
    (when-not (= :data (:op record-decl))
      ((:fail! opts) "Expected record type."
                    {:pattern pattern
                     :type target-type}))
    (reduce (fn [acc field-pattern]
              (bind-record-field acc record-decl field-pattern decls opts target-type))
            ctx
            (:fields pattern))))

(defn bind-pattern
  [ctx pattern target-type decls {:keys [bind-binder bind-literal fail!]}]
  (let [opts (options bind-binder bind-literal fail!)]
    (case (:op pattern)
      :wildcard-pattern ctx
      :literal-pattern (bind-literal ctx pattern target-type)
      :binder-pattern (bind-binder ctx pattern target-type decls)
      :union-pattern (bind-union-pattern ctx pattern target-type decls opts)
      :record-pattern (bind-record-pattern ctx pattern target-type decls opts)
      (fail! "Unsupported pattern."
             {:pattern pattern}))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T21:54:01.139081-05:00", :module-hash "-137036240", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "453762726"} {:id "form/1/declare", :kind "declare", :line 3, :end-line 3, :hash "569079974"} {:id "defn/declared-type-name", :kind "defn", :line 5, :end-line 9, :hash "-1494019016"} {:id "defn/type-bindings", :kind "defn", :line 11, :end-line 16, :hash "-1876478264"} {:id "defn/instantiate-type", :kind "defn", :line 18, :end-line 23, :hash "-32358289"} {:id "defn/decl-for-type", :kind "defn", :line 25, :end-line 27, :hash "-1626223648"} {:id "defn/field-type", :kind "defn", :line 29, :end-line 37, :hash "866046255"} {:id "defn/union-variant", :kind "defn", :line 39, :end-line 54, :hash "-807745172"} {:id "defn/enum-variant?", :kind "defn", :line 56, :end-line 58, :hash "366372957"} {:id "defn/enum-pattern?", :kind "defn", :line 60, :end-line 65, :hash "-1257824303"} {:id "defn-/options", :kind "defn-", :line 67, :end-line 71, :hash "971725910"} {:id "defn-/bind-union-pattern", :kind "defn-", :line 73, :end-line 100, :hash "445584970"} {:id "defn-/bind-record-field", :kind "defn-", :line 102, :end-line 113, :hash "-632071101"} {:id "defn-/bind-record-pattern", :kind "defn-", :line 115, :end-line 130, :hash "431235345"} {:id "defn/bind-pattern", :kind "defn", :line 132, :end-line 142, :hash "-840191915"}]}
;; clj-mutate-manifest-end
