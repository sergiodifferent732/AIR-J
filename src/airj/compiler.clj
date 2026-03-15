(ns airj.compiler
  (:require [airj.effect-checker :as effect-checker]
            [airj.exhaustiveness-checker :as exhaustiveness-checker]
            [airj.java-resolver :as java-resolver]
            [airj.linker :as linker]
            [airj.module-interface :as module-interface]
            [airj.project-files :as project-files]
            [airj.project-sources :as project-sources]
            [airj.jvm-emitter :as jvm-emitter]
            [airj.jvm-lowerer :as jvm-lowerer]
            [airj.normalizer :as normalizer]
            [airj.parser :as parser]
            [airj.resolver :as resolver]
            [airj.stdlib :as stdlib]
            [airj.type-checker :as type-checker]
            [clojure.java.io :as io]
            [clojure.string :as str])
  (:import (java.net URL URLClassLoader)
           (java.util.jar Attributes$Name JarEntry JarFile JarOutputStream Manifest)))

(declare write-bundle!)
(declare class-name)

(def ^:private contract-result-name '__airj_result)
(def ^:private contract-unit-name '__airj_contract_unit)
(def ^:private invariant-self-name 'self)

(defn- contract-failure-expr
  [kind fn-name]
  {:op :raise
   :expr {:op :java-new
          :class-name 'java.lang.IllegalStateException
          :type-args []
          :args [(str kind " failed: " fn-name)]}})

(defn- contract-check-expr
  [contract kind fn-name]
  {:op :if
   :test contract
   :then true
   :else (contract-failure-expr kind fn-name)})

(defn- with-contract-checks
  [expr contracts kind fn-name]
  (if (seq contracts)
    {:op :seq
     :exprs (vec (concat (map #(contract-check-expr % kind fn-name) contracts)
                         [expr]))}
    expr))

(defn- with-postcondition-checks
  [decl]
  (let [body (:body decl)
        fn-name (:name decl)
        post-checks (map #(contract-check-expr % "Postcondition" fn-name)
                         (:ensures decl))]
    (cond
      (empty? post-checks)
      body

      (= 'Unit (:return-type decl))
      {:op :seq
       :exprs (vec (concat [body]
                           post-checks
                           [{:op :var
                             :name contract-unit-name
                             :type 'Int
                             :init 0}]))}

      :else
      {:op :let
       :bindings [{:name contract-result-name
                   :expr body}]
       :body {:op :seq
              :exprs (vec (concat post-checks
                                  [{:op :local
                                    :name contract-result-name}]))}})))

(defn- local-expr
  [name]
  {:op :local
   :name name})

(defn- instrument-data-invariants
  [expr decl]
  (if (seq (:invariants decl))
    (let [temp-bindings (mapv (fn [field arg]
                                {:name (gensym "__airj_field_")
                                 :expr arg})
                              (:fields decl)
                              (:args expr))
          field-bindings (mapv (fn [field temp-binding]
                                 {:name (:name field)
                                  :expr (local-expr (:name temp-binding))})
                               (:fields decl)
                               temp-bindings)
          rebound-expr (assoc expr
                              :args (mapv (fn [field]
                                            (local-expr (:name field)))
                                          (:fields decl)))]
      {:op :let
       :bindings (vec (concat temp-bindings field-bindings))
       :body (with-contract-checks rebound-expr
                                   (:invariants decl)
                                   "Invariant"
                                   (:name decl))})
    expr))

(defn- instrument-union-invariants
  [expr decl]
  (if (seq (:invariants decl))
    (let [temp-bindings (mapv (fn [_ arg]
                                {:name (gensym "__airj_variant_arg_")
                                 :expr arg})
                              (:args expr)
                              (:args expr))
          rebound-expr (assoc expr
                              :args (mapv (fn [binding]
                                            (local-expr (:name binding)))
                                          temp-bindings))]
      {:op :let
       :bindings (vec (concat temp-bindings
                              [{:name invariant-self-name
                                :expr rebound-expr}]))
       :body (with-contract-checks (local-expr invariant-self-name)
                                   (:invariants decl)
                                   "Invariant"
                                   (:name decl))})
    expr))

(declare instrument-expr)

(defn- instrument-exprs
  [exprs decls]
  (mapv #(instrument-expr % decls) exprs))

(defn- instrument-bindings
  [bindings decls]
  (mapv (fn [binding]
          (update binding :expr instrument-expr decls))
        bindings))

(defn- instrument-catches
  [catches decls]
  (mapv (fn [catch]
          (update catch :body instrument-expr decls))
        catches))

(defn- instrument-match-cases
  [cases decls]
  (mapv (fn [case]
          (update case :body instrument-expr decls))
        cases))

(defn- instrument-try-expr
  [expr decls]
  (-> expr
      (update :body instrument-expr decls)
      (update :catches instrument-catches decls)
      (update :finally #(when % (instrument-expr % decls)))))

(defn- instrument-java-call-expr
  [expr decls]
  (-> expr
      (update :target instrument-expr decls)
      (update :args instrument-exprs decls)))

(defn- instrument-java-set-field-expr
  [expr decls]
  (-> expr
      (update :target instrument-expr decls)
      (update :expr instrument-expr decls)))

(defn- instrument-call-expr
  [expr decls]
  (-> expr
      (update :callee instrument-expr decls)
      (update :args instrument-exprs decls)))

(defn- instrument-construct-expr
  [expr decls]
  (update expr :args instrument-exprs decls))

(defn- instrument-record-get-expr
  [expr decls]
  (update expr :target instrument-expr decls))

(defn- instrument-if-expr
  [expr decls]
  (-> expr
      (update :test instrument-expr decls)
      (update :then instrument-expr decls)
      (update :else instrument-expr decls)))

(defn- instrument-match-expr
  [expr decls]
  (-> expr
      (update :target instrument-expr decls)
      (update :cases instrument-match-cases decls)))

(defn- instrument-let-expr
  [expr decls]
  (-> expr
      (update :bindings instrument-bindings decls)
      (update :body instrument-expr decls)))

(defn- instrument-lambda-expr
  [expr decls]
  (update expr :body instrument-expr decls))

(defn- instrument-loop-expr
  [expr decls]
  (-> expr
      (update :bindings instrument-bindings decls)
      (update :body instrument-expr decls)))

(def ^:private expr-instrumenters
  {:call instrument-call-expr
   :int-add (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-sub (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-mul (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-div (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-mod (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-eq (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-lt (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-le (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-gt (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-ge (fn [expr decls] (update expr :args instrument-exprs decls))
   :bool-eq (fn [expr decls] (update expr :args instrument-exprs decls))
   :int-ne (fn [expr decls] (update expr :args instrument-exprs decls))
   :string-eq (fn [expr decls] (update expr :args instrument-exprs decls))
   :string-concat (fn [expr decls] (update expr :args instrument-exprs decls))
   :string-split-on (fn [expr decls] (update expr :args instrument-exprs decls))
   :string-char-at (fn [expr decls] (update expr :args instrument-exprs decls))
   :string-substring (fn [expr decls] (update expr :args instrument-exprs decls))
   :int->string (fn [expr decls] (update expr :arg instrument-expr decls))
   :env-get (fn [expr decls] (update expr :arg instrument-expr decls))
   :process-run (fn [expr decls] (update expr :args instrument-exprs decls))
   :string->int (fn [expr decls] (update expr :arg instrument-expr decls))
   :string-length (fn [expr decls] (update expr :arg instrument-expr decls))
   :string-trim (fn [expr decls] (update expr :arg instrument-expr decls))
   :string-empty? (fn [expr decls] (update expr :arg instrument-expr decls))
   :seq-empty? (fn [expr decls] (update expr :arg instrument-expr decls))
   :seq-length (fn [expr decls] (update expr :arg instrument-expr decls))
   :seq-first (fn [expr decls] (update expr :arg instrument-expr decls))
   :seq-rest (fn [expr decls] (update expr :arg instrument-expr decls))
   :seq-concat (fn [expr decls] (update expr :args instrument-exprs decls))
   :seq-get (fn [expr decls] (update expr :args instrument-exprs decls))
   :map-empty (fn [expr _decls] expr)
   :map-set (fn [expr decls] (update expr :args instrument-exprs decls))
   :map-get (fn [expr decls] (update expr :args instrument-exprs decls))
   :map-contains? (fn [expr decls] (update expr :args instrument-exprs decls))
   :map-keys (fn [expr decls] (update expr :arg instrument-expr decls))
   :io-read-line (fn [expr _decls] expr)
   :io-print (fn [expr decls] (update expr :arg instrument-expr decls))
   :bool-not (fn [expr decls] (update expr :arg instrument-expr decls))
   :bool-and (fn [expr decls] (update expr :args instrument-exprs decls))
   :bool-or (fn [expr decls] (update expr :args instrument-exprs decls))
   :io-println (fn [expr decls] (update expr :arg instrument-expr decls))
   :construct instrument-construct-expr
   :variant instrument-construct-expr
   :record-get instrument-record-get-expr
   :if instrument-if-expr
   :match instrument-match-expr
   :let instrument-let-expr
   :seq (fn [expr decls]
          (update expr :exprs instrument-exprs decls))
   :lambda instrument-lambda-expr
   :try instrument-try-expr
   :var (fn [expr decls]
          (update expr :init instrument-expr decls))
   :set (fn [expr decls]
          (update expr :expr instrument-expr decls))
   :loop instrument-loop-expr
   :recur (fn [expr decls]
            (update expr :args instrument-exprs decls))
   :raise (fn [expr decls]
            (update expr :expr instrument-expr decls))
   :java-new (fn [expr decls]
               (update expr :args instrument-exprs decls))
   :java-call instrument-java-call-expr
   :java-static-call (fn [expr decls]
                       (update expr :args instrument-exprs decls))
   :java-get-field instrument-record-get-expr
   :java-set-field instrument-java-set-field-expr
   :java-static-get-field (fn [expr _decls] expr)
   :java-static-set-field (fn [expr decls]
                            (update expr :expr instrument-expr decls))})

(defn- recursively-instrument-expr
  [expr decls]
  (if-let [instrumenter (get expr-instrumenters (:op expr))]
    (instrumenter expr decls)
    expr))

(defn- instrument-value-invariants
  [expr decls]
  (if-let [decl (get decls (:type expr))]
    (case [(:op expr) (:op decl)]
      [:construct :data] (instrument-data-invariants expr decl)
      [:variant :union] (instrument-union-invariants expr decl)
      expr)
    expr))

(defn- instrument-expr
  [expr decls]
  (if-not (map? expr)
    expr
    (-> expr
        (recursively-instrument-expr decls)
        (instrument-value-invariants decls))))

(defn- instrument-contracts
  [module]
  (let [decls-by-name (into {} (map (juxt :name identity) (:decls module)))]
    (update module
            :decls
            (fn [decls]
              (mapv (fn [decl]
                      (if (= :fn (:op decl))
                        (assoc decl
                               :body (-> decl
                                         with-postcondition-checks
                                         (with-contract-checks (:requires decl)
                                                               "Precondition"
                                                               (:name decl))
                                         (instrument-expr decls-by-name)))
                        decl))
                    decls)))))

(defn compile-module
  ([module]
   (compile-module module {}))
  ([module options]
   (let [checked (-> module
                     normalizer/normalize-module
                     (linker/link-module options)
                     resolver/resolve-module
                     effect-checker/check-module
                     type-checker/check-module
                     java-resolver/check-module
                     exhaustiveness-checker/check-module)
        lowered (jvm-lowerer/lower-module (instrument-contracts checked))]
     (jvm-emitter/emit-class-bytes lowered))))

(defn checked-module
  ([module]
   (checked-module module {}))
  ([module options]
   (-> module
       normalizer/normalize-module
       (linker/link-module options)
       resolver/resolve-module
       effect-checker/check-module
       type-checker/check-module
       java-resolver/check-module
       exhaustiveness-checker/check-module)))

(defn- compile-source-map
  ([sources]
   (compile-source-map sources {}))
  ([sources base-options]
   (reduce (fn [bundle [module-name source]]
             (let [module (parser/parse-module source)]
               (merge bundle
                      (compile-module module
                                      (merge base-options
                                             (project-sources/compiler-options sources module-name)
                                             {:available-modules (set (keys sources))})))))
           {}
           sources)))

(defn compile-source
  ([source]
   (compile-source source {}))
  ([source options]
   (let [module (parser/parse-module source)
         sources (merge (stdlib/reachable-source-map module)
                        {(:name module) source})]
     (if (seq (stdlib/reachable-source-map module))
       (compile-source-map sources options)
       (compile-module module
                       (merge options
                              {:available-modules #{(:name module)}}))))))

(defn compile-project-source
  [module-sources root-module-name]
  (let [sources (project-sources/compilation-source-map module-sources root-module-name)]
    (compile-source-map sources)))

(defn checked-project-module
  [module-sources root-module-name]
  (checked-module (parser/parse-module (project-sources/root-source module-sources root-module-name))
                  (project-sources/compiler-options module-sources root-module-name)))

(defn compile-project-dir
  [project-dir root-module-name]
  (compile-project-source (project-files/reachable-source-map project-dir root-module-name)
                          root-module-name))

(defn checked-project-dir-module
  [project-dir root-module-name]
  (checked-module (parser/parse-module (project-files/root-source project-dir root-module-name))
                  (project-files/compiler-options project-dir root-module-name)))

(defn build-project-source!
  [module-sources root-module-name output-dir]
  (-> (compile-project-source module-sources root-module-name)
      (write-bundle! output-dir)))

(defn build-project-dir!
  [project-dir root-module-name output-dir]
  (-> (compile-project-dir project-dir root-module-name)
      (write-bundle! output-dir)))

(defn interface-source
  [source]
  (-> source
      parser/parse-module
      normalizer/normalize-module
      module-interface/extract-interface))

(defn- class-file
  [output-dir internal-name]
  (io/file output-dir (str internal-name ".class")))

(defn- skip-jar-entry?
  [entry-name]
  (or (= "META-INF/MANIFEST.MF" entry-name)
      (re-find #"^META-INF/.*\.(SF|RSA|DSA)$" entry-name)))

(defn- add-entry!
  [^JarOutputStream jar-stream seen entry-name bytes]
  (when-not (or (contains? @seen entry-name)
                (skip-jar-entry? entry-name))
    (swap! seen conj entry-name)
    (.putNextEntry jar-stream (JarEntry. entry-name))
    (.write jar-stream ^bytes bytes)
    (.closeEntry jar-stream)))

(defn- add-file-entry!
  [^JarOutputStream jar-stream seen root-dir file]
  (let [root-path (.toPath (io/file root-dir))
        file-path (.toPath (io/file file))
        entry-name (-> (.relativize root-path file-path)
                       str
                       (str/replace "\\" "/"))]
    (add-entry! jar-stream seen entry-name (java.nio.file.Files/readAllBytes file-path))))

(defn- add-directory!
  [^JarOutputStream jar-stream seen dir]
  (when (.exists (io/file dir))
    (doseq [file (rest (file-seq (io/file dir)))
            :when (.isFile (io/file file))]
      (add-file-entry! jar-stream seen dir file))))

(defn- add-jar-file!
  [^JarOutputStream jar-stream seen jar-path]
  (with-open [jar-file (JarFile. (io/file jar-path))]
    (doseq [entry (enumeration-seq (.entries jar-file))
            :when (not (.isDirectory entry))]
      (let [entry-name (.getName entry)]
        (when-not (skip-jar-entry? entry-name)
          (with-open [in (.getInputStream jar-file entry)
                      out (java.io.ByteArrayOutputStream.)]
            (io/copy in out)
            (add-entry! jar-stream seen entry-name (.toByteArray out))))))))

(defn- classpath-entries
  []
  (vec (str/split (System/getProperty "java.class.path" "")
                  (re-pattern java.io.File/pathSeparator))))

(defn- add-classpath-entry!
  [^JarOutputStream jar-stream seen entry]
  (let [file (io/file entry)]
    (when (.exists file)
      (if (.isDirectory file)
        (add-directory! jar-stream seen file)
        (add-jar-file! jar-stream seen file)))))

(defn- executable-manifest
  [main-class]
  (doto (Manifest.)
    (-> .getMainAttributes
        (.put Attributes$Name/MANIFEST_VERSION "1.0"))
    (-> .getMainAttributes
        (.put Attributes$Name/MAIN_CLASS main-class))))

(defn- write-jar!
  [bundle output-jar main-class]
  (let [seen (atom #{})]
    (io/make-parents output-jar)
    (with-open [jar-stream (JarOutputStream. (io/output-stream output-jar)
                                             (executable-manifest main-class))]
      (doseq [[internal-name bytecode] bundle]
        (add-entry! jar-stream seen (str internal-name ".class") bytecode))
      (doseq [entry (classpath-entries)]
        (add-classpath-entry! jar-stream seen entry)))
    {:jar output-jar}))

(defn write-bundle!
  [bundle output-dir]
  (doseq [[internal-name bytecode] bundle]
    (let [file (class-file output-dir internal-name)]
      (io/make-parents file)
      (with-open [out (io/output-stream file)]
        (.write out bytecode))))
  (into {}
        (map (fn [[internal-name _]]
               [internal-name (.getPath (class-file output-dir internal-name))]))
        bundle))

(defn build-source!
  ([source output-dir]
   (build-source! source output-dir {}))
  ([source output-dir options]
   (-> source
       (compile-source options)
       (write-bundle! output-dir))))

(defn build-source-jar!
  ([source output-jar]
   (build-source-jar! source output-jar {}))
  ([source output-jar options]
   (let [module (parser/parse-module source)]
     (-> source
         (compile-source options)
         (write-jar! output-jar (class-name module))))))

(defn- load-class-from-dir
  [output-dir class-name]
  (let [url (.toURL (.toURI (io/file output-dir)))
        loader (URLClassLoader. (into-array URL [url]))]
    (.loadClass loader class-name)))

(defn- class-name
  [module]
  (-> (:name module) str (.replace \/ \.)))

(defn- airj-main-method
  [klass]
  (or (try
        (.getMethod klass "airj_main" (into-array Class [(class (into-array String []))]))
        (catch NoSuchMethodException _ nil))
      (try
        (.getMethod klass "airj_main" (into-array Class []))
        (catch NoSuchMethodException _ nil))
      (throw (ex-info "Missing AIR-J main entrypoint."
                      {:class (.getName klass)}))))

(defn run-source!
  ([source]
   (run-source! source [] {}))
  ([source args]
   (run-source! source args {}))
  ([source args options]
   (let [module (parser/parse-module source)
         output-dir (.toString (java.nio.file.Files/createTempDirectory
                                "airj-run"
                                (make-array java.nio.file.attribute.FileAttribute 0)))
         _ (-> source
               (compile-source options)
               (write-bundle! output-dir))
         klass (load-class-from-dir output-dir (class-name module))
         method (airj-main-method klass)]
     (if (= 1 (alength (.getParameterTypes method)))
       (.invoke method nil (object-array [(into-array String args)]))
       (.invoke method nil (object-array []))))))

(defn run-project-source!
  ([module-sources root-module-name]
   (run-project-source! module-sources root-module-name []))
  ([module-sources root-module-name args]
   (let [output-dir (.toString (java.nio.file.Files/createTempDirectory
                                "airj-run-project"
                                (make-array java.nio.file.attribute.FileAttribute 0)))
         root-source (project-sources/root-source module-sources root-module-name)
         module (parser/parse-module root-source)
         _ (-> (compile-project-source module-sources root-module-name)
               (write-bundle! output-dir))
         klass (load-class-from-dir output-dir (class-name module))
         method (airj-main-method klass)]
     (if (= 1 (alength (.getParameterTypes method)))
       (.invoke method nil (object-array [(into-array String args)]))
       (.invoke method nil (object-array []))))))

(defn run-project-dir!
  ([project-dir root-module-name]
   (run-project-dir! project-dir root-module-name []))
  ([project-dir root-module-name args]
   (let [output-dir (.toString (java.nio.file.Files/createTempDirectory
                                "airj-run-project-dir"
                                (make-array java.nio.file.attribute.FileAttribute 0)))
         root-source (project-files/root-source project-dir root-module-name)
         module (parser/parse-module root-source)
         _ (-> (compile-project-dir project-dir root-module-name)
               (write-bundle! output-dir))
         klass (load-class-from-dir output-dir (class-name module))
         method (airj-main-method klass)]
     (if (= 1 (alength (.getParameterTypes method)))
       (.invoke method nil (object-array [(into-array String args)]))
       (.invoke method nil (object-array []))))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-15T10:25:55.643284-05:00", :module-hash "-1231248289", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 19, :hash "1817783771"} {:id "form/1/declare", :kind "declare", :line 21, :end-line 21, :hash "2058848851"} {:id "form/2/declare", :kind "declare", :line 22, :end-line 22, :hash "1168981682"} {:id "def/contract-result-name", :kind "def", :line 24, :end-line 24, :hash "-105850544"} {:id "def/contract-unit-name", :kind "def", :line 25, :end-line 25, :hash "347759796"} {:id "def/invariant-self-name", :kind "def", :line 26, :end-line 26, :hash "-1593038412"} {:id "defn-/contract-failure-expr", :kind "defn-", :line 28, :end-line 34, :hash "101978921"} {:id "defn-/contract-check-expr", :kind "defn-", :line 36, :end-line 41, :hash "995434044"} {:id "defn-/with-contract-checks", :kind "defn-", :line 43, :end-line 49, :hash "-2133473937"} {:id "defn-/with-postcondition-checks", :kind "defn-", :line 51, :end-line 77, :hash "-190297322"} {:id "defn-/local-expr", :kind "defn-", :line 79, :end-line 82, :hash "1731534324"} {:id "defn-/instrument-data-invariants", :kind "defn-", :line 84, :end-line 107, :hash "570384672"} {:id "defn-/instrument-union-invariants", :kind "defn-", :line 109, :end-line 129, :hash "1766008045"} {:id "form/13/declare", :kind "declare", :line 131, :end-line 131, :hash "-1070001430"} {:id "defn-/instrument-exprs", :kind "defn-", :line 133, :end-line 135, :hash "-2018037737"} {:id "defn-/instrument-bindings", :kind "defn-", :line 137, :end-line 141, :hash "850191118"} {:id "defn-/instrument-catches", :kind "defn-", :line 143, :end-line 147, :hash "-786001501"} {:id "defn-/instrument-match-cases", :kind "defn-", :line 149, :end-line 153, :hash "-1115947730"} {:id "defn-/instrument-try-expr", :kind "defn-", :line 155, :end-line 160, :hash "1730985181"} {:id "defn-/instrument-java-call-expr", :kind "defn-", :line 162, :end-line 166, :hash "1131125098"} {:id "defn-/instrument-java-set-field-expr", :kind "defn-", :line 168, :end-line 172, :hash "1627552813"} {:id "defn-/instrument-call-expr", :kind "defn-", :line 174, :end-line 178, :hash "-796600826"} {:id "defn-/instrument-construct-expr", :kind "defn-", :line 180, :end-line 182, :hash "-1298094645"} {:id "defn-/instrument-record-get-expr", :kind "defn-", :line 184, :end-line 186, :hash "1341046921"} {:id "defn-/instrument-if-expr", :kind "defn-", :line 188, :end-line 193, :hash "-1581702021"} {:id "defn-/instrument-match-expr", :kind "defn-", :line 195, :end-line 199, :hash "384899293"} {:id "defn-/instrument-let-expr", :kind "defn-", :line 201, :end-line 205, :hash "-1926311263"} {:id "defn-/instrument-lambda-expr", :kind "defn-", :line 207, :end-line 209, :hash "-1477337255"} {:id "defn-/instrument-loop-expr", :kind "defn-", :line 211, :end-line 215, :hash "494953759"} {:id "def/expr-instrumenters", :kind "def", :line 217, :end-line 288, :hash "-694959846"} {:id "defn-/recursively-instrument-expr", :kind "defn-", :line 290, :end-line 294, :hash "1761646999"} {:id "defn-/instrument-value-invariants", :kind "defn-", :line 296, :end-line 303, :hash "-803348989"} {:id "defn-/instrument-expr", :kind "defn-", :line 305, :end-line 311, :hash "-1857520944"} {:id "defn-/instrument-contracts", :kind "defn-", :line 313, :end-line 329, :hash "939158315"} {:id "defn/compile-module", :kind "defn", :line 331, :end-line 344, :hash "-303758079"} {:id "defn/checked-module", :kind "defn", :line 346, :end-line 357, :hash "-1523096844"} {:id "defn-/compile-source-map", :kind "defn-", :line 359, :end-line 371, :hash "-1906915902"} {:id "defn/compile-source", :kind "defn", :line 373, :end-line 384, :hash "1284182197"} {:id "defn/compile-project-source", :kind "defn", :line 386, :end-line 389, :hash "-1709957155"} {:id "defn/checked-project-module", :kind "defn", :line 391, :end-line 394, :hash "1201666930"} {:id "defn/compile-project-dir", :kind "defn", :line 396, :end-line 399, :hash "-932599350"} {:id "defn/checked-project-dir-module", :kind "defn", :line 401, :end-line 404, :hash "213051726"} {:id "defn/build-project-source!", :kind "defn", :line 406, :end-line 409, :hash "434622430"} {:id "defn/build-project-dir!", :kind "defn", :line 411, :end-line 414, :hash "919358575"} {:id "defn/interface-source", :kind "defn", :line 416, :end-line 421, :hash "-277567554"} {:id "defn-/class-file", :kind "defn-", :line 423, :end-line 425, :hash "1900463827"} {:id "defn-/skip-jar-entry?", :kind "defn-", :line 427, :end-line 430, :hash "860569880"} {:id "defn-/add-entry!", :kind "defn-", :line 432, :end-line 439, :hash "1853967105"} {:id "defn-/add-file-entry!", :kind "defn-", :line 441, :end-line 448, :hash "-1663370589"} {:id "defn-/add-directory!", :kind "defn-", :line 450, :end-line 455, :hash "-1552530834"} {:id "defn-/add-jar-file!", :kind "defn-", :line 457, :end-line 467, :hash "-2064398966"} {:id "defn-/classpath-entries", :kind "defn-", :line 469, :end-line 472, :hash "-7708379"} {:id "defn-/add-classpath-entry!", :kind "defn-", :line 474, :end-line 480, :hash "-1404275351"} {:id "defn-/executable-manifest", :kind "defn-", :line 482, :end-line 488, :hash "-743405217"} {:id "defn-/write-jar!", :kind "defn-", :line 490, :end-line 500, :hash "-307603517"} {:id "defn/write-bundle!", :kind "defn", :line 502, :end-line 512, :hash "-785230489"} {:id "defn/build-source!", :kind "defn", :line 514, :end-line 520, :hash "358574914"} {:id "defn/build-source-jar!", :kind "defn", :line 522, :end-line 529, :hash "636703518"} {:id "defn-/load-class-from-dir", :kind "defn-", :line 531, :end-line 535, :hash "648982521"} {:id "defn-/class-name", :kind "defn-", :line 537, :end-line 539, :hash "-314160674"} {:id "defn-/airj-main-method", :kind "defn-", :line 541, :end-line 550, :hash "-130380503"} {:id "defn/run-source!", :kind "defn", :line 552, :end-line 569, :hash "-303308050"} {:id "defn/run-project-source!", :kind "defn", :line 571, :end-line 586, :hash "484953388"} {:id "defn/run-project-dir!", :kind "defn", :line 588, :end-line 603, :hash "-487228803"}]}
;; clj-mutate-manifest-end
