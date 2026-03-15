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
(declare write-jar!)
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

(defn build-project-source-jar!
  [module-sources root-module-name output-jar]
  (-> (compile-project-source module-sources root-module-name)
      (write-jar! output-jar (-> root-module-name str (.replace \/ \.)))))

(defn build-project-dir!
  [project-dir root-module-name output-dir]
  (-> (compile-project-dir project-dir root-module-name)
      (write-bundle! output-dir)))

(defn build-project-dir-jar!
  [project-dir root-module-name output-jar]
  (-> (compile-project-dir project-dir root-module-name)
      (write-jar! output-jar (-> root-module-name str (.replace \/ \.)))))

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
;; {:version 1, :tested-at "2026-03-15T10:49:45.573693-05:00", :module-hash "168189159", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 19, :hash "1817783771"} {:id "form/1/declare", :kind "declare", :line 21, :end-line 21, :hash "2058848851"} {:id "form/2/declare", :kind "declare", :line 22, :end-line 22, :hash "596567384"} {:id "form/3/declare", :kind "declare", :line 23, :end-line 23, :hash "1168981682"} {:id "def/contract-result-name", :kind "def", :line 25, :end-line 25, :hash "-105850544"} {:id "def/contract-unit-name", :kind "def", :line 26, :end-line 26, :hash "347759796"} {:id "def/invariant-self-name", :kind "def", :line 27, :end-line 27, :hash "-1593038412"} {:id "defn-/contract-failure-expr", :kind "defn-", :line 29, :end-line 35, :hash "101978921"} {:id "defn-/contract-check-expr", :kind "defn-", :line 37, :end-line 42, :hash "995434044"} {:id "defn-/with-contract-checks", :kind "defn-", :line 44, :end-line 50, :hash "-2133473937"} {:id "defn-/with-postcondition-checks", :kind "defn-", :line 52, :end-line 78, :hash "-190297322"} {:id "defn-/local-expr", :kind "defn-", :line 80, :end-line 83, :hash "1731534324"} {:id "defn-/instrument-data-invariants", :kind "defn-", :line 85, :end-line 108, :hash "570384672"} {:id "defn-/instrument-union-invariants", :kind "defn-", :line 110, :end-line 130, :hash "1766008045"} {:id "form/14/declare", :kind "declare", :line 132, :end-line 132, :hash "-1070001430"} {:id "defn-/instrument-exprs", :kind "defn-", :line 134, :end-line 136, :hash "-2018037737"} {:id "defn-/instrument-bindings", :kind "defn-", :line 138, :end-line 142, :hash "850191118"} {:id "defn-/instrument-catches", :kind "defn-", :line 144, :end-line 148, :hash "-786001501"} {:id "defn-/instrument-match-cases", :kind "defn-", :line 150, :end-line 154, :hash "-1115947730"} {:id "defn-/instrument-try-expr", :kind "defn-", :line 156, :end-line 161, :hash "1730985181"} {:id "defn-/instrument-java-call-expr", :kind "defn-", :line 163, :end-line 167, :hash "1131125098"} {:id "defn-/instrument-java-set-field-expr", :kind "defn-", :line 169, :end-line 173, :hash "1627552813"} {:id "defn-/instrument-call-expr", :kind "defn-", :line 175, :end-line 179, :hash "-796600826"} {:id "defn-/instrument-construct-expr", :kind "defn-", :line 181, :end-line 183, :hash "-1298094645"} {:id "defn-/instrument-record-get-expr", :kind "defn-", :line 185, :end-line 187, :hash "1341046921"} {:id "defn-/instrument-if-expr", :kind "defn-", :line 189, :end-line 194, :hash "-1581702021"} {:id "defn-/instrument-match-expr", :kind "defn-", :line 196, :end-line 200, :hash "384899293"} {:id "defn-/instrument-let-expr", :kind "defn-", :line 202, :end-line 206, :hash "-1926311263"} {:id "defn-/instrument-lambda-expr", :kind "defn-", :line 208, :end-line 210, :hash "-1477337255"} {:id "defn-/instrument-loop-expr", :kind "defn-", :line 212, :end-line 216, :hash "494953759"} {:id "def/expr-instrumenters", :kind "def", :line 218, :end-line 289, :hash "-694959846"} {:id "defn-/recursively-instrument-expr", :kind "defn-", :line 291, :end-line 295, :hash "1761646999"} {:id "defn-/instrument-value-invariants", :kind "defn-", :line 297, :end-line 304, :hash "-803348989"} {:id "defn-/instrument-expr", :kind "defn-", :line 306, :end-line 312, :hash "-1857520944"} {:id "defn-/instrument-contracts", :kind "defn-", :line 314, :end-line 330, :hash "939158315"} {:id "defn/compile-module", :kind "defn", :line 332, :end-line 345, :hash "-303758079"} {:id "defn/checked-module", :kind "defn", :line 347, :end-line 358, :hash "-1523096844"} {:id "defn-/compile-source-map", :kind "defn-", :line 360, :end-line 372, :hash "-1906915902"} {:id "defn/compile-source", :kind "defn", :line 374, :end-line 385, :hash "1284182197"} {:id "defn/compile-project-source", :kind "defn", :line 387, :end-line 390, :hash "-1709957155"} {:id "defn/checked-project-module", :kind "defn", :line 392, :end-line 395, :hash "1201666930"} {:id "defn/compile-project-dir", :kind "defn", :line 397, :end-line 400, :hash "-932599350"} {:id "defn/checked-project-dir-module", :kind "defn", :line 402, :end-line 405, :hash "213051726"} {:id "defn/build-project-source!", :kind "defn", :line 407, :end-line 410, :hash "434622430"} {:id "defn/build-project-source-jar!", :kind "defn", :line 412, :end-line 415, :hash "-2026246245"} {:id "defn/build-project-dir!", :kind "defn", :line 417, :end-line 420, :hash "919358575"} {:id "defn/build-project-dir-jar!", :kind "defn", :line 422, :end-line 425, :hash "1827509347"} {:id "defn/interface-source", :kind "defn", :line 427, :end-line 432, :hash "-277567554"} {:id "defn-/class-file", :kind "defn-", :line 434, :end-line 436, :hash "1900463827"} {:id "defn-/skip-jar-entry?", :kind "defn-", :line 438, :end-line 441, :hash "860569880"} {:id "defn-/add-entry!", :kind "defn-", :line 443, :end-line 450, :hash "1853967105"} {:id "defn-/add-file-entry!", :kind "defn-", :line 452, :end-line 459, :hash "-1663370589"} {:id "defn-/add-directory!", :kind "defn-", :line 461, :end-line 466, :hash "-1552530834"} {:id "defn-/add-jar-file!", :kind "defn-", :line 468, :end-line 478, :hash "-2064398966"} {:id "defn-/classpath-entries", :kind "defn-", :line 480, :end-line 483, :hash "-7708379"} {:id "defn-/add-classpath-entry!", :kind "defn-", :line 485, :end-line 491, :hash "-1404275351"} {:id "defn-/executable-manifest", :kind "defn-", :line 493, :end-line 499, :hash "-743405217"} {:id "defn-/write-jar!", :kind "defn-", :line 501, :end-line 511, :hash "-307603517"} {:id "defn/write-bundle!", :kind "defn", :line 513, :end-line 523, :hash "-785230489"} {:id "defn/build-source!", :kind "defn", :line 525, :end-line 531, :hash "358574914"} {:id "defn/build-source-jar!", :kind "defn", :line 533, :end-line 540, :hash "636703518"} {:id "defn-/load-class-from-dir", :kind "defn-", :line 542, :end-line 546, :hash "648982521"} {:id "defn-/class-name", :kind "defn-", :line 548, :end-line 550, :hash "-314160674"} {:id "defn-/airj-main-method", :kind "defn-", :line 552, :end-line 561, :hash "-130380503"} {:id "defn/run-source!", :kind "defn", :line 563, :end-line 580, :hash "-303308050"} {:id "defn/run-project-source!", :kind "defn", :line 582, :end-line 597, :hash "484953388"} {:id "defn/run-project-dir!", :kind "defn", :line 599, :end-line 614, :hash "-487228803"}]}
;; clj-mutate-manifest-end
