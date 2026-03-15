(ns airj.test-runner
  (:require [airj.compiler :as compiler]
            [airj.parser :as parser]
            [airj.project-files :as project-files]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private test-suite-return-type
  '(Seq TestOutcome))

(def ^:private main-param-type
  'StringSeq)

(declare summary)

(defn- temp-output-dir
  []
  (.toString (java.nio.file.Files/createTempDirectory
              "airj-tests"
              (make-array java.nio.file.attribute.FileAttribute 0))))

(defn- class-name
  [module]
  (-> (:name module) str (.replace \/ \.)))

(defn- method-name
  [name]
  (-> name str (.replace "-" "_")))

(defn- load-class-from-dir
  [output-dir class-name]
  (let [url (.toURL (.toURI (io/file output-dir)))
        loader (java.net.URLClassLoader.
                (into-array java.net.URL [url]))]
    (.loadClass loader class-name)))

(defn- exported-test-suite
  [module export-order]
  (let [decls-by-name (into {} (map (juxt :name identity) (:decls module)))]
    (some->> export-order
             (map decls-by-name)
             (filter #(and (= :fn (:op %))
                           (= 'tests (:name %))
                           (empty? (:params %))
                           (= test-suite-return-type (:return-type %))))
             first)))

(defn- exported-main
  [module export-order]
  (let [decls-by-name (into {} (map (juxt :name identity) (:decls module)))]
    (some->> export-order
             (map decls-by-name)
             (filter #(and (= :fn (:op %))
                           (= 'main (:name %))
                           (= 1 (count (:params %)))
                           (= main-param-type (-> % :params first :type))
                           (= 'Int (:return-type %))
                           (some #{'Stdout.Write} (:effects %))))
             first)))

(defn- fail!
  [message data]
  (throw (ex-info message (assoc data :phase :test))))

(defn- ensure-canonical-test-module!
  [module export-order]
  (when-not (exported-test-suite module export-order)
    (fail! "Malformed AIR-J test module."
           {:module (:name module)
            :detail "Expected exported zero-arg tests : () -> (Seq TestOutcome)."}))
  (when-not (exported-main module export-order)
    (fail! "Malformed AIR-J test module."
           {:module (:name module)
            :detail "Expected exported main : (StringSeq) -> Int with Stdout.Write."})))

(defn- field-value
  [object field-name]
  (.get (.getField (.getClass object) field-name) object))

(defn- diagnostic-map
  ([phase message detail]
   {:phase phase
    :message message
    :detail detail})
  ([diagnostic]
   {:phase (str (field-value diagnostic "phase"))
    :message (str (field-value diagnostic "message"))
    :detail (str (field-value diagnostic "detail"))}))

(defn- unexpected-outcome
  [test-name value]
  {:status "error"
   :name test-name
   :diagnostic (diagnostic-map "runtime"
                               "Unexpected test outcome."
                               (str (.getName (.getClass value))))})

(defn- outcome-map
  [test-name value]
  (let [class-name (.getName (.getClass value))]
    (cond
      (.endsWith class-name "$TestOutcome$Pass")
      {:status "pass"
       :name (str (field-value value "name"))}

      (.endsWith class-name "$TestOutcome$Fail")
      {:status "fail"
       :name (str (field-value value "name"))
       :diagnostic (diagnostic-map (field-value value "diagnostic"))}

      (.endsWith class-name "$TestOutcome$Error")
      {:status "error"
       :name (str (field-value value "name"))
       :diagnostic (diagnostic-map (field-value value "diagnostic"))}

      :else
      (unexpected-outcome test-name value))))

(defn- throwable-detail
  [throwable]
  (str (.getName (class throwable)) ": " (.getMessage throwable)))

(defn- execution-error
  [test-name throwable]
  {:status "error"
   :name test-name
   :diagnostic (diagnostic-map "runtime"
                               "Execution error."
                               (throwable-detail throwable))})

(defn- invoke-test
  [klass test-decl]
  (let [name (:name test-decl)
        method (.getMethod klass (method-name name) (into-array Class []))]
    (try
      (outcome-map (str name) (.invoke method nil (object-array [])))
      (catch java.lang.reflect.InvocationTargetException e
        (execution-error (str name) (.getTargetException e))))))

(defn- invoke-test-suite
  [module-name klass suite-decl]
  (let [method (.getMethod klass (method-name (:name suite-decl)) (into-array Class []))]
    (try
      (summary module-name (mapv #(outcome-map nil %) (.invoke method nil (object-array []))))
      (catch java.lang.reflect.InvocationTargetException e
        (summary module-name [(execution-error "tests" (.getTargetException e))])))))

(defn- summary
  [module-name outcomes]
  {:module (str module-name)
   :passed (count (filter #(= "pass" (:status %)) outcomes))
   :failed (count (filter #(= "fail" (:status %)) outcomes))
   :errored (count (filter #(= "error" (:status %)) outcomes))
   :outcomes outcomes})

(defn- run-tests
  [module bundle export-order]
  (ensure-canonical-test-module! module export-order)
  (let [output-dir (temp-output-dir)
        _ (compiler/write-bundle! bundle output-dir)
        klass (load-class-from-dir output-dir (class-name module))
        suite-decl (exported-test-suite module export-order)]
    (invoke-test-suite (:name module) klass suite-decl)))

(defn run-source-tests!
  [source]
  (let [parsed (parser/parse-module source)
        module (compiler/checked-module parsed)
        bundle (compiler/compile-source source)]
    (run-tests module bundle (:exports parsed))))

(defn run-project-source-tests!
  [module-sources root-module-name]
  (let [parsed (parser/parse-module (get module-sources root-module-name))
        module (compiler/checked-project-module module-sources root-module-name)
        bundle (compiler/compile-project-source module-sources root-module-name)]
    (run-tests module bundle (:exports parsed))))

(defn run-project-dir-tests!
  [project-dir root-module-name]
  (let [source (project-files/root-source project-dir root-module-name)
        parsed (parser/parse-module source)
        module (compiler/checked-project-dir-module project-dir root-module-name)
        bundle (compiler/compile-project-dir project-dir root-module-name)]
    (run-tests module bundle (:exports parsed))))

(defn summary->json
  [summary]
  (json/write-str summary))

(defn summary->text
  [summary]
  (let [lines (map (fn [outcome]
                     (case (:status outcome)
                       "pass" (str "PASS " (:name outcome))
                       "fail" (str "FAIL " (:name outcome) ": " (get-in outcome [:diagnostic :message]))
                       "error" (str "ERROR " (:name outcome) ": " (get-in outcome [:diagnostic :message]))))
                   (:outcomes summary))]
    (str "Module: "
         (:module summary)
         "\n"
         (str/join "\n" lines)
         (when (seq lines) "\n")
         "Summary: "
         (:passed summary)
         " passed, "
         (:failed summary)
         " failed, "
         (:errored summary)
         " errored")))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-15T13:47:06.798619-05:00", :module-hash "-803272560", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 7, :hash "-601430363"} {:id "def/test-suite-return-type", :kind "def", :line 9, :end-line 10, :hash "-2092148690"} {:id "def/main-param-type", :kind "def", :line 12, :end-line 13, :hash "1719642001"} {:id "form/3/declare", :kind "declare", :line 15, :end-line 15, :hash "2053142492"} {:id "defn-/temp-output-dir", :kind "defn-", :line 17, :end-line 21, :hash "559598651"} {:id "defn-/class-name", :kind "defn-", :line 23, :end-line 25, :hash "-314160674"} {:id "defn-/method-name", :kind "defn-", :line 27, :end-line 29, :hash "-1731610028"} {:id "defn-/load-class-from-dir", :kind "defn-", :line 31, :end-line 36, :hash "-1739352517"} {:id "defn-/exported-test-suite", :kind "defn-", :line 38, :end-line 47, :hash "-1803344014"} {:id "defn-/exported-main", :kind "defn-", :line 49, :end-line 60, :hash "-1689303310"} {:id "defn-/fail!", :kind "defn-", :line 62, :end-line 64, :hash "-459802263"} {:id "defn-/ensure-canonical-test-module!", :kind "defn-", :line 66, :end-line 75, :hash "-1563303375"} {:id "defn-/field-value", :kind "defn-", :line 77, :end-line 79, :hash "-2078382156"} {:id "defn-/diagnostic-map", :kind "defn-", :line 81, :end-line 89, :hash "-1957919945"} {:id "defn-/unexpected-outcome", :kind "defn-", :line 91, :end-line 97, :hash "-1505678912"} {:id "defn-/outcome-map", :kind "defn-", :line 99, :end-line 118, :hash "1550567249"} {:id "defn-/throwable-detail", :kind "defn-", :line 120, :end-line 122, :hash "-328705310"} {:id "defn-/execution-error", :kind "defn-", :line 124, :end-line 130, :hash "-2023157389"} {:id "defn-/invoke-test", :kind "defn-", :line 132, :end-line 139, :hash "-445921971"} {:id "defn-/invoke-test-suite", :kind "defn-", :line 141, :end-line 147, :hash "-29650502"} {:id "defn-/summary", :kind "defn-", :line 149, :end-line 155, :hash "2038126858"} {:id "defn-/run-tests", :kind "defn-", :line 157, :end-line 164, :hash "1284299481"} {:id "defn/run-source-tests!", :kind "defn", :line 166, :end-line 171, :hash "825598030"} {:id "defn/run-project-source-tests!", :kind "defn", :line 173, :end-line 178, :hash "1881464140"} {:id "defn/run-project-dir-tests!", :kind "defn", :line 180, :end-line 186, :hash "-513862989"} {:id "defn/summary->json", :kind "defn", :line 188, :end-line 190, :hash "-143482131"} {:id "defn/summary->text", :kind "defn", :line 192, :end-line 211, :hash "158746297"}]}
;; clj-mutate-manifest-end
