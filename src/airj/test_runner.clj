(ns airj.test-runner
  (:require [airj.compiler :as compiler]
            [airj.parser :as parser]
            [airj.project-files :as project-files]
            [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.string :as str]))

(def ^:private test-return-types
  #{'TestOutcome 'TestPass 'TestFail 'TestError})

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

(defn- exported-tests
  [module export-order]
  (let [decls-by-name (into {} (map (juxt :name identity) (:decls module)))]
    (->> export-order
         (map decls-by-name)
         (filter #(and (= :fn (:op %))
                       (empty? (:params %))
                       (contains? test-return-types (:return-type %))))
         vec)))

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

(defn- summary
  [outcomes]
  {:passed (count (filter #(= "pass" (:status %)) outcomes))
   :failed (count (filter #(= "fail" (:status %)) outcomes))
   :errored (count (filter #(= "error" (:status %)) outcomes))
   :outcomes outcomes})

(defn- run-tests
  [module bundle export-order]
  (let [output-dir (temp-output-dir)
        _ (compiler/write-bundle! bundle output-dir)
        klass (load-class-from-dir output-dir (class-name module))]
    (summary (mapv #(invoke-test klass %) (exported-tests module export-order)))))

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
    (str (str/join "\n" lines)
         (when (seq lines) "\n")
         "Summary: "
         (:passed summary)
         " passed, "
         (:failed summary)
         " failed, "
         (:errored summary)
         " errored")))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-15T11:53:55.969234-05:00", :module-hash "-925322778", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 7, :hash "-601430363"} {:id "def/test-return-types", :kind "def", :line 9, :end-line 10, :hash "-870535961"} {:id "defn-/temp-output-dir", :kind "defn-", :line 12, :end-line 16, :hash "559598651"} {:id "defn-/class-name", :kind "defn-", :line 18, :end-line 20, :hash "-314160674"} {:id "defn-/method-name", :kind "defn-", :line 22, :end-line 24, :hash "-1731610028"} {:id "defn-/load-class-from-dir", :kind "defn-", :line 26, :end-line 31, :hash "-1739352517"} {:id "defn-/exported-tests", :kind "defn-", :line 33, :end-line 41, :hash "-1503506462"} {:id "defn-/field-value", :kind "defn-", :line 43, :end-line 45, :hash "-2078382156"} {:id "defn-/diagnostic-map", :kind "defn-", :line 47, :end-line 55, :hash "-1957919945"} {:id "defn-/unexpected-outcome", :kind "defn-", :line 57, :end-line 63, :hash "-1505678912"} {:id "defn-/outcome-map", :kind "defn-", :line 65, :end-line 84, :hash "1550567249"} {:id "defn-/throwable-detail", :kind "defn-", :line 86, :end-line 88, :hash "-328705310"} {:id "defn-/execution-error", :kind "defn-", :line 90, :end-line 96, :hash "-2023157389"} {:id "defn-/invoke-test", :kind "defn-", :line 98, :end-line 105, :hash "-445921971"} {:id "defn-/summary", :kind "defn-", :line 107, :end-line 112, :hash "-1471394056"} {:id "defn-/run-tests", :kind "defn-", :line 114, :end-line 119, :hash "-1644101752"} {:id "defn/run-source-tests!", :kind "defn", :line 121, :end-line 126, :hash "825598030"} {:id "defn/run-project-source-tests!", :kind "defn", :line 128, :end-line 133, :hash "1881464140"} {:id "defn/run-project-dir-tests!", :kind "defn", :line 135, :end-line 141, :hash "-513862989"} {:id "defn/summary->json", :kind "defn", :line 143, :end-line 145, :hash "-143482131"} {:id "defn/summary->text", :kind "defn", :line 147, :end-line 163, :hash "-1794054746"}]}
;; clj-mutate-manifest-end
