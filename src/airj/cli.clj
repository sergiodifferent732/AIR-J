(ns airj.cli
  (:require [airj.compiler :as compiler]
            [airj.effect-checker :as effect-checker]
            [airj.exhaustiveness-checker :as exhaustiveness-checker]
            [airj.java-resolver :as java-resolver]
            [airj.jvm-lowerer :as jvm-lowerer]
            [airj.normalizer :as normalizer]
            [airj.parser :as parser]
            [airj.resolver :as resolver]
            [airj.test-runner :as test-runner]
            [airj.type-checker :as type-checker]
            [clojure.data.json :as json]
            [clojure.edn :as edn]))

(defn- parse-command
  [source]
  (pr-str (parser/parse-module source)))

(defn- normalize-command
  [source]
  (-> source
      parser/parse-module
      normalizer/normalize-module
      pr-str))

(defn- check-command
  [source options]
  (-> source
      parser/parse-module
      (compiler/checked-module options)
      pr-str))

(defn- lower-command
  [source options]
  (-> source
      parser/parse-module
      (compiler/checked-module options)
      jvm-lowerer/lower-module
      pr-str))

(defn- interface-command
  [source]
  (-> source
      compiler/interface-source
      pr-str))

(defn- build-command
  [source output-dir options]
  (-> (if-let [jar-output (:jar-output options)]
        (compiler/build-source-jar! source jar-output options)
        (compiler/build-source! source output-dir options))
      pr-str))

(defn- run-command
  [source args options]
  (if-some [result (compiler/run-source! source args options)]
    (pr-str result)
    ""))

(defn- test-output
  [summary options]
  (if (:json-output options)
    (json/write-str summary)
    (test-runner/summary->text summary)))

(defn- test-command
  [source options]
  (-> source
      test-runner/run-source-tests!
      (test-output options)))

(defn- read-source
  [source-option stdin]
  (if (= "--stdin" source-option)
    stdin
    (slurp source-option)))

(defn- read-interfaces
  [path]
  (-> path
      slurp
      edn/read-string))

(defn- read-interface-sources
  [path]
  (-> path
      slurp
      edn/read-string))

(defn- read-project-sources
  [path]
  (-> path
      slurp
      edn/read-string))

(defn- read-project-dir
  [path]
  path)

(def ^:private option-readers
  {"--interfaces-edn" {:key :interfaces
                       :read read-interfaces
                       :advance nnext}
   "--interface-sources-edn" {:key :interface-sources
                              :read read-interface-sources
                              :advance nnext}
   "--project-sources-edn" {:key :project-sources
                            :read read-project-sources
                            :advance nnext}
   "--project-dir" {:key :project-dir
                    :read read-project-dir
                    :advance nnext}
   "--jar" {:key :jar-output
            :read identity
            :advance nnext}
   "--json" {:key :json-output
             :read (constantly true)
             :advance next}})

(defn- consume-option
  [options remaining]
  (when-let [{:keys [key read advance]} (get option-readers (first remaining))]
    [(assoc options key (read (second remaining)))
     (advance remaining)]))

(defn- parse-options
  [args]
  (loop [options {}
         remaining args]
    (if-let [[next-options next-remaining] (consume-option options remaining)]
      (recur next-options next-remaining)
      [options remaining])))

(def ^:private source-commands
  {"parse" (fn [source _] (parse-command source))
   "normalize" (fn [source _] (normalize-command source))
   "check" check-command
   "interface" (fn [source _] (interface-command source))
   "lower" lower-command
   "test" test-command})

(def ^:private project-commands
  {"check" (fn [project-sources root-module-name _ _]
             (-> (compiler/checked-project-module project-sources root-module-name)
                 pr-str))
   "lower" (fn [project-sources root-module-name _ _]
             (-> (compiler/checked-project-module project-sources root-module-name)
                 jvm-lowerer/lower-module
                 pr-str))
   "build" (fn [project-sources root-module-name args options]
             (-> (if-let [jar-output (:jar-output options)]
                   (compiler/build-project-source-jar! project-sources
                                                      root-module-name
                                                      jar-output)
                   (compiler/build-project-source! project-sources
                                                  root-module-name
                                                  (or (first args) "target/classes")))
                 pr-str))
   "run" (fn [project-sources root-module-name args _]
           (if-some [result (compiler/run-project-source! project-sources
                                                          root-module-name
                                                          args)]
             (pr-str result)
             ""))
   "test" (fn [project-sources root-module-name _ options]
            (-> (test-runner/run-project-source-tests! project-sources root-module-name)
                (test-output options)))})

(def ^:private project-dir-commands
  {"check" (fn [project-dir root-module-name _ _]
             (-> (compiler/checked-project-dir-module project-dir root-module-name)
                 pr-str))
   "lower" (fn [project-dir root-module-name _ _]
             (-> (compiler/checked-project-dir-module project-dir root-module-name)
                 jvm-lowerer/lower-module
                 pr-str))
   "build" (fn [project-dir root-module-name args options]
             (-> (if-let [jar-output (:jar-output options)]
                   (compiler/build-project-dir-jar! project-dir
                                                   root-module-name
                                                   jar-output)
                   (compiler/build-project-dir! project-dir
                                               root-module-name
                                               (or (first args) "target/classes")))
                 pr-str))
   "run" (fn [project-dir root-module-name args _]
           (if-some [result (compiler/run-project-dir! project-dir root-module-name args)]
             (pr-str result)
             ""))
   "test" (fn [project-dir root-module-name _ options]
            (-> (test-runner/run-project-dir-tests! project-dir root-module-name)
                (test-output options)))})

(defn- source-command-result
  [command source options]
  (if-let [handler (get source-commands command)]
    (handler source options)
    (throw (ex-info "Unsupported command."
                    {:phase :cli
                     :args [command]}))))

(defn- project-command-result
  [command project-sources root-module-name args options]
  (if-let [handler (get project-commands command)]
    (handler project-sources root-module-name args options)
    (throw (ex-info "Unsupported command."
                    {:phase :cli
                     :args [command root-module-name]}))))

(defn- project-command-args
  [command remaining-args]
  (case command
    "build" [(second remaining-args)]
    "run" (vec (rest remaining-args))
    []))

(defn- project-root-module
  [remaining-args]
  (symbol (first remaining-args)))

(defn- with-project-context
  [command root-module-name thunk]
  (try
    (thunk)
    (catch clojure.lang.ExceptionInfo e
      (throw (ex-info "Project command failed."
                      (assoc (ex-data e)
                             :phase :project
                             :command command
                             :root-module root-module-name)
                      e)))))

(defn- project-result
  [command options remaining-args]
  (let [root-module-name (project-root-module remaining-args)]
    (with-project-context command
                          root-module-name
                           #(project-command-result command
                                                   (:project-sources options)
                                                   root-module-name
                                                   (project-command-args command remaining-args)
                                                   options))))

(defn- project-dir-result
  [command options remaining-args]
  (let [project-dir (:project-dir options)
        root-module-name (project-root-module remaining-args)
        args (project-command-args command remaining-args)]
    (with-project-context command
                          root-module-name
                          #(if-let [handler (get project-dir-commands command)]
                             (handler project-dir root-module-name args options)
                             (throw (ex-info "Unsupported command."
                                             {:phase :cli
                                              :args [command root-module-name]}))))))

(defn- source-result
  [command options remaining-args stdin]
  (let [[source-option output-dir] remaining-args]
    (case command
      "build" (build-command (read-source source-option stdin)
                             (or output-dir "target/classes")
                             options)
      "run" (run-command (read-source source-option stdin)
                         (rest remaining-args)
                         options)
      (source-command-result command
                             (read-source source-option stdin)
                             options))))

(defn- command-result
  [command args stdin]
  (let [[options remaining-args] (parse-options args)]
    (cond
      (:project-sources options)
      (project-result command options remaining-args)

      (:project-dir options)
      (project-dir-result command options remaining-args)

      (contains? source-commands command)
      (source-result command options remaining-args stdin)

      (#{"build" "run"} command)
      (source-result command options remaining-args stdin)

      :else
      (throw (ex-info "Unsupported command."
                      {:phase :cli
                       :args (cons command args)})))))

(defn run
  [args stdin]
  (let [[command & command-args] args]
    (command-result command command-args stdin)))

(defn- stdin-required?
  [args]
  (some #{"--stdin"} args))

(defn- diagnostic-data
  [error]
  (let [data (ex-data error)
        cause (ex-cause error)]
    (cond-> {:message (.getMessage error)}
      (:phase data) (assoc :phase (:phase data))
      (seq data) (assoc :data data)
      cause (assoc :cause (cond-> {:message (.getMessage cause)}
                            (:phase (ex-data cause)) (assoc :phase (:phase (ex-data cause)))
                            (seq (ex-data cause)) (assoc :data (ex-data cause)))))))

(defn- format-cli-error
  [error]
  (pr-str (diagnostic-data error)))

(defn- handle-cli-exception
  [error]
  (binding [*out* *err*]
    (println (format-cli-error error)))
  1)

(defn -main
  [& args]
  (try
    (println (run args (if (stdin-required? args) (slurp *in*) "")))
    0
    (catch clojure.lang.ExceptionInfo e
      (handle-cli-exception e))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-15T11:54:57.027029-05:00", :module-hash "-283285449", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 13, :hash "883067072"} {:id "defn-/parse-command", :kind "defn-", :line 15, :end-line 17, :hash "1121072814"} {:id "defn-/normalize-command", :kind "defn-", :line 19, :end-line 24, :hash "1007829560"} {:id "defn-/check-command", :kind "defn-", :line 26, :end-line 31, :hash "1273151433"} {:id "defn-/lower-command", :kind "defn-", :line 33, :end-line 39, :hash "2093538237"} {:id "defn-/interface-command", :kind "defn-", :line 41, :end-line 45, :hash "-347479495"} {:id "defn-/build-command", :kind "defn-", :line 47, :end-line 52, :hash "599793461"} {:id "defn-/run-command", :kind "defn-", :line 54, :end-line 58, :hash "1684929123"} {:id "defn-/test-output", :kind "defn-", :line 60, :end-line 64, :hash "-187021782"} {:id "defn-/test-command", :kind "defn-", :line 66, :end-line 70, :hash "-1359927827"} {:id "defn-/read-source", :kind "defn-", :line 72, :end-line 76, :hash "-1955156800"} {:id "defn-/read-interfaces", :kind "defn-", :line 78, :end-line 82, :hash "-125656322"} {:id "defn-/read-interface-sources", :kind "defn-", :line 84, :end-line 88, :hash "-19164240"} {:id "defn-/read-project-sources", :kind "defn-", :line 90, :end-line 94, :hash "-1257545745"} {:id "defn-/read-project-dir", :kind "defn-", :line 96, :end-line 98, :hash "1367042759"} {:id "def/option-readers", :kind "def", :line 100, :end-line 118, :hash "319966490"} {:id "defn-/consume-option", :kind "defn-", :line 120, :end-line 124, :hash "-875269635"} {:id "defn-/parse-options", :kind "defn-", :line 126, :end-line 132, :hash "-327026084"} {:id "def/source-commands", :kind "def", :line 134, :end-line 140, :hash "424826235"} {:id "def/project-commands", :kind "def", :line 142, :end-line 167, :hash "-1156792275"} {:id "def/project-dir-commands", :kind "def", :line 169, :end-line 192, :hash "-798074608"} {:id "defn-/source-command-result", :kind "defn-", :line 194, :end-line 200, :hash "2021591110"} {:id "defn-/project-command-result", :kind "defn-", :line 202, :end-line 208, :hash "-1387662110"} {:id "defn-/project-command-args", :kind "defn-", :line 210, :end-line 215, :hash "1835019243"} {:id "defn-/project-root-module", :kind "defn-", :line 217, :end-line 219, :hash "1262629310"} {:id "defn-/with-project-context", :kind "defn-", :line 221, :end-line 231, :hash "-115430676"} {:id "defn-/project-result", :kind "defn-", :line 233, :end-line 242, :hash "2030947946"} {:id "defn-/project-dir-result", :kind "defn-", :line 244, :end-line 255, :hash "-575643101"} {:id "defn-/source-result", :kind "defn-", :line 257, :end-line 269, :hash "253873564"} {:id "defn-/command-result", :kind "defn-", :line 271, :end-line 290, :hash "-1586741967"} {:id "defn/run", :kind "defn", :line 292, :end-line 295, :hash "-360055027"} {:id "defn-/stdin-required?", :kind "defn-", :line 297, :end-line 299, :hash "1700238319"} {:id "defn-/diagnostic-data", :kind "defn-", :line 301, :end-line 310, :hash "-121100729"} {:id "defn-/format-cli-error", :kind "defn-", :line 312, :end-line 314, :hash "1099909786"} {:id "defn-/handle-cli-exception", :kind "defn-", :line 316, :end-line 320, :hash "2060159512"} {:id "defn/-main", :kind "defn", :line 322, :end-line 328, :hash "-1422999948"}]}
;; clj-mutate-manifest-end
