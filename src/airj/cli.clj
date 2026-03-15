(ns airj.cli
  (:require [airj.compiler :as compiler]
            [airj.effect-checker :as effect-checker]
            [airj.exhaustiveness-checker :as exhaustiveness-checker]
            [airj.java-resolver :as java-resolver]
            [airj.jvm-lowerer :as jvm-lowerer]
            [airj.normalizer :as normalizer]
            [airj.parser :as parser]
            [airj.resolver :as resolver]
            [airj.type-checker :as type-checker]
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

(defn- parse-options
  [args]
  (loop [options {}
         remaining args]
    (cond
      (= "--interfaces-edn" (first remaining))
      (recur (assoc options :interfaces (read-interfaces (second remaining)))
             (nnext remaining))

      (= "--interface-sources-edn" (first remaining))
      (recur (assoc options :interface-sources (read-interface-sources (second remaining)))
             (nnext remaining))

      (= "--project-sources-edn" (first remaining))
      (recur (assoc options :project-sources (read-project-sources (second remaining)))
             (nnext remaining))

      (= "--project-dir" (first remaining))
      (recur (assoc options :project-dir (read-project-dir (second remaining)))
             (nnext remaining))

      (= "--jar" (first remaining))
      (recur (assoc options :jar-output (second remaining))
             (nnext remaining))

      :else
      [options remaining])))

(def ^:private source-commands
  {"parse" (fn [source _] (parse-command source))
   "normalize" (fn [source _] (normalize-command source))
   "check" check-command
   "interface" (fn [source _] (interface-command source))
   "lower" lower-command})

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
             ""))})

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
             ""))})

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
;; {:version 1, :tested-at "2026-03-15T10:50:52.696658-05:00", :module-hash "1337544775", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 11, :hash "1645197149"} {:id "defn-/parse-command", :kind "defn-", :line 13, :end-line 15, :hash "1121072814"} {:id "defn-/normalize-command", :kind "defn-", :line 17, :end-line 22, :hash "1007829560"} {:id "defn-/check-command", :kind "defn-", :line 24, :end-line 29, :hash "1273151433"} {:id "defn-/lower-command", :kind "defn-", :line 31, :end-line 37, :hash "2093538237"} {:id "defn-/interface-command", :kind "defn-", :line 39, :end-line 43, :hash "-347479495"} {:id "defn-/build-command", :kind "defn-", :line 45, :end-line 50, :hash "599793461"} {:id "defn-/run-command", :kind "defn-", :line 52, :end-line 56, :hash "1684929123"} {:id "defn-/read-source", :kind "defn-", :line 58, :end-line 62, :hash "-1955156800"} {:id "defn-/read-interfaces", :kind "defn-", :line 64, :end-line 68, :hash "-125656322"} {:id "defn-/read-interface-sources", :kind "defn-", :line 70, :end-line 74, :hash "-19164240"} {:id "defn-/read-project-sources", :kind "defn-", :line 76, :end-line 80, :hash "-1257545745"} {:id "defn-/read-project-dir", :kind "defn-", :line 82, :end-line 84, :hash "1367042759"} {:id "defn-/parse-options", :kind "defn-", :line 86, :end-line 112, :hash "19057762"} {:id "def/source-commands", :kind "def", :line 114, :end-line 119, :hash "1484354291"} {:id "def/project-commands", :kind "def", :line 121, :end-line 143, :hash "-1200634936"} {:id "def/project-dir-commands", :kind "def", :line 145, :end-line 165, :hash "-928067373"} {:id "defn-/source-command-result", :kind "defn-", :line 167, :end-line 173, :hash "2021591110"} {:id "defn-/project-command-result", :kind "defn-", :line 175, :end-line 181, :hash "-1387662110"} {:id "defn-/project-command-args", :kind "defn-", :line 183, :end-line 188, :hash "1835019243"} {:id "defn-/project-root-module", :kind "defn-", :line 190, :end-line 192, :hash "1262629310"} {:id "defn-/with-project-context", :kind "defn-", :line 194, :end-line 204, :hash "-115430676"} {:id "defn-/project-result", :kind "defn-", :line 206, :end-line 215, :hash "2030947946"} {:id "defn-/project-dir-result", :kind "defn-", :line 217, :end-line 228, :hash "-575643101"} {:id "defn-/source-result", :kind "defn-", :line 230, :end-line 242, :hash "253873564"} {:id "defn-/command-result", :kind "defn-", :line 244, :end-line 263, :hash "-1586741967"} {:id "defn/run", :kind "defn", :line 265, :end-line 268, :hash "-360055027"} {:id "defn-/stdin-required?", :kind "defn-", :line 270, :end-line 272, :hash "1700238319"} {:id "defn-/diagnostic-data", :kind "defn-", :line 274, :end-line 283, :hash "-121100729"} {:id "defn-/format-cli-error", :kind "defn-", :line 285, :end-line 287, :hash "1099909786"} {:id "defn-/handle-cli-exception", :kind "defn-", :line 289, :end-line 293, :hash "2060159512"} {:id "defn/-main", :kind "defn", :line 295, :end-line 301, :hash "-1422999948"}]}
;; clj-mutate-manifest-end
