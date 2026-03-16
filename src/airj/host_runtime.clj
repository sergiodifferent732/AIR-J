(ns airj.host-runtime
  (:require [clojure.java.io :as io])
  (:import (com.sun.net.httpserver HttpExchange HttpHandler HttpServer)
           (java.net InetSocketAddress)
           (java.util.concurrent LinkedBlockingQueue)))

(defn- fail!
  [message data]
  (throw (ex-info message data)))

(defn- nested-class
  [^Class root suffix]
  (.loadClass (.getClassLoader root)
              (str (.getName root) "$" suffix)))

(defn- instantiate
  [^Class root suffix parameter-types values]
  (let [klass (if (= "" suffix) root (nested-class root suffix))
        ctor (.getConstructor klass (into-array Class parameter-types))]
    (.newInstance ctor (object-array values))))

(defn- option-none
  [^Class option-root]
  (instantiate option-root "None" [] []))

(defn- option-some
  [^Class option-root value]
  (instantiate option-root "Some" [Object] [value]))

(defn env-get
  [name ^Class option-root]
  (if-some [value (System/getenv ^String name)]
    (option-some option-root value)
    (option-none option-root)))

(defn- read-all-bytes
  [stream]
  (with-open [in stream
              out (java.io.ByteArrayOutputStream.)]
    (io/copy in out)
    (.toByteArray out)))

(defn- await-bytes
  [stream]
  @(future (read-all-bytes stream)))

(defn- process-builder
  [command]
  (ProcessBuilder. ^java.util.List command))

(defn process-run
  [command stdin-bytes ^Class process-result-root]
  (try
    (let [builder (process-builder command)
          process (.start builder)
          _ (with-open [out (.getOutputStream process)]
              (.write out ^bytes stdin-bytes))
          stdout (await-bytes (.getInputStream process))
          stderr (await-bytes (.getErrorStream process))
          exit-code (.waitFor process)]
      (instantiate process-result-root
                   ""
                   [Integer/TYPE (Class/forName "[B") (Class/forName "[B")]
                   [exit-code stdout stderr]))
    (catch RuntimeException ex
      (throw ex))
    (catch Exception ex
      (throw (ex-info "Process execution failed."
                      {:command (vec command)}
                      ex)))))

(defn cwd
  []
  (or (System/getProperty "user.dir")
      (fail! "Working directory unavailable." {})))

(defn- field-value
  [instance field-name]
  (.get (.getField (class instance) ^String field-name) instance))

(defrecord HttpServerState [server queue pending next-id port])

(defn- server-state
  [server-record]
  ^HttpServerState (field-value server-record "handle"))

(defn- request-map
  [id ^HttpExchange exchange]
  {:id (int id)
   :method (.getRequestMethod exchange)
   :path (.getRawPath (.getRequestURI exchange))
   :body (slurp (.getRequestBody exchange) :encoding "UTF-8")})

(defn http-listen
  [port ^Class server-root]
  (try
    (let [queue (LinkedBlockingQueue.)
          pending (atom {})
          next-id (atom 0)
          server (HttpServer/create (InetSocketAddress. (int port)) 0)]
      (.createContext server
                      "/"
                      (reify HttpHandler
                        (handle [_ exchange]
                          (let [id (swap! next-id inc)]
                            (swap! pending assoc id exchange)
                            (.put queue (request-map id exchange))))))
      (.setExecutor server nil)
      (.start server)
      (instantiate server-root
                   ""
                   [Object]
                   [(->HttpServerState server
                                       queue
                                       pending
                                       next-id
                                       (.getPort (.getAddress server)))]))
    (catch RuntimeException ex
      (throw ex))
    (catch Exception ex
      (throw (ex-info "HTTP listen failed."
                      {:port port}
                      ex)))))

(defn http-port
  [server-record]
  (:port (server-state server-record)))

(defn http-accept
  [server-record ^Class request-root]
  (try
    (let [request (.take ^LinkedBlockingQueue (:queue (server-state server-record)))]
      (instantiate request-root
                   ""
                   [Integer/TYPE String String String]
                   [(int (:id request)) (:method request) (:path request) (:body request)]))
    (catch RuntimeException ex
      (throw ex))
    (catch Exception ex
      (throw (ex-info "HTTP accept failed." {} ex)))))

(defn http-respond
  [server-record request-record response-record]
  (try
    (let [{:keys [pending]} (server-state server-record)
          request-id (field-value request-record "id")
          exchange (get @pending request-id)]
      (when-not exchange
        (fail! "HTTP response failed." {:request-id request-id}))
      (swap! pending dissoc request-id)
      (let [status (int (field-value response-record "status"))
            content-type (field-value response-record "contentType")
            body (field-value response-record "body")
            bytes (.getBytes ^String body "UTF-8")]
        (.set (.getResponseHeaders ^HttpExchange exchange)
              "Content-Type"
              ^String content-type)
        (.sendResponseHeaders ^HttpExchange exchange status (long (alength bytes)))
        (with-open [out (.getResponseBody ^HttpExchange exchange)]
          (.write out ^bytes bytes)))
      nil)
    (catch RuntimeException ex
      (throw ex))
    (catch Exception ex
      (throw (ex-info "HTTP respond failed." {} ex)))))

(defn http-close
  [server-record]
  (try
    (.stop ^HttpServer (:server (server-state server-record)) 0)
    nil
    (catch RuntimeException ex
      (throw ex))
    (catch Exception ex
      (throw (ex-info "HTTP close failed." {} ex)))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T07:46:29.107852-05:00", :module-hash "1431500576", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "207570792"} {:id "defn-/fail!", :kind "defn-", :line 7, :end-line 9, :hash "879938479"} {:id "defn-/nested-class", :kind "defn-", :line 11, :end-line 14, :hash "1735185178"} {:id "defn-/instantiate", :kind "defn-", :line 16, :end-line 20, :hash "-248836681"} {:id "defn-/option-none", :kind "defn-", :line 22, :end-line 24, :hash "-108896378"} {:id "defn-/option-some", :kind "defn-", :line 26, :end-line 28, :hash "353731076"} {:id "defn/env-get", :kind "defn", :line 30, :end-line 34, :hash "1771397497"} {:id "defn-/read-all-bytes", :kind "defn-", :line 36, :end-line 41, :hash "894071159"} {:id "defn-/await-bytes", :kind "defn-", :line 43, :end-line 45, :hash "506605214"} {:id "defn-/process-builder", :kind "defn-", :line 47, :end-line 49, :hash "-2053909289"} {:id "defn/process-run", :kind "defn", :line 51, :end-line 70, :hash "-990801009"} {:id "defn/cwd", :kind "defn", :line 72, :end-line 75, :hash "1149366332"} {:id "defn-/field-value", :kind "defn-", :line 77, :end-line 79, :hash "-14028815"} {:id "form/13/defrecord", :kind "defrecord", :line 81, :end-line 81, :hash "-720980307"} {:id "defn-/server-state", :kind "defn-", :line 83, :end-line 85, :hash "1368756026"} {:id "defn-/request-map", :kind "defn-", :line 87, :end-line 92, :hash "1925471879"} {:id "defn/http-listen", :kind "defn", :line 94, :end-line 123, :hash "420050244"} {:id "defn/http-port", :kind "defn", :line 125, :end-line 127, :hash "-222708065"} {:id "defn/http-accept", :kind "defn", :line 129, :end-line 140, :hash "-777451927"} {:id "defn/http-respond", :kind "defn", :line 142, :end-line 165, :hash "-1500565875"} {:id "defn/http-close", :kind "defn", :line 167, :end-line 175, :hash "121228757"}]}
;; clj-mutate-manifest-end
