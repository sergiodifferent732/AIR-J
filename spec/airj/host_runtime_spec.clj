(ns airj.host-runtime-spec
  (:require [airj.compiler :as compiler]
            [airj.host-runtime :as sut]
            [speclj.core :refer :all]))

(defn- binary-name
  [internal-name]
  (.replace ^String internal-name \/ \.))

(defn- define-classes
  [bytecode-map]
  (let [loader (clojure.lang.DynamicClassLoader.)]
    (into {}
          (map (fn [[internal-name bytecode]]
                 [internal-name
                  (.defineClass loader (binary-name internal-name) bytecode nil)]))
          (sort-by key bytecode-map))))

(describe "host runtime"
  (let [bundle (delay (compiler/compile-source "(module example/host_bundle
                                                 (imports
                                                   (airj airj/core Option)
                                                   (airj airj/process ProcessResult)
                                                   (airj airj/http HttpServer HttpRequest HttpResponse))
                                                 (export main)
                                                 (fn main
                                                   (params)
                                                   (returns Int)
                                                   (effects ())
                                                   (requires true)
                                                   (ensures true)
                                                   0))"))
        classes (delay (define-classes @bundle))
        option-root (delay (get @classes "airj/core$Option"))
        process-root (delay (get @classes "airj/process$ProcessResult"))
        http-server-root (delay (get @classes "airj/http$HttpServer"))
        http-request-root (delay (get @classes "airj/http$HttpRequest"))
        http-response-root (delay (get @classes "airj/http$HttpResponse"))]
    (it "returns Some or None for environment lookup"
      (let [some-value (sut/env-get "PATH" @option-root)
            none-value (sut/env-get "__AIRJ_MISSING_ENV__" @option-root)]
        (should (.endsWith (.getName (class some-value)) "$Some"))
        (should (.endsWith (.getName (class none-value)) "$None"))))

    (it "executes subprocesses into canonical process results"
      (let [result (sut/process-run ["/bin/cat"]
                                    (.getBytes "{\"tool\":\"ok\"}" "UTF-8")
                                    @process-root)
            fields (vec (.getFields (class result)))
            exit-code (.get (first (filter #(= Integer/TYPE (.getType %)) fields)) result)
            byte-values (map #(.get % result)
                             (filter #(= (Class/forName "[B") (.getType %)) fields))]
        (should= 0 exit-code)
        (should= #{"{\"tool\":\"ok\"}" ""}
                 (set (map #(String. ^bytes % "UTF-8") byte-values)))))

    (it "accepts and responds to HTTP requests through canonical runtime values"
      (let [server (sut/http-listen 0 @http-server-root)
            port (sut/http-port server)
            request-future (future (sut/http-accept server @http-request-root))
            client (java.net.http.HttpClient/newHttpClient)
            uri (java.net.URI/create (str "http://127.0.0.1:" port "/wiki/Home"))
            response-future (future
                              (.send client
                                     (-> (java.net.http.HttpRequest/newBuilder uri)
                                         (.GET)
                                         (.build))
                                     (java.net.http.HttpResponse$BodyHandlers/ofString)))
            request @request-future
            ctor (.getConstructor @http-response-root
                                  (into-array Class [Integer/TYPE String String]))
            response (.newInstance ctor
                                   (object-array [(int 200) "text/plain; charset=utf-8" "served"]))]
        (sut/http-respond server request response)
        (let [client-response @response-future]
          (should= "/wiki/Home" (.get (.getField (class request) "path") request))
          (should= "GET" (.get (.getField (class request) "method") request))
          (should= 200 (.statusCode client-response))
          (should= "served" (.body client-response)))
        (sut/http-close server)))))
