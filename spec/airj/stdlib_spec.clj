(ns airj.stdlib-spec
  (:require [airj.parser :as parser]
            [airj.stdlib :as sut]
            [speclj.core :refer :all]))

(describe "stdlib"
  (it "exposes the canonical standard modules"
    (should= #{'airj/bytes 'airj/core 'airj/env 'airj/file 'airj/json 'airj/process}
             (set (keys (sut/source-map)))))

  (it "finds only the reachable standard modules for a root AIR-J module"
    (let [module (parser/parse-module
                  "(module example/tool
                     (imports
                       (airj airj/env get)
                       (airj airj/file read-bytes-result)
                       (airj airj/json parse-result)
                       (airj airj/process run-result))
                     (export main)
                     (fn main
                       (params)
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       0))")]
      (should= #{'airj/core 'airj/env 'airj/file 'airj/json 'airj/process}
               (set (keys (sut/reachable-source-map module))))))

  (it "includes transitive standard-module dependencies in exported interfaces"
    (let [module (parser/parse-module
                  "(module example/workflow
                     (imports
                       (airj airj/bytes utf8-decode)
                       (airj airj/json parse-result)
                       (airj airj/process ProcessResult run-result))
                     (export main)
                     (fn main
                       (params)
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       0))")
          interfaces (sut/interfaces-for-module module)]
      (should (contains? interfaces 'airj/core))
      (should (contains? interfaces 'airj/bytes))
      (should (contains? interfaces 'airj/json))
      (should (contains? interfaces 'airj/process))
      (should-not (contains? interfaces 'airj/file))
      (should-not (contains? interfaces 'airj/env)))))
