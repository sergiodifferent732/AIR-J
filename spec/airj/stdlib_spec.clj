(ns airj.stdlib-spec
  (:require [airj.parser :as parser]
            [airj.stdlib :as sut]
            [speclj.core :refer :all]))

(describe "stdlib"
  (it "exposes the canonical standard modules"
    (should= #{'airj/bytes 'airj/core 'airj/env 'airj/file 'airj/json 'airj/process 'airj/test 'airj/test-runner}
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

  (it "finds the AIR-J test module as reachable when imported"
    (let [module (parser/parse-module
                  "(module example/tests
                     (imports
                       (airj airj/test TestOutcome assert-true))
                     (export passing)
                     (fn passing
                       (params)
                       (returns TestOutcome)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (call (local assert-true) \"passing\" true)))")]
      (should= #{'airj/core 'airj/test}
               (set (keys (sut/reachable-source-map module))))))

  (it "finds the AIR-J test runner as reachable when imported"
    (let [module (parser/parse-module
                  "(module example/tests
                     (imports
                       (airj airj/test TestOutcome assert-true)
                       (airj airj/test-runner summarize))
                     (export main)
                     (fn main
                       (params)
                       (returns Int)
                       (effects ())
                       (requires true)
                       (ensures true)
                       (let ((summary
                               (call (local summarize)
                                     (seq-append
                                       (seq-empty TestOutcome)
                                       (call (local assert-true) \"passing\" true)))))
                         0)))")]
      (should= #{'airj/core 'airj/test 'airj/test-runner}
               (set (keys (sut/reachable-source-map module))))))
