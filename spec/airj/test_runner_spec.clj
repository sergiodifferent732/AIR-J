(ns airj.test-runner-spec
  (:require [airj.test-runner :as sut]
            [speclj.core :refer :all]))

(describe "test runner"
  (it "reports an uncaught canonical test suite exception as one test error"
    (let [source "(module example/tests
                    (imports
                      (airj airj/test TestOutcome assert-true assert-false)
                      (airj airj/test-runner run))
                    (export tests main)
                    (fn tests
                      (params)
                      (returns (Seq TestOutcome))
                      (effects (Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (seq-concat
                        (seq-append
                          (seq-append
                            (seq-empty TestOutcome)
                            (call (local assert-true) \"passing\" true))
                          (call (local assert-false) \"failing\" true))
                        (seq-append
                          (seq-empty TestOutcome)
                          (seq
                            (string->int \"boom\")
                            (call (local assert-true) \"exploding\" true)))))
                    (fn main
                      (params (args StringSeq))
                      (returns Int)
                      (effects (Stdout.Write Foreign.Throw))
                      (requires true)
                      (ensures true)
                      (call (local run) (call (local tests)))) )"
          summary (sut/run-source-tests! source)]
      (should= "example/tests" (:module summary))
      (should= 0 (:passed summary))
      (should= 0 (:failed summary))
      (should= 1 (:errored summary))
      (should= [{:status "error"
                 :name "tests"
                 :diagnostic {:phase "runtime"
                              :message "Execution error."
                              :detail "java.lang.NumberFormatException: For input string: \"boom\""}}]
               (:outcomes summary))))

  (it "runs canonical AIR-J test suites from project sources"
    (let [module-sources
          {'alpha/math "(module alpha/math
                          (imports)
                          (export tick)
                          (fn tick
                            (params (x Int))
                            (returns Int)
                            (effects ())
                            (requires true)
                            (ensures true)
                            (local x)))"
           'example/tests "(module example/tests
                             (imports
                               (airj alpha/math tick)
                               (airj airj/test TestOutcome assert-int-eq)
                               (airj airj/test-runner run))
                             (export tests main)
                             (fn tests
                               (params)
                               (returns (Seq TestOutcome))
                               (effects ())
                               (requires true)
                               (ensures true)
                               (seq-append
                                 (seq-empty TestOutcome)
                                 (call (local assert-int-eq)
                                       \"tick-test\"
                                       (call (local tick) 7)
                                       7)))
                             (fn main
                               (params (args StringSeq))
                               (returns Int)
                               (effects (Stdout.Write))
                               (requires true)
                               (ensures true)
                               (call (local run) (call (local tests)))))"}
          summary (sut/run-project-source-tests! module-sources 'example/tests)]
      (should= "example/tests" (:module summary))
      (should= 1 (:passed summary))
      (should= 0 (:failed summary))
      (should= 0 (:errored summary))
      (should= [{:status "pass"
                 :name "tick-test"}]
               (:outcomes summary))))

  (it "classifies assertion mismatches as failures rather than errors"
    (let [source "(module example/tests
                    (imports
                      (airj airj/test TestOutcome assert-false)
                      (airj airj/test-runner run))
                    (export tests main)
                    (fn tests
                      (params)
                      (returns (Seq TestOutcome))
                      (effects ())
                      (requires true)
                      (ensures true)
                      (seq-append
                        (seq-empty TestOutcome)
                        (call (local assert-false) \"failing\" true)))
                    (fn main
                      (params (args StringSeq))
                      (returns Int)
                      (effects (Stdout.Write))
                      (requires true)
                      (ensures true)
                      (call (local run) (call (local tests)))))"
          summary (sut/run-source-tests! source)]
      (should= 0 (:passed summary))
      (should= 1 (:failed summary))
      (should= 0 (:errored summary))
      (should= [{:status "fail"
                 :name "failing"
                 :diagnostic {:phase "test"
                              :message "Expected false."
                              :detail "Assertion evaluated to true."}}]
               (:outcomes summary))))

  (it "rejects non-canonical test modules without exported tests and main"
    (let [source "(module example/tests
                    (imports
                      (airj airj/test TestOutcome assert-true))
                    (export passing)
                    (fn passing
                      (params)
                      (returns TestOutcome)
                      (effects ())
                      (requires true)
                      (ensures true)
                      (call (local assert-true) \"passing\" true)))"]
      (should-throw clojure.lang.ExceptionInfo
                    "Malformed AIR-J test module."
                    (sut/run-source-tests! source)))))
