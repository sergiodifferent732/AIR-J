(ns airj.exhaustiveness-checker-spec
  (:require [airj.exhaustiveness-checker :as sut]
            [speclj.core :refer :all]))

(describe "check-module"
  (it "accepts exhaustive boolean matches"
    (let [module {:name 'example/bool-match
                  :imports []
                  :exports ['describe]
                  :decls [{:op :fn
                           :name 'describe
                           :params [{:name 'flag :type 'Bool}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :local :name 'flag}
                                  :cases [{:pattern {:op :literal-pattern
                                                     :literal true}
                                           :body 1}
                                          {:pattern {:op :literal-pattern
                                                     :literal false}
                                           :body 0}]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts exhaustive enum matches"
    (let [module {:name 'example/enum-match
                  :imports []
                  :exports ['describe]
                  :decls [{:op :enum
                           :name 'Color
                           :variants ['Red 'Blue]}
                          {:op :fn
                           :name 'describe
                           :params [{:name 'color :type 'Color}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :local :name 'color}
                                  :cases [{:pattern {:op :binder-pattern
                                                     :name 'Red}
                                           :body 1}
                                          {:pattern {:op :binder-pattern
                                                     :name 'Blue}
                                           :body 2}]}}]}]
      (should= module (sut/check-module module))))

  (it "rejects non-exhaustive union matches"
    (should-throw clojure.lang.ExceptionInfo
                  "Non-exhaustive match."
                  (sut/check-module
                   {:name 'example/non-exhaustive-union
                    :imports []
                    :exports ['render]
                    :decls [{:op :union
                             :name 'Response
                             :type-params []
                             :invariants []
                             :variants [{:name 'Ok
                                         :fields [{:name 'value :type 'Int}]}
                                        {:name 'Error
                                         :fields [{:name 'message :type 'String}]}]}
                            {:op :fn
                             :name 'render
                             :params [{:name 'response :type 'Response}]
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :match
                                    :target {:op :local :name 'response}
                                    :cases [{:pattern {:op :union-pattern
                                                       :name 'Ok
                                                       :args [{:op :binder-pattern
                                                               :name 'value}]}
                                             :body {:op :local :name 'value}}]}}]})))

  (it "checks nested matches inside let bodies"
    (should-throw clojure.lang.ExceptionInfo
                  "Non-exhaustive match."
                  (sut/check-module
                   {:name 'example/nested-match
                    :imports []
                    :exports ['describe]
                    :decls [{:op :enum
                             :name 'Color
                             :variants ['Red 'Blue]}
                            {:op :fn
                             :name 'describe
                             :params [{:name 'color :type 'Color}]
                             :return-type 'Int
                             :effects []
                             :requires [true]
                             :ensures [true]
                             :body {:op :let
                                    :bindings [{:name 'current
                                                :expr {:op :local :name 'color}}]
                                    :body {:op :match
                                           :target {:op :local :name 'current}
                                           :cases [{:pattern {:op :binder-pattern
                                                              :name 'Red}
                                                    :body 1}]}}}]})))

  (it "accepts exhaustive union matches and traverses bound case bodies"
    (let [module {:name 'example/union-match
                  :imports []
                  :exports ['render]
                  :decls [{:op :union
                           :name 'Response
                           :type-params []
                           :invariants []
                           :variants [{:name 'Ok
                                       :fields [{:name 'value :type 'Int}]}
                                      {:name 'Error
                                       :fields [{:name 'message :type 'String}]}]}
                          {:op :fn
                           :name 'render
                           :params [{:name 'response :type 'Response}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :local :name 'response}
                                  :cases [{:pattern {:op :union-pattern
                                                     :name 'Ok
                                                     :args [{:op :binder-pattern
                                                             :name 'value}]}
                                           :body {:op :local :name 'value}}
                                          {:pattern {:op :union-pattern
                                                     :name 'Error
                                                     :args [{:op :binder-pattern
                                                             :name 'message}]}
                                           :body 0}]}}]}]
      (should= module (sut/check-module module))))

  (it "traverses record-pattern field bindings inside match cases"
    (let [module {:name 'example/record-match
                  :imports []
                  :exports ['status]
                  :decls [{:op :data
                           :name 'Response
                           :type-params []
                           :invariants []
                           :fields [{:name 'status :type 'Int}
                                    {:name 'body :type 'String}]}
                          {:op :fn
                           :name 'status
                           :params [{:name 'response :type 'Response}]
                           :return-type 'Int
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :local :name 'response}
                                  :cases [{:pattern {:op :record-pattern
                                                     :type 'Response
                                                     :fields [{:name 'status
                                                               :pattern {:op :binder-pattern
                                                                         :name 'code}}]}
                                           :body {:op :local :name 'code}}]}}]}]
      (should= module (sut/check-module module))))

  (it "accepts matches over imported generic union types"
    (let [module {:name 'example/imported-option-match
                  :imports [{:op :airj-import
                             :module 'airj/core
                             :symbols ['Interchange]}]
                  :interfaces {'airj/core {:name 'airj/core
                                           :imports []
                                           :exports ['Option 'None 'Some 'Interchange]
                                           :decls [{:op :union
                                                    :name 'Option
                                                    :type-params ['T]
                                                    :invariants []
                                                    :variants [{:name 'None
                                                                :fields []}
                                                               {:name 'Some
                                                                :fields [{:name 'value
                                                                          :type 'T}]}]}
                                                   {:op :union
                                                    :name 'Interchange
                                                    :type-params []
                                                    :invariants []
                                                    :variants [{:name 'StringValue
                                                                :fields [{:name 'value
                                                                          :type 'String}]}
                                                               {:name 'MapValue
                                                                :fields [{:name 'entries
                                                                          :type '(Map String Interchange)}]}]}]}}
                  :exports ['read]
                  :decls [{:op :fn
                           :name 'lookup
                           :params []
                           :return-type '(Option Interchange)
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :variant
                                  :type '(Option Interchange)
                                  :name 'None
                                  :args []}}
                          {:op :fn
                           :name 'read
                           :params []
                           :return-type 'String
                           :effects []
                           :requires [true]
                           :ensures [true]
                           :body {:op :match
                                  :target {:op :call
                                           :callee {:op :local :name 'lookup}
                                           :args []}
                                  :cases [{:pattern {:op :union-pattern
                                                     :name 'Some
                                                     :args [{:op :binder-pattern
                                                             :name 'value}]}
                                           :body {:op :match
                                                  :target {:op :local :name 'value}
                                                  :cases [{:pattern {:op :union-pattern
                                                                     :name 'StringValue
                                                                     :args [{:op :binder-pattern
                                                                             :name 'text}]}
                                                           :body {:op :local :name 'text}}
                                                          {:pattern {:op :union-pattern
                                                                     :name 'MapValue
                                                                     :args [{:op :wildcard-pattern}]}
                                                           :body ""}]}}
                                          {:pattern {:op :union-pattern
                                                     :name 'None
                                                     :args []}
                                           :body ""}]}}]}]
      (should= module (sut/check-module module)))))
