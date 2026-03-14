(ns airj.jvm-lowerer-types-spec
  (:require [airj.jvm-lowerer-types :as sut]
            [speclj.core :refer :all]))

(describe "JVM lowerer types"
  (it "lowers AIR-J and declared JVM types"
    (let [ctx {:module-name 'example/types
               :decls {'Result {:op :union
                                :name 'Result
                                :type-params ['Ok 'Err]
                                :variants []}}}]
      (should= :int
               (sut/lower-type 'Int ctx))
      (should= "java/lang/String"
               (sut/lower-type 'String ctx))
      (should= "java/lang/StringBuilder"
               (sut/lower-type '(Java java.lang.StringBuilder) ctx))
      (should= "example/types$Result"
               (sut/lower-type 'Result ctx))
      (should= "example/types$Result"
               (sut/lower-type '(Result Int String) ctx))))

  (it "instantiates parameterized union fields from type arguments"
    (let [decl {:op :union
                :name 'Result
                :type-params ['Ok 'Err]
                :variants [{:name 'Ok
                            :fields [{:name 'value
                                      :type 'Ok}]}
                           {:name 'Err
                            :fields [{:name 'error
                                      :type 'Err}]}]}]
      (should= 'Int
               (sut/field-type {:name 'Box
                                :type-params ['T]
                                :fields [{:name 'value
                                          :type 'T}]}
                               'value
                               '(Box Int)))
      (should= [{:name 'value
                 :type 'Int}]
               (:fields (sut/union-variant {:decls {'Result decl}}
                                           '(Result Int String)
                                           'Ok)))))

  (it "instantiates lowered type parameters through symbols lists and literals"
    (let [instantiate-type (ns-resolve 'airj.jvm-lowerer-types 'instantiate-type)]
      (should= 'Int
               (instantiate-type 'T {'T 'Int}))
      (should= '(Result Int String)
               (instantiate-type '(Result T E) {'T 'Int
                                                'E 'String}))
      (should= :int
               (instantiate-type :int {'T 'Int}))))

  (it "infers call types from imported function declarations"
    (let [ctx {:module-name 'example/use
               :decls {}
               :imported-decls {'tick {:module 'alpha/math
                                       :decl {:op :fn
                                              :name 'tick
                                              :params [{:name 'x :type 'Int}]
                                              :return-type 'Int
                                              :effects ['Clock.Read]}}}
               :locals {}}
          expr {:op :call
                :callee {:op :local :name 'tick}
                :args [1]}]
      (should= 'Int
               (sut/infer-type expr ctx))))

  (it "infers Float and Double literals"
    (let [ctx {:module-name 'example/literals
               :decls {}
               :locals {}}]
      (should= 'Float
               (sut/infer-type (float 1.25) ctx))
      (should= 'Double
               (sut/infer-type 1.25 ctx))))

  (it "rejects unsupported JVM types"
    (should-throw clojure.lang.ExceptionInfo
                  "Unsupported JVM type."
                  (sut/lower-type 'MissingType)))

  (it "rejects unknown lowered locals"
    (should-throw clojure.lang.ExceptionInfo
                  "Unknown lowered local."
                  (sut/local-type {:locals {}} 'missing)))

  (it "rejects unknown lowered fields"
    (should-throw clojure.lang.ExceptionInfo
                  "Unknown lowered field."
                  (sut/field-type {:name 'Response
                                   :fields [{:name 'status
                                             :type 'Int}]}
                                  'missing)))

  (it "rejects mismatched lowered branch types"
    (should-throw clojure.lang.ExceptionInfo
                  "Lowered branch types must agree."
                  (sut/join-branch-types :int
                                         :boolean
                                         {:expr :if}))))
