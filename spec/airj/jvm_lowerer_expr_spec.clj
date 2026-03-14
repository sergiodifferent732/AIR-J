(ns airj.jvm-lowerer-expr-spec
  (:require [airj.jvm-lowerer-expr :as sut]
            [speclj.core :refer :all]))

(describe "JVM lowerer expr"
  (it "lowers Float and Double literals"
    (should= {:op :jvm-float
              :value (float 1.25)
              :jvm-type :float}
             (sut/lower-expr (float 1.25) {}))
    (should= {:op :jvm-double
              :value 1.25
              :jvm-type :double}
             (sut/lower-expr 1.25 {})))

  (it "lowers direct imported function calls"
    (let [ctx {:module-name 'example/use
               :decls {}
               :imported-decls {'tick {:module 'alpha/math
                                       :decl {:op :fn
                                              :name 'tick
                                              :params [{:name 'x :type 'Int}]
                                              :return-type 'Int
                                              :effects ['Clock.Read]}}}
               :locals {}
               :mutable-locals {}
               :lambdas {}}
          expr {:op :call
                :callee {:op :local :name 'tick}
                :args [1]}]
      (should= {:op :jvm-invoke-static
                :owner "alpha/math"
                :name 'tick
                :parameter-types [:int]
                :return-type :int
                :args [{:op :jvm-int
                        :value 1
                        :jvm-type :int}]
                :jvm-type :int}
               (sut/lower-expr expr ctx)))))
