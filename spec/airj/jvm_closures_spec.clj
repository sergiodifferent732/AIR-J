(ns airj.jvm-closures-spec
  (:require [airj.jvm-closures :as sut]
            [speclj.core :refer :all]))

(describe "type-token"
  (it "normalizes JVM reference and array type names for closure interfaces"
    (let [fail! (fn [message data]
                  (throw (ex-info message data)))]
      (should= "java_lang_String"
               (sut/type-token "java/lang/String;" fail!))
      (should= "Arr_I"
               (sut/type-token "[I" fail!))))

  (it "rejects unsupported JVM types"
    (let [fail! (fn [message data]
                  (throw (ex-info message data)))]
      (should-throw clojure.lang.ExceptionInfo
                    "Unsupported JVM type token."
                    (sut/type-token nil fail!)))))

(describe "free-locals"
  (it "collects free locals in first-use order"
    (should= ['base 'y]
             (sut/free-locals
              {:op :if
               :test {:op :local :name 'base}
               :then {:op :call
                      :callee {:op :local :name 'f}
                      :args [{:op :local :name 'y}
                             {:op :local :name 'base}]}
               :else 0}
              #{'f})))

  (it "excludes names bound by let"
    (should= ['outside]
             (sut/free-locals
              {:op :let
               :bindings [{:name 'inside
                           :expr {:op :local :name 'outside}}]
               :body {:op :if
                      :test true
                      :then {:op :local :name 'inside}
                      :else 0}}
              #{})))

  (it "does not descend into nested lambdas"
    (should= []
             (sut/free-locals
              {:op :lambda
               :params [{:name 'x :type 'Int}]
               :return-type 'Int
               :effects []
               :body {:op :local :name 'outside}}
              #{})))

  (it "excludes names bound by loop bindings"
    (should= ['outside]
             (sut/free-locals
              {:op :loop
               :bindings [{:name 'inside
                           :expr {:op :local :name 'outside}}]
               :body {:op :recur
                      :args [{:op :local :name 'inside}]}}
              #{})))

  (it "collects free locals from try, catches, and finally in order"
    (should= ['body-ref 'catch-ref 'finally-ref]
             (sut/free-locals
              {:op :try
               :body {:op :local :name 'body-ref}
               :catches [{:name 'ex
                          :type '(Java java.lang.Exception)
                          :body {:op :local :name 'catch-ref}}]
               :finally {:op :local :name 'finally-ref}}
              #{})))

  (it "collects free locals from static field writes"
    (should= ['replacement]
             (sut/free-locals
              {:op :java-static-set-field
               :class-name 'java.lang.System
               :field-name 'out
               :field-type '(Java java.io.PrintStream)
               :expr {:op :local :name 'replacement}}
              #{})))

  (it "returns no free locals for unknown ops"
    (should= []
             (sut/free-locals
              {:op :unknown}
              #{}))))
