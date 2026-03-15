(ns airj.project-sources-spec
  (:require [airj.project-sources :as sut]
            [speclj.core :refer :all]))

(describe "project-sources"
  (it "derives imported interfaces for one root module from sibling AIR-J sources"
    (let [project-sources {'alpha/math "(module alpha/math
                                          (imports)
                                          (export Counter tick)
                                          (data Counter
                                            (invariants true)
                                            (field value Int))
                                          (fn tick
                                            (params (x Int))
                                            (returns Int)
                                            (effects (Clock.Read))
                                            (requires true)
                                            (ensures true)
                                            (local x)))"
                           'example/use "(module example/use
                                           (imports
                                             (airj alpha/math tick))
                                           (export program)
                                           (fn program
                                             (params)
                                             (returns Int)
                                             (effects (Clock.Read))
                                             (requires true)
                                             (ensures true)
                                             (call (local tick) 1)))"}]
      (should= {'alpha/math {:name 'alpha/math
                             :imports []
                             :exports ['Counter 'tick]
                             :decls [{:op :data
                                      :name 'Counter
                                      :type-params []
                                      :invariants [true]
                                      :fields [{:name 'value
                                                :type 'Int}]}
                                     {:op :fn
                                      :name 'tick
                                      :params [{:name 'x :type 'Int}]
                                      :return-type 'Int
                                      :effects ['Clock.Read]}]}}
               (sut/imported-interfaces project-sources 'example/use))))

  (it "selects only the root module and its reachable AIR-J imports"
    (let [project-sources {'alpha/math "(module alpha/math
                                          (imports)
                                          (export tick)
                                          (fn tick
                                            (params (x Int))
                                            (returns Int)
                                            (effects ())
                                            (requires true)
                                            (ensures true)
                                            (local x)))"
                           'example/use "(module example/use
                                           (imports
                                             (airj alpha/math tick))
                                           (export main)
                                           (fn main
                                             (params)
                                             (returns Int)
                                             (effects ())
                                             (requires true)
                                             (ensures true)
                                             (call (local tick) 1)))"
                           'broken/unused "(module broken/unused
                                             (imports)
                                             (export main)
                                             (fn main
                                               (params)
                                               (returns Int)
                                               (effects ())
                                               (requires true)
                                               (ensures true)
                                               (local nope)))"}]
      (should= #{'alpha/math 'example/use}
               (set (keys (sut/reachable-source-map project-sources 'example/use))))))

  (it "adds reachable standard modules to the compilation source map"
    (let [project-sources {'example/tool "(module example/tool
                                           (imports
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
                                             0))"}]
      (should= #{'airj/core 'airj/file 'airj/json 'airj/process 'example/tool}
               (set (keys (sut/compilation-source-map project-sources 'example/tool)))))))
