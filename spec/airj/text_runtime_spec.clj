(ns airj.text-runtime-spec
  (:require [airj.text-runtime :as sut]
            [speclj.core :refer :all]))

(describe "text runtime"
  (around [it]
    (let [original-in (System/in)]
      (sut/reset-stdin!)
      (try
        (it)
        (finally
          (sut/reset-stdin!)
          (System/setIn original-in)))))

  (it "reads multiple lines from the same stdin stream"
    (System/setIn (java.io.ByteArrayInputStream. (.getBytes "alpha\nbeta\n" "UTF-8")))
    (should= "alpha" (sut/read-line))
    (should= "beta" (sut/read-line)))

  (it "refreshes the scanner when reset after System/in changes"
    (System/setIn (java.io.ByteArrayInputStream. (.getBytes "left\n" "UTF-8")))
    (should= "left" (sut/read-line))
    (System/setIn (java.io.ByteArrayInputStream. (.getBytes "right\n" "UTF-8")))
    (sut/reset-stdin!)
    (should= "right" (sut/read-line))))
