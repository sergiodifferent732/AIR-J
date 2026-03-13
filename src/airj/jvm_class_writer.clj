(ns airj.jvm-class-writer
  (:import (clojure.asm ClassWriter)))

(defn- class-hierarchy
  [module]
  (merge {(:internal-name module) (or (get-in module [:host :class-name])
                                      "java/lang/Object")}
         (into {}
               (map (fn [record]
                      [(:class-name record) "java/lang/Object"]))
               (:records module))
         (into {}
               (mapcat (fn [union]
                         (concat [[(:base-class union) "java/lang/Object"]]
                                 (map (fn [variant]
                                        [(:class-name variant) (:base-class union)])
                                      (:variants union))))
                       (:unions module)))))

(defn- ancestor-chain
  [hierarchy class-name]
  (loop [current class-name
         ancestors []]
    (if current
      (recur (get hierarchy current)
             (conj ancestors current))
      ancestors)))

(defn- common-super-class
  [hierarchy left right]
  (cond
    (= left right) left
    (= left "java/lang/Object") left
    (= right "java/lang/Object") right
    :else (let [left-ancestors (set (ancestor-chain hierarchy left))]
            (or (some left-ancestors (ancestor-chain hierarchy right))
                "java/lang/Object"))))

(defn class-writer
  [module]
  (let [hierarchy (class-hierarchy module)]
    (proxy [ClassWriter] [(+ ClassWriter/COMPUTE_FRAMES
                             ClassWriter/COMPUTE_MAXS)]
      (getCommonSuperClass [left right]
        (common-super-class hierarchy left right)))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T15:57:40.959496-05:00", :module-hash "-3827559", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-129056526"} {:id "defn-/class-hierarchy", :kind "defn-", :line 4, :end-line 18, :hash "1296911537"} {:id "defn-/ancestor-chain", :kind "defn-", :line 20, :end-line 27, :hash "-114668316"} {:id "defn-/common-super-class", :kind "defn-", :line 29, :end-line 37, :hash "212174441"} {:id "defn/class-writer", :kind "defn", :line 39, :end-line 45, :hash "1930453079"}]}
;; clj-mutate-manifest-end
