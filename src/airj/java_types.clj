(ns airj.java-types)

(def ^:private primitive-types
  {'Bool Boolean/TYPE
   'Float Float/TYPE
   'Double Double/TYPE
   'Int Integer/TYPE
   'Unit Void/TYPE})

(defn load-class
  [class-name]
  (Class/forName (str class-name)))

(def ^:private named-reference-types
  {'String java.lang.String
   'StringSeq (load-class "[Ljava.lang.String;")})

(defn java-type-expr?
  [type-expr]
  (and (seq? type-expr)
       (= "Java" (name (first type-expr)))))

(defn- seq-type?
  [type-expr]
  (and (seq? type-expr)
       (= 'Seq (first type-expr))))

(defn- map-type?
  [type-expr]
  (and (seq? type-expr)
       (= 'Map (first type-expr))))

(defn- special-reference-type
  [type-expr]
  (or (get named-reference-types type-expr)
      (when (seq-type? type-expr)
        java.util.List)
      (when (map-type? type-expr)
        java.util.Map)))

(defn resolve-type
  [type-expr]
  (cond
    (contains? primitive-types type-expr) (get primitive-types type-expr)
    (special-reference-type type-expr) (special-reference-type type-expr)
    (java-type-expr? type-expr) (load-class (second type-expr))
    :else nil))

(defn assignable-type-expr?
  [expected-type actual-type]
  (let [expected (resolve-type expected-type)
        actual (resolve-type actual-type)]
    (cond
      (or (nil? expected) (nil? actual)) (= expected-type actual-type)
      (or (.isPrimitive expected) (.isPrimitive actual)) (= expected actual)
      :else (.isAssignableFrom expected actual))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T08:33:50.39138-05:00", :module-hash "1852755138", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "-220394871"} {:id "def/primitive-types", :kind "def", :line 3, :end-line 8, :hash "49647705"} {:id "defn/load-class", :kind "defn", :line 10, :end-line 12, :hash "1222057517"} {:id "def/named-reference-types", :kind "def", :line 14, :end-line 16, :hash "2127171633"} {:id "defn/java-type-expr?", :kind "defn", :line 18, :end-line 21, :hash "-2087947263"} {:id "defn-/seq-type?", :kind "defn-", :line 23, :end-line 26, :hash "1535070684"} {:id "defn-/map-type?", :kind "defn-", :line 28, :end-line 31, :hash "-1201869236"} {:id "defn-/special-reference-type", :kind "defn-", :line 33, :end-line 39, :hash "2072413056"} {:id "defn/resolve-type", :kind "defn", :line 41, :end-line 47, :hash "-1708781742"} {:id "defn/assignable-type-expr?", :kind "defn", :line 49, :end-line 56, :hash "648140632"}]}
;; clj-mutate-manifest-end
