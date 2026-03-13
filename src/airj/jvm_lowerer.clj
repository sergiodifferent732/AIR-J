(ns airj.jvm-lowerer
  (:require [airj.imported-interfaces :as imported-interfaces]
            [airj.java-types :as java-types]
            [airj.jvm-lowerer-expr :as expr]
            [airj.jvm-lowerer-types :as types]))

(defn- lower-field
  [field ctx]
  {:name (:name field)
   :jvm-type (types/lower-type (:type field) ctx)})

(defn- lower-record
  [ctx decl]
  {:name (:name decl)
   :class-name (types/nested-class-name (:module-name ctx) (:name decl))
   :fields (mapv #(lower-field % ctx) (:fields decl))})

(defn- lower-enum
  [ctx decl]
  {:name (:name decl)
   :class-name (types/nested-class-name (:module-name ctx) (:name decl))
   :variants (vec (:variants decl))})

(defn- lower-union-variant
  [ctx union-name variant]
  {:name (:name variant)
   :class-name (types/union-variant-class-name (:module-name ctx) union-name (:name variant))
   :fields (mapv #(lower-field % ctx) (:fields variant))})

(defn- lower-union
  [ctx decl]
  {:name (:name decl)
   :base-class (types/nested-class-name (:module-name ctx) (:name decl))
   :variants (mapv #(lower-union-variant ctx (:name decl) %) (:variants decl))})

(defn- lower-decls
  [decls op lower-fn]
  (->> decls
       (filter #(= op (:op %)))
       (mapv lower-fn)))

(defn- host-type-expr
  [module]
  (when-let [host (:host module)]
    (list 'Java (:class-name host))))

(defn- bridgeable-host-method?
  [module exports decl]
  (and (:host module)
       (contains? exports (:name decl))
       (seq (:params decl))
       (java-types/assignable-type-expr? (-> decl :params first :type)
                                         (host-type-expr module))))

(defn- lower-instance-method
  [ctx decl]
  (let [params (mapv #(expr/lower-param % ctx) (rest (:params decl)))
        target-params (mapv #(expr/lower-param % ctx) (:params decl))
        return-type (types/lower-type (:return-type decl) ctx)]
    {:name (:name decl)
     :owner (types/internal-name (:module-name ctx))
     :params params
     :return-type return-type
     :target {:name (:name decl)
              :owner (types/internal-name (:module-name ctx))
              :parameter-types (mapv :jvm-type target-params)
              :return-type return-type}}))

(defn lower-module
  [module]
  (let [decls (types/decl-map module)
        exports (set (:exports module))
        ctx {:module-name (:name module)
             :decls decls
             :imported-decls (imported-interfaces/imported-decls module)
             :closure-counter (atom 0)
             :closures (atom [])
             :closure-interfaces (atom {})}
        lower-method* (fn [decl]
                        (let [method-ctx (assoc ctx
                                           :locals (into {}
                                                         (map (fn [param]
                                                                [(:name param) (:type param)]))
                                                         (:params decl)))]
                          {:name (:name decl)
                           :owner (types/internal-name (:module-name ctx))
                           :params (mapv #(expr/lower-param % method-ctx) (:params decl))
                           :return-type (types/lower-type (:return-type decl) method-ctx)
                           :effects (vec (:effects decl))
                           :body (expr/lower-expr (:body decl) method-ctx)}))]
    (cond-> {:op :jvm-module
             :module-name (:name module)
             :internal-name (types/internal-name (:name module))
             :exports (vec (:exports module))
             :records (lower-decls (:decls module) :data #(lower-record ctx %))
             :enums (lower-decls (:decls module) :enum #(lower-enum ctx %))
             :unions (lower-decls (:decls module) :union #(lower-union ctx %))
             :methods (lower-decls (:decls module) :fn lower-method*)}
      (:host module)
      (assoc :host {:class-name (types/java-internal-name (host-type-expr module))})
      (seq (filter #(bridgeable-host-method? module exports %) (:decls module)))
      (assoc :instance-methods (->> (:decls module)
                                    (filter #(and (= :fn (:op %))
                                                  (bridgeable-host-method? module exports %)))
                                    (mapv #(lower-instance-method ctx %))))
      (seq @(:closure-interfaces ctx))
      (assoc :closure-interfaces (->> @(:closure-interfaces ctx) vals (sort-by :class-name) vec))
      (seq @(:closures ctx))
      (assoc :closures (vec @(:closures ctx))))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T16:12:02.798193-05:00", :module-hash "946550683", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "-580314099"} {:id "defn-/lower-field", :kind "defn-", :line 7, :end-line 10, :hash "-959639482"} {:id "defn-/lower-record", :kind "defn-", :line 12, :end-line 16, :hash "-958206154"} {:id "defn-/lower-enum", :kind "defn-", :line 18, :end-line 22, :hash "507139670"} {:id "defn-/lower-union-variant", :kind "defn-", :line 24, :end-line 28, :hash "-261619844"} {:id "defn-/lower-union", :kind "defn-", :line 30, :end-line 34, :hash "2049467452"} {:id "defn-/lower-decls", :kind "defn-", :line 36, :end-line 40, :hash "533670331"} {:id "defn-/host-type-expr", :kind "defn-", :line 42, :end-line 45, :hash "-398773626"} {:id "defn-/bridgeable-host-method?", :kind "defn-", :line 47, :end-line 53, :hash "-1014172909"} {:id "defn-/lower-instance-method", :kind "defn-", :line 55, :end-line 67, :hash "-1618807735"} {:id "defn/lower-module", :kind "defn", :line 69, :end-line 109, :hash "400967000"}]}
;; clj-mutate-manifest-end
