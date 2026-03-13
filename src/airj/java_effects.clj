(ns airj.java-effects)

(def ^:private pure-static-members
  #{['java.lang.Math 'abs [Integer/TYPE] Integer/TYPE]})

(def ^:private pure-instance-members
  #{['length [] Integer/TYPE]})

(defn- pure-static?
  [member]
  (contains? pure-static-members
             [(:class-name member)
              (:member-id member)
              (:parameter-types member)
              (:return-type member)]))

(defn- pure-instance?
  [member]
  (contains? pure-instance-members
             [(:member-id member)
              (:parameter-types member)
              (:return-type member)]))

(defn- call-effects
  [member]
  (if (pure-instance? member)
    #{}
    #{'Foreign.Throw}))

(defn- static-call-effects
  [member]
  (if (pure-static? member)
    #{}
    #{'Foreign.Throw}))

(defn- field-effects
  [member]
  (if (:writable? member)
    #{'State.Write}
    #{}))

(defn- method-effects
  [member]
  (if (:static? member)
    (static-call-effects member)
    (call-effects member)))

(defn member-effects
  [member]
  (case (:kind member)
    :constructor #{'Foreign.Throw}
    :method (method-effects member)
    :field (field-effects member)
    #{}))

(defn- write-field-effects
  [member]
  (member-effects (assoc member :writable? true)))

(def ^:private interop-effect-handlers
  {:java-new (fn [_expr _member] #{'Foreign.Throw})
   :java-call (fn [_expr member] (member-effects member))
   :java-static-call (fn [_expr member] (member-effects member))
   :java-get-field (fn [_expr _member] #{})
   :java-set-field (fn [_expr member] (write-field-effects member))
   :java-static-get-field (fn [_expr _member] #{})
   :java-static-set-field (fn [_expr member] (write-field-effects member))})

(defn interop-effects
  [expr member]
  (if-let [handler (get interop-effect-handlers (:op expr))]
    (handler expr member)
    #{}))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-13T07:11:27.319054-05:00", :module-hash "-1630466831", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 1, :hash "1558614143"} {:id "def/pure-static-members", :kind "def", :line 3, :end-line 4, :hash "1168056257"} {:id "def/pure-instance-members", :kind "def", :line 6, :end-line 7, :hash "-89144274"} {:id "defn-/pure-static?", :kind "defn-", :line 9, :end-line 15, :hash "597918340"} {:id "defn-/pure-instance?", :kind "defn-", :line 17, :end-line 22, :hash "-762772300"} {:id "defn-/call-effects", :kind "defn-", :line 24, :end-line 28, :hash "-1174490964"} {:id "defn-/static-call-effects", :kind "defn-", :line 30, :end-line 34, :hash "1040603783"} {:id "defn-/field-effects", :kind "defn-", :line 36, :end-line 40, :hash "2052571544"} {:id "defn-/method-effects", :kind "defn-", :line 42, :end-line 46, :hash "772524545"} {:id "defn/member-effects", :kind "defn", :line 48, :end-line 54, :hash "492357140"} {:id "defn-/write-field-effects", :kind "defn-", :line 56, :end-line 58, :hash "-991261598"} {:id "def/interop-effect-handlers", :kind "def", :line 60, :end-line 67, :hash "1463933422"} {:id "defn/interop-effects", :kind "defn", :line 69, :end-line 73, :hash "2053226548"}]}
;; clj-mutate-manifest-end
