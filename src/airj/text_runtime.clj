(ns airj.text-runtime
  (:refer-clojure :exclude [read-line]))

(defonce stdin-state (atom nil))

(defn reset-stdin!
  []
  (reset! stdin-state nil))

(defn- scanner-for-current-stdin
  []
  (if-some [scanner @stdin-state]
    scanner
    (let [scanner (java.util.Scanner. System/in)]
      (reset! stdin-state scanner)
      scanner)))

(defn read-line
  []
  (.nextLine ^java.util.Scanner (scanner-for-current-stdin)))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-15T10:28:18.156947-05:00", :module-hash "-13988607", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "-2007510925"} {:id "form/1/defonce", :kind "defonce", :line 4, :end-line 4, :hash "1733468777"} {:id "defn/reset-stdin!", :kind "defn", :line 6, :end-line 8, :hash "1785696049"} {:id "defn-/scanner-for-current-stdin", :kind "defn-", :line 10, :end-line 16, :hash "-1283483984"} {:id "defn/read-line", :kind "defn", :line 18, :end-line 20, :hash "-1036289507"}]}
;; clj-mutate-manifest-end
