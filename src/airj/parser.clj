(ns airj.parser
  (:require [clojure.edn :as edn]
            [clojure.string :as str])
  (:import (clojure.lang LineNumberingPushbackReader)
           (java.io StringReader)))

(declare parse-expr)
(declare parse-pattern)

(defn- fail!
  [message data]
  (throw (ex-info message (assoc data :phase :parse))))

(defn- reader-location
  [data]
  (cond-> {}
    (:line data) (assoc :line (:line data))
    (:column data) (assoc :column (:column data))))

(defn- reader-diagnostic
  [message data]
  (let [text (or message "")
        location (reader-location data)]
    (cond
      (str/includes? text "EOF while reading")
      (fail! "Reader error: missing closing delimiter."
             (merge {:detail "Expected a closing ')' before end of source."}
                    location))

      (str/includes? text "Unmatched delimiter")
      (fail! "Reader error: unexpected closing delimiter."
             (merge {:detail "Found a ')' that does not match any open form."}
                    location))

      :else
      (fail! "Reader error."
             (merge {:detail text}
                    location)))))

(defn- read-source-form
  [source]
  (with-open [reader (LineNumberingPushbackReader. (StringReader. source))]
    (try
      (edn/read {:eof ::eof} reader)
      (catch clojure.lang.ExceptionInfo e
        (reader-diagnostic (.getMessage e) (ex-data e)))
      (catch RuntimeException e
        (reader-diagnostic (.getMessage e) (ex-data e))))))

(defn- expect-seq-form
  [form context]
  (when-not (sequential? form)
    (fail! "Malformed AIR-J form."
           {:context context
            :detail "Expected a parenthesized form."
            :form form})))

(defn- expect-count
  [form expected context]
  (when-not (= expected (count form))
    (fail! "Malformed AIR-J form."
           {:context context
            :detail (str "Expected " expected " elements.")
            :form form})))

(defn- expect-at-least
  [form minimum context]
  (when (< (count form) minimum)
    (fail! "Malformed AIR-J form."
           {:context context
            :detail (str "Expected at least " minimum " elements.")
            :form form})))

(defn- parse-import
  [[tag target & symbols :as form]]
  (case tag
    airj {:op :airj-import
          :module target
          :symbols (vec symbols)}
    java (do
           (when (seq symbols)
             (fail! "Unexpected java import form."
                    {:form form}))
           {:op :java-import
            :class-name target})
    (fail! "Unsupported import."
           {:form form})))

(defn- expect-tag
  [expected form]
  (expect-seq-form form expected)
  (when-not (= expected (first form))
    (fail! "Unexpected form tag."
           {:expected expected
            :actual (first form)
            :form form})))

(defn- parse-imports
  [form]
  (expect-tag 'imports form)
  (mapv parse-import (rest form)))

(defn- parse-exports
  [form]
  (expect-tag 'export form)
  (vec (rest form)))

(defn- parse-host
  [forms]
  (if (and (seq forms)
           (= 'host (ffirst forms)))
    (do
      (expect-count (first forms) 2 'host)
      [{:class-name (second (first forms))} (rest forms)])
    [nil forms]))

(defn- parse-param
  [[name type-expr]]
  {:name name
   :type type-expr})

(defn- parse-field
  [[tag name type-expr]]
  (expect-tag 'field [tag])
  {:name name
   :type type-expr})

(defn- parse-effects
  [form]
  (expect-tag 'effects form)
  (let [entries (rest form)]
    (cond
      (and (= 1 (count entries))
           (sequential? (first entries))
           (empty? (first entries)))
      []

      (and (= 1 (count entries))
           (sequential? (first entries)))
      (vec (first entries))

      :else
      (vec entries))))

(defn- parse-contract-clause
  [tag form]
  (expect-tag tag form)
  (vec (rest form)))

(defn- parse-invariants
  [forms]
  (if (and (seq forms)
           (= 'invariants (ffirst forms)))
    [(mapv parse-expr (rest (first forms))) (rest forms)]
    [[] forms]))

(defn- parse-type-params
  [forms]
  (if (and (seq forms)
           (= 'type-params (ffirst forms)))
    [(vec (rest (first forms))) (rest forms)]
    [[] forms]))

(defn- parse-data-decl
  [[_ name & forms]]
  (let [[type-params forms] (parse-type-params forms)
        [invariants forms] (parse-invariants forms)]
    {:op :data
     :name name
     :type-params type-params
     :invariants invariants
     :fields (mapv parse-field forms)}))

(defn- parse-enum-decl
  [[_ name & variants]]
  {:op :enum
   :name name
   :variants (vec variants)})

(defn- parse-variant-decl
  [[tag name & forms]]
  (expect-tag 'variant [tag])
  {:name name
   :fields (mapv parse-field forms)})

(defn- parse-case
  [[tag pattern body]]
  (expect-tag 'case [tag])
  {:pattern (parse-pattern pattern)
   :body (parse-expr body)})

(defn- parse-call-expr
  [form]
  {:op :call
   :callee (parse-expr (second form))
   :args (mapv parse-expr (drop 2 form))})

(defn- parse-construct-expr
  [form]
  {:op :construct
   :type (second form)
   :args (mapv parse-expr (drop 2 form))})

(defn- parse-variant-expr
  [form]
  {:op :variant
   :type (second form)
   :name (nth form 2)
   :args (mapv parse-expr (drop 3 form))})

(defn- parse-record-get-expr
  [form]
  {:op :record-get
   :target (parse-expr (second form))
   :field (nth form 2)})

(defn- parse-if-expr
  [form]
  {:op :if
   :test (parse-expr (second form))
   :then (parse-expr (nth form 2))
   :else (parse-expr (nth form 3))})

(defn- parse-match-expr
  [form]
  {:op :match
   :target (parse-expr (second form))
   :cases (mapv parse-case (drop 2 form))})

(defn- parse-binding
  [[name expr]]
  {:name name
   :expr (parse-expr expr)})

(defn- parse-let-expr
  [form]
  {:op :let
   :bindings (mapv parse-binding (second form))
   :body (parse-expr (nth form 2))})

(defn- parse-seq-expr
  [form]
  {:op :seq
   :exprs (mapv parse-expr (rest form))})

(defn- parse-lambda-expr
  [[_ params-form returns-form effects-form body-form]]
  (expect-tag 'params params-form)
  (expect-tag 'returns returns-form)
  {:op :lambda
   :params (mapv parse-param (rest params-form))
   :return-type (second returns-form)
   :effects (parse-effects effects-form)
   :body (parse-expr body-form)})

(defn- parse-catch-clause
  [[tag type-expr name body]]
  (expect-tag 'catch [tag])
  {:type type-expr
   :name name
   :body (parse-expr body)})

(defn- parse-finally-clause
  [[tag expr]]
  (expect-tag 'finally [tag])
  (parse-expr expr))

(defn- parse-try-expr
  [form]
  (let [parts (drop 2 form)
        catches (filter #(and (seq? %) (= 'catch (first %))) parts)
        finally-form (first (filter #(and (seq? %) (= 'finally (first %))) parts))]
    {:op :try
     :body (parse-expr (second form))
     :catches (mapv parse-catch-clause catches)
     :finally (when finally-form
                (parse-finally-clause finally-form))}))

(defn- parse-var-expr
  [[_ name type-expr init]]
  {:op :var
   :name name
   :type type-expr
   :init (parse-expr init)})

(defn- parse-set-expr
  [[_ name expr]]
  {:op :set
   :name name
   :expr (parse-expr expr)})

(defn- parse-loop-expr
  [form]
  {:op :loop
   :bindings (mapv parse-binding (second form))
   :body (parse-expr (nth form 2))})

(defn- parse-recur-expr
  [form]
  {:op :recur
   :args (mapv parse-expr (rest form))})

(defn- parse-raise-expr
  [form]
  {:op :raise
   :expr (parse-expr (second form))})

(defn- parse-primitive-unary-expr
  [op form]
  {:op op
   :arg (parse-expr (second form))})

(defn- parse-primitive-binary-expr
  [op form]
  {:op op
   :args (mapv parse-expr (rest form))})

(defn- parse-io-read-line-expr
  [_form]
  {:op :io-read-line})

(defn- parse-io-print-expr
  [form]
  {:op :io-print
   :arg (parse-expr (second form))})

(defn- parse-io-println-expr
  [form]
  {:op :io-println
   :arg (parse-expr (second form))})

(defn- parse-string-split-on-expr
  [form]
  {:op :string-split-on
   :args (mapv parse-expr (rest form))})

(defn- parse-seq-get-expr
  [form]
  {:op :seq-get
   :args (mapv parse-expr (rest form))})

(defn- parse-seq-empty-expr
  [[_ element-type]]
  {:op :seq-empty
   :element-type element-type})

(defn- parse-map-empty-expr
  [[_ value-type]]
  {:op :map-empty
   :value-type value-type})

(defn- parse-string-substring-expr
  [form]
  {:op :string-substring
   :args (mapv parse-expr (rest form))})

(defn- parse-type-args
  [form]
  (expect-tag 'type-args form)
  (vec (rest form)))

(defn- parse-signature
  [[tag params return-type]]
  (expect-tag 'signature [tag])
  {:params (vec params)
   :return-type return-type})

(defn- parse-java-new-expr
  [form]
  (let [[_ class-name & more] form
        [type-args args] (if (and (seq more)
                                  (seq? (first more))
                                  (= 'type-args (ffirst more)))
                           [(parse-type-args (first more)) (rest more)]
                           [[] more])]
    {:op :java-new
     :class-name class-name
     :type-args type-args
     :args (mapv parse-expr args)}))

(defn- parse-java-call-expr
  [[_ target member-id signature-form & args]]
  {:op :java-call
   :target (parse-expr target)
   :member-id member-id
   :signature (parse-signature signature-form)
   :args (mapv parse-expr args)})

(defn- parse-java-static-call-expr
  [[_ class-name member-id signature-form & args]]
  {:op :java-static-call
   :class-name class-name
   :member-id member-id
   :signature (parse-signature signature-form)
   :args (mapv parse-expr args)})

(defn- parse-java-get-field-expr
  [[_ target field-name field-type]]
  {:op :java-get-field
   :target (parse-expr target)
   :field-name field-name
   :field-type field-type})

(defn- parse-java-static-get-field-expr
  [[_ class-name field-name field-type]]
  {:op :java-static-get-field
   :class-name class-name
   :field-name field-name
   :field-type field-type})

(defn- parse-java-set-field-expr
  [[_ target field-name field-type expr]]
  {:op :java-set-field
   :target (parse-expr target)
   :field-name field-name
   :field-type field-type
   :expr (parse-expr expr)})

(defn- parse-java-static-set-field-expr
  [[_ class-name field-name field-type expr]]
  {:op :java-static-set-field
   :class-name class-name
   :field-name field-name
   :field-type field-type
   :expr (parse-expr expr)})

(def ^:private expr-handlers
  {'call parse-call-expr
   'construct parse-construct-expr
   'variant parse-variant-expr
   'record-get parse-record-get-expr
   'if parse-if-expr
   'match parse-match-expr
   'let parse-let-expr
   'seq parse-seq-expr
   'lambda parse-lambda-expr
   'try parse-try-expr
   'var parse-var-expr
   'set parse-set-expr
   'loop parse-loop-expr
   'recur parse-recur-expr
   'raise parse-raise-expr
   'int-add (partial parse-primitive-binary-expr :int-add)
   'int-sub (partial parse-primitive-binary-expr :int-sub)
   'int-mul (partial parse-primitive-binary-expr :int-mul)
   'int-div (partial parse-primitive-binary-expr :int-div)
   'int-mod (partial parse-primitive-binary-expr :int-mod)
   'int-eq (partial parse-primitive-binary-expr :int-eq)
   'int-lt (partial parse-primitive-binary-expr :int-lt)
   'int-le (partial parse-primitive-binary-expr :int-le)
   'int-gt (partial parse-primitive-binary-expr :int-gt)
   'int-ge (partial parse-primitive-binary-expr :int-ge)
   'float-add (partial parse-primitive-binary-expr :float-add)
   'float-sub (partial parse-primitive-binary-expr :float-sub)
   'float-mul (partial parse-primitive-binary-expr :float-mul)
   'float-div (partial parse-primitive-binary-expr :float-div)
   'float-eq (partial parse-primitive-binary-expr :float-eq)
   'float-lt (partial parse-primitive-binary-expr :float-lt)
   'float-le (partial parse-primitive-binary-expr :float-le)
   'float-gt (partial parse-primitive-binary-expr :float-gt)
   'float-ge (partial parse-primitive-binary-expr :float-ge)
   'double-add (partial parse-primitive-binary-expr :double-add)
   'double-sub (partial parse-primitive-binary-expr :double-sub)
   'double-mul (partial parse-primitive-binary-expr :double-mul)
   'double-div (partial parse-primitive-binary-expr :double-div)
   'double-eq (partial parse-primitive-binary-expr :double-eq)
   'double-lt (partial parse-primitive-binary-expr :double-lt)
   'double-le (partial parse-primitive-binary-expr :double-le)
   'double-gt (partial parse-primitive-binary-expr :double-gt)
   'double-ge (partial parse-primitive-binary-expr :double-ge)
   'bool-eq (partial parse-primitive-binary-expr :bool-eq)
   'int-ne (partial parse-primitive-binary-expr :int-ne)
   'string-eq (partial parse-primitive-binary-expr :string-eq)
   'string-concat (partial parse-primitive-binary-expr :string-concat)
   'string-split-on parse-string-split-on-expr
   'string-char-at (partial parse-primitive-binary-expr :string-char-at)
   'string-substring parse-string-substring-expr
   'int->string (partial parse-primitive-unary-expr :int->string)
   'int->float (partial parse-primitive-unary-expr :int->float)
   'int->double (partial parse-primitive-unary-expr :int->double)
   'float->double (partial parse-primitive-unary-expr :float->double)
   'double->float (partial parse-primitive-unary-expr :double->float)
   'json-parse (partial parse-primitive-unary-expr :json-parse)
   'json-write (partial parse-primitive-unary-expr :json-write)
   'env-get (partial parse-primitive-unary-expr :env-get)
   'process-run (partial parse-primitive-binary-expr :process-run)
   'string->int (partial parse-primitive-unary-expr :string->int)
   'string-length (partial parse-primitive-unary-expr :string-length)
   'string-trim (partial parse-primitive-unary-expr :string-trim)
   'string-empty? (partial parse-primitive-unary-expr :string-empty?)
   'seq-empty parse-seq-empty-expr
   'seq-empty? (partial parse-primitive-unary-expr :seq-empty?)
   'seq-length (partial parse-primitive-unary-expr :seq-length)
   'seq-first (partial parse-primitive-unary-expr :seq-first)
   'seq-rest (partial parse-primitive-unary-expr :seq-rest)
   'seq-append (partial parse-primitive-binary-expr :seq-append)
   'seq-concat (partial parse-primitive-binary-expr :seq-concat)
   'seq-get parse-seq-get-expr
   'map-empty parse-map-empty-expr
   'map-set (partial parse-primitive-binary-expr :map-set)
   'map-get (partial parse-primitive-binary-expr :map-get)
   'map-contains? (partial parse-primitive-binary-expr :map-contains?)
   'map-keys (partial parse-primitive-unary-expr :map-keys)
   'bool-not (partial parse-primitive-unary-expr :bool-not)
   'bool-and (partial parse-primitive-binary-expr :bool-and)
   'bool-or (partial parse-primitive-binary-expr :bool-or)
   'io/read-line parse-io-read-line-expr
   'io/print parse-io-print-expr
   'io/println parse-io-println-expr
   'java/new parse-java-new-expr
   'java/call parse-java-call-expr
   'java/static-call parse-java-static-call-expr
   'java/get-field parse-java-get-field-expr
   'java/set-field parse-java-set-field-expr
   'java/get-static-field parse-java-static-get-field-expr
   'java/set-static-field parse-java-static-set-field-expr
   'local (fn [form]
            {:op :local
             :name (second form)})})

(defn- literal-pattern?
  [form]
  (or (true? form)
      (false? form)
      (integer? form)
      (string? form)))

(defn- wildcard-pattern?
  [form]
  (= '_ form))

(defn- record-pattern-form?
  [form]
  (and (seq? form) (= 'record (first form))))

(defn- union-pattern-form?
  [form]
  (seq? form))

(defn- parse-literal-pattern
  [form]
  {:op :literal-pattern
   :literal form})

(defn- parse-wildcard-pattern
  [_form]
  {:op :wildcard-pattern})

(defn- parse-binder-pattern
  [form]
  {:op :binder-pattern
   :name form})

(defn- parse-record-pattern
  [form]
  {:op :record-pattern
   :type (second form)
   :fields (mapv (fn [[name pattern]]
                   {:name name
                    :pattern (parse-pattern pattern)})
                 (drop 2 form))})

(defn- parse-union-pattern
  [form]
  {:op :union-pattern
   :name (first form)
   :args (mapv parse-pattern (rest form))})

(def ^:private pattern-parsers
  [[literal-pattern? parse-literal-pattern]
   [wildcard-pattern? parse-wildcard-pattern]
   [symbol? parse-binder-pattern]
   [record-pattern-form? parse-record-pattern]
   [union-pattern-form? parse-union-pattern]])

(defn parse-pattern
  [form]
  (if-let [[_ parser] (some (fn [[pred parser]]
                              (when (pred form)
                                [pred parser]))
                            pattern-parsers)]
    (parser form)
    (fail! "Unsupported pattern."
           {:form form})))

(defn- parse-union-decl
  [[_ name & forms]]
  (let [[type-params forms] (parse-type-params forms)
        [invariants forms] (parse-invariants forms)]
    {:op :union
     :name name
     :type-params type-params
     :invariants invariants
     :variants (mapv parse-variant-decl forms)}))

(defn- parse-fn-decl
  [form]
  (expect-at-least form 8 'fn)
  (let [[tag name params-form returns-form effects-form requires-form ensures-form body-form & extra] form]
    (expect-tag 'fn [tag])
    (when (seq extra)
      (fail! "Malformed fn declaration."
             {:context 'fn
              :detail "Expected exactly one function body expression."
              :form form}))
    (expect-tag 'params params-form)
    (expect-tag 'returns returns-form)
    (expect-tag 'effects effects-form)
    (expect-tag 'requires requires-form)
    (expect-tag 'ensures ensures-form)
    {:op :fn
     :name name
     :params (mapv parse-param (rest params-form))
     :return-type (second returns-form)
     :effects (parse-effects effects-form)
     :requires (parse-contract-clause 'requires requires-form)
     :ensures (parse-contract-clause 'ensures ensures-form)
     :body (parse-expr body-form)}))

(defn- parse-decl
  [form]
  (case (first form)
    data (parse-data-decl form)
    enum (parse-enum-decl form)
    union (parse-union-decl form)
    fn (parse-fn-decl form)
    (fail! "Unsupported declaration."
           {:form form})))

(defn parse-expr
  [form]
  (if (seq? form)
    (if-let [handler (get expr-handlers (first form))]
      (handler form)
      (fail! "Unsupported expression."
             {:form form}))
    form))

(defn parse-module
  [source]
  (let [[tag name & forms :as module-form] (read-source-form source)
        [host forms] (parse-host forms)
        [imports-form export-form & decl-forms] forms]
    (expect-seq-form module-form 'module)
    (expect-at-least module-form 4 'module)
    (expect-tag 'module [tag])
    (when-not imports-form
      (fail! "Malformed module."
             {:context 'module
              :detail "Expected (imports ...) after the module header."
              :form module-form}))
    (when-not export-form
      (fail! "Malformed module."
             {:context 'module
              :detail "Expected (export ...) immediately after (imports ...)."
              :form module-form}))
    (expect-tag 'imports imports-form)
    (expect-tag 'export export-form)
    (cond-> {:name name
             :imports (parse-imports imports-form)
             :exports (parse-exports export-form)
             :decls (mapv parse-decl decl-forms)}
      host
      (assoc :host host))))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-15T12:12:31.931016-05:00", :module-hash "71015801", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 5, :hash "-705971706"} {:id "form/1/declare", :kind "declare", :line 7, :end-line 7, :hash "1128659220"} {:id "form/2/declare", :kind "declare", :line 8, :end-line 8, :hash "-1446383543"} {:id "defn-/fail!", :kind "defn-", :line 10, :end-line 12, :hash "444631387"} {:id "defn-/reader-location", :kind "defn-", :line 14, :end-line 18, :hash "480682887"} {:id "defn-/reader-diagnostic", :kind "defn-", :line 20, :end-line 38, :hash "254148700"} {:id "defn-/read-source-form", :kind "defn-", :line 40, :end-line 48, :hash "857693789"} {:id "defn-/expect-seq-form", :kind "defn-", :line 50, :end-line 56, :hash "1060748871"} {:id "defn-/expect-count", :kind "defn-", :line 58, :end-line 64, :hash "1975585924"} {:id "defn-/expect-at-least", :kind "defn-", :line 66, :end-line 72, :hash "1949113360"} {:id "defn-/parse-import", :kind "defn-", :line 74, :end-line 87, :hash "-1464173778"} {:id "defn-/expect-tag", :kind "defn-", :line 89, :end-line 96, :hash "158154506"} {:id "defn-/parse-imports", :kind "defn-", :line 98, :end-line 101, :hash "2124099339"} {:id "defn-/parse-exports", :kind "defn-", :line 103, :end-line 106, :hash "987718804"} {:id "defn-/parse-host", :kind "defn-", :line 108, :end-line 115, :hash "-1723731476"} {:id "defn-/parse-param", :kind "defn-", :line 117, :end-line 120, :hash "949945736"} {:id "defn-/parse-field", :kind "defn-", :line 122, :end-line 126, :hash "1251715611"} {:id "defn-/parse-effects", :kind "defn-", :line 128, :end-line 143, :hash "2129032603"} {:id "defn-/parse-contract-clause", :kind "defn-", :line 145, :end-line 148, :hash "2065410627"} {:id "defn-/parse-invariants", :kind "defn-", :line 150, :end-line 155, :hash "192681247"} {:id "defn-/parse-type-params", :kind "defn-", :line 157, :end-line 162, :hash "-1233585086"} {:id "defn-/parse-data-decl", :kind "defn-", :line 164, :end-line 172, :hash "240075906"} {:id "defn-/parse-enum-decl", :kind "defn-", :line 174, :end-line 178, :hash "53471563"} {:id "defn-/parse-variant-decl", :kind "defn-", :line 180, :end-line 184, :hash "545649087"} {:id "defn-/parse-case", :kind "defn-", :line 186, :end-line 190, :hash "505569259"} {:id "defn-/parse-call-expr", :kind "defn-", :line 192, :end-line 196, :hash "1364196914"} {:id "defn-/parse-construct-expr", :kind "defn-", :line 198, :end-line 202, :hash "587366883"} {:id "defn-/parse-variant-expr", :kind "defn-", :line 204, :end-line 209, :hash "-385235348"} {:id "defn-/parse-record-get-expr", :kind "defn-", :line 211, :end-line 215, :hash "1628847814"} {:id "defn-/parse-if-expr", :kind "defn-", :line 217, :end-line 222, :hash "-1106654283"} {:id "defn-/parse-match-expr", :kind "defn-", :line 224, :end-line 228, :hash "1800281827"} {:id "defn-/parse-binding", :kind "defn-", :line 230, :end-line 233, :hash "-1959265528"} {:id "defn-/parse-let-expr", :kind "defn-", :line 235, :end-line 239, :hash "-647963169"} {:id "defn-/parse-seq-expr", :kind "defn-", :line 241, :end-line 244, :hash "302220611"} {:id "defn-/parse-lambda-expr", :kind "defn-", :line 246, :end-line 254, :hash "217255988"} {:id "defn-/parse-catch-clause", :kind "defn-", :line 256, :end-line 261, :hash "1820817090"} {:id "defn-/parse-finally-clause", :kind "defn-", :line 263, :end-line 266, :hash "-378492021"} {:id "defn-/parse-try-expr", :kind "defn-", :line 268, :end-line 277, :hash "716102796"} {:id "defn-/parse-var-expr", :kind "defn-", :line 279, :end-line 284, :hash "1325393821"} {:id "defn-/parse-set-expr", :kind "defn-", :line 286, :end-line 290, :hash "1429713983"} {:id "defn-/parse-loop-expr", :kind "defn-", :line 292, :end-line 296, :hash "-198693472"} {:id "defn-/parse-recur-expr", :kind "defn-", :line 298, :end-line 301, :hash "-1009172251"} {:id "defn-/parse-raise-expr", :kind "defn-", :line 303, :end-line 306, :hash "-1091784450"} {:id "defn-/parse-primitive-unary-expr", :kind "defn-", :line 308, :end-line 311, :hash "1155615128"} {:id "defn-/parse-primitive-binary-expr", :kind "defn-", :line 313, :end-line 316, :hash "-981498976"} {:id "defn-/parse-io-read-line-expr", :kind "defn-", :line 318, :end-line 320, :hash "1866185537"} {:id "defn-/parse-io-print-expr", :kind "defn-", :line 322, :end-line 325, :hash "2139111722"} {:id "defn-/parse-io-println-expr", :kind "defn-", :line 327, :end-line 330, :hash "-369802684"} {:id "defn-/parse-string-split-on-expr", :kind "defn-", :line 332, :end-line 335, :hash "-549703719"} {:id "defn-/parse-seq-get-expr", :kind "defn-", :line 337, :end-line 340, :hash "683923185"} {:id "defn-/parse-seq-empty-expr", :kind "defn-", :line 342, :end-line 345, :hash "-169556366"} {:id "defn-/parse-map-empty-expr", :kind "defn-", :line 347, :end-line 350, :hash "-922121204"} {:id "defn-/parse-string-substring-expr", :kind "defn-", :line 352, :end-line 355, :hash "-2136231073"} {:id "defn-/parse-type-args", :kind "defn-", :line 357, :end-line 360, :hash "-266637052"} {:id "defn-/parse-signature", :kind "defn-", :line 362, :end-line 366, :hash "804347329"} {:id "defn-/parse-java-new-expr", :kind "defn-", :line 368, :end-line 379, :hash "-1667842733"} {:id "defn-/parse-java-call-expr", :kind "defn-", :line 381, :end-line 387, :hash "-1542269505"} {:id "defn-/parse-java-static-call-expr", :kind "defn-", :line 389, :end-line 395, :hash "1075951034"} {:id "defn-/parse-java-get-field-expr", :kind "defn-", :line 397, :end-line 402, :hash "1853237465"} {:id "defn-/parse-java-static-get-field-expr", :kind "defn-", :line 404, :end-line 409, :hash "-1799498212"} {:id "defn-/parse-java-set-field-expr", :kind "defn-", :line 411, :end-line 417, :hash "-2021484420"} {:id "defn-/parse-java-static-set-field-expr", :kind "defn-", :line 419, :end-line 425, :hash "-1416091135"} {:id "def/expr-handlers", :kind "def", :line 427, :end-line 519, :hash "182380796"} {:id "defn-/literal-pattern?", :kind "defn-", :line 521, :end-line 526, :hash "635525918"} {:id "defn-/wildcard-pattern?", :kind "defn-", :line 528, :end-line 530, :hash "76497133"} {:id "defn-/record-pattern-form?", :kind "defn-", :line 532, :end-line 534, :hash "-1394925114"} {:id "defn-/union-pattern-form?", :kind "defn-", :line 536, :end-line 538, :hash "765247313"} {:id "defn-/parse-literal-pattern", :kind "defn-", :line 540, :end-line 543, :hash "148576341"} {:id "defn-/parse-wildcard-pattern", :kind "defn-", :line 545, :end-line 547, :hash "688434532"} {:id "defn-/parse-binder-pattern", :kind "defn-", :line 549, :end-line 552, :hash "-1102720389"} {:id "defn-/parse-record-pattern", :kind "defn-", :line 554, :end-line 561, :hash "785832603"} {:id "defn-/parse-union-pattern", :kind "defn-", :line 563, :end-line 567, :hash "1434576212"} {:id "def/pattern-parsers", :kind "def", :line 569, :end-line 574, :hash "1242457034"} {:id "defn/parse-pattern", :kind "defn", :line 576, :end-line 584, :hash "-258633312"} {:id "defn-/parse-union-decl", :kind "defn-", :line 586, :end-line 594, :hash "-1120328440"} {:id "defn-/parse-fn-decl", :kind "defn-", :line 596, :end-line 618, :hash "2092807272"} {:id "defn-/parse-decl", :kind "defn-", :line 620, :end-line 628, :hash "900166689"} {:id "defn/parse-expr", :kind "defn", :line 630, :end-line 637, :hash "-1140331564"} {:id "defn/parse-module", :kind "defn", :line 639, :end-line 664, :hash "1884068889"}]}
;; clj-mutate-manifest-end
