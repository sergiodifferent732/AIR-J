(ns airj.jvm-emit-text
  (:import (clojure.asm Label MethodVisitor Opcodes)))

(defn- emit-clojure-var
  [^MethodVisitor mv ns-name var-name]
  (.visitLdcInsn mv ^String ns-name)
  (.visitLdcInsn mv ^String var-name)
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    "clojure/lang/RT"
                    "var"
                    "(Ljava/lang/String;Ljava/lang/String;)Lclojure/lang/Var;"
                    false))

(defn- emit-require
  [^MethodVisitor mv ns-name]
  (emit-clojure-var mv "clojure.core" "require")
  (.visitLdcInsn mv ^String ns-name)
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    "clojure/lang/Symbol"
                    "intern"
                    "(Ljava/lang/String;)Lclojure/lang/Symbol;"
                    false)
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;)Ljava/lang/Object;"
                    true)
  (.visitInsn mv Opcodes/POP))

(defn- emit-seq-element-cast
  [^MethodVisitor mv jvm-type]
  (case jvm-type
    :int (do
           (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/Integer")
           (.visitMethodInsn mv
                             Opcodes/INVOKEVIRTUAL
                             "java/lang/Integer"
                             "intValue"
                             "()I"
                             false))
    :boolean (do
               (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/Boolean")
               (.visitMethodInsn mv
                                 Opcodes/INVOKEVIRTUAL
                                 "java/lang/Boolean"
                                 "booleanValue"
                                 "()Z"
                                 false))
    :float (do
             (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/Float")
             (.visitMethodInsn mv
                               Opcodes/INVOKEVIRTUAL
                               "java/lang/Float"
                               "floatValue"
                               "()F"
                               false))
    :double (do
              (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/Double")
              (.visitMethodInsn mv
                                Opcodes/INVOKEVIRTUAL
                                "java/lang/Double"
                                "doubleValue"
                                "()D"
                                false))
    (when (string? jvm-type)
      (.visitTypeInsn mv Opcodes/CHECKCAST jvm-type))))

(defn emit-int->string
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    "java/lang/String"
                    "valueOf"
                    "(I)Ljava/lang/String;"
                    false))

(defn emit-string-eq
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/String"
                    "equals"
                    "(Ljava/lang/Object;)Z"
                    false))

(defn emit-string-concat
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/String"
                    "concat"
                    "(Ljava/lang/String;)Ljava/lang/String;"
                    false))

(defn emit-string-split-on
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (second (:args expr)) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    "java/util/regex/Pattern"
                    "quote"
                    "(Ljava/lang/String;)Ljava/lang/String;"
                    false)
  (emit-expr mv (first (:args expr)) env)
  (.visitInsn mv Opcodes/SWAP)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/String"
                    "split"
                    "(Ljava/lang/String;)[Ljava/lang/String;"
                    false)
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    "java/util/Arrays"
                    "asList"
                    "([Ljava/lang/Object;)Ljava/util/List;"
                    false))

(defn emit-string-char-at
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (.visitInsn mv Opcodes/DUP)
  (.visitInsn mv Opcodes/ICONST_1)
  (.visitInsn mv Opcodes/IADD)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/String"
                    "substring"
                    "(II)Ljava/lang/String;"
                    false))

(defn emit-string-substring
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (emit-expr mv (nth (:args expr) 2) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/String"
                    "substring"
                    "(II)Ljava/lang/String;"
                    false))

(defn emit-string->int
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    "java/lang/Integer"
                    "parseInt"
                    "(Ljava/lang/String;)I"
                    false))

(defn emit-string-length
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/String"
                    "length"
                    "()I"
                    false))

(defn emit-string-trim
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/String"
                    "trim"
                    "()Ljava/lang/String;"
                    false))

(defn emit-string-empty
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/String"
                    "isEmpty"
                    "()Z"
                    false))

(defn emit-seq-empty
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (if (= "[Ljava/lang/String;" (:jvm-type (:arg expr)))
    (let [true-label (Label.)
          end-label (Label.)]
      (emit-expr mv (:arg expr) env)
      (.visitInsn mv Opcodes/ARRAYLENGTH)
      (.visitJumpInsn mv Opcodes/IFEQ true-label)
      (.visitLdcInsn mv false)
      (.visitJumpInsn mv Opcodes/GOTO end-label)
      (.visitLabel mv true-label)
      (.visitLdcInsn mv true)
      (.visitLabel mv end-label))
    (do
      (emit-expr mv (:arg expr) env)
      (.visitMethodInsn mv
                        Opcodes/INVOKEINTERFACE
                        "java/util/List"
                        "isEmpty"
                        "()Z"
                        true))))

(defn emit-seq-length
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (if (= "[Ljava/lang/String;" (:jvm-type (:arg expr)))
    (.visitInsn mv Opcodes/ARRAYLENGTH)
    (.visitMethodInsn mv
                      Opcodes/INVOKEINTERFACE
                      "java/util/List"
                      "size"
                      "()I"
                      true)))

(defn emit-seq-first
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (:arg expr) env)
  (.visitInsn mv Opcodes/ICONST_0)
  (if (= "[Ljava/lang/String;" (:jvm-type (:arg expr)))
    (.visitInsn mv Opcodes/AALOAD)
    (do
      (.visitMethodInsn mv
                        Opcodes/INVOKEINTERFACE
                        "java/util/List"
                        "get"
                        "(I)Ljava/lang/Object;"
                        true)
      (emit-seq-element-cast mv (:jvm-type expr)))))

(defn emit-seq-get
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (if (= "[Ljava/lang/String;" (:jvm-type (first (:args expr))))
    (.visitInsn mv Opcodes/AALOAD)
    (do
      (.visitMethodInsn mv
                        Opcodes/INVOKEINTERFACE
                        "java/util/List"
                        "get"
                        "(I)Ljava/lang/Object;"
                        true)
      (emit-seq-element-cast mv (:jvm-type expr)))))

(defn- emit-seq-as-list
  [^MethodVisitor mv expr env emit-expr]
  (emit-expr mv expr env)
  (when (= "[Ljava/lang/String;" (:jvm-type expr))
    (.visitMethodInsn mv
                      Opcodes/INVOKESTATIC
                      "java/util/Arrays"
                      "asList"
                      "([Ljava/lang/Object;)Ljava/util/List;"
                      false)))

(defn emit-seq-rest
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (if (= "[Ljava/lang/String;" (:jvm-type (:arg expr)))
    (do
      (emit-expr mv (:arg expr) env)
      (.visitInsn mv Opcodes/DUP)
      (.visitInsn mv Opcodes/ARRAYLENGTH)
      (.visitInsn mv Opcodes/DUP)
      (.visitInsn mv Opcodes/ICONST_1)
      (.visitMethodInsn mv
                        Opcodes/INVOKESTATIC
                        "java/lang/Math"
                        "min"
                        "(II)I"
                        false)
      (.visitInsn mv Opcodes/SWAP)
      (.visitMethodInsn mv
                        Opcodes/INVOKESTATIC
                        "java/util/Arrays"
                        "copyOfRange"
                        "([Ljava/lang/Object;II)[Ljava/lang/Object;"
                        false)
      (.visitMethodInsn mv
                        Opcodes/INVOKESTATIC
                        "java/util/Arrays"
                        "asList"
                        "([Ljava/lang/Object;)Ljava/util/List;"
                        false))
    (do
      (emit-expr mv (:arg expr) env)
      (.visitInsn mv Opcodes/DUP)
      (.visitMethodInsn mv
                        Opcodes/INVOKEINTERFACE
                        "java/util/List"
                        "size"
                        "()I"
                        true)
      (.visitInsn mv Opcodes/DUP)
      (.visitInsn mv Opcodes/ICONST_1)
      (.visitMethodInsn mv
                        Opcodes/INVOKESTATIC
                        "java/lang/Math"
                        "min"
                        "(II)I"
                        false)
      (.visitInsn mv Opcodes/SWAP)
      (.visitMethodInsn mv
                        Opcodes/INVOKEINTERFACE
                        "java/util/List"
                        "subList"
                        "(II)Ljava/util/List;"
                        true)
      (.visitTypeInsn mv Opcodes/NEW "java/util/ArrayList")
      (.visitInsn mv Opcodes/DUP_X1)
      (.visitInsn mv Opcodes/SWAP)
      (.visitMethodInsn mv
                        Opcodes/INVOKESPECIAL
                        "java/util/ArrayList"
                        "<init>"
                        "(Ljava/util/Collection;)V"
                        false))))

(defn emit-seq-concat
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (.visitTypeInsn mv Opcodes/NEW "java/util/ArrayList")
  (.visitInsn mv Opcodes/DUP)
  (emit-seq-as-list mv (first (:args expr)) env emit-expr)
  (.visitMethodInsn mv
                    Opcodes/INVOKESPECIAL
                    "java/util/ArrayList"
                    "<init>"
                    "(Ljava/util/Collection;)V"
                    false)
  (.visitInsn mv Opcodes/DUP)
  (emit-seq-as-list mv (second (:args expr)) env emit-expr)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/util/ArrayList"
                    "addAll"
                    "(Ljava/util/Collection;)Z"
                    false)
  (.visitInsn mv Opcodes/POP))

(defn emit-io-read-line
  [^MethodVisitor mv _expr _env _]
  (emit-require mv "airj.text-runtime")
  (emit-clojure-var mv "airj.text-runtime" "read-line")
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "()Ljava/lang/Object;"
                    true)
  (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/String"))

(defn emit-io-print
  [^MethodVisitor mv expr env {:keys [emit-expr method-name]}]
  (.visitFieldInsn mv
                   Opcodes/GETSTATIC
                   "java/lang/System"
                   "out"
                   "Ljava/io/PrintStream;")
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/io/PrintStream"
                    method-name
                    "(Ljava/lang/String;)V"
                    false))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-15T10:29:11.798873-05:00", :module-hash "2145461010", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "828052773"} {:id "defn-/emit-clojure-var", :kind "defn-", :line 4, :end-line 13, :hash "487263659"} {:id "defn-/emit-require", :kind "defn-", :line 15, :end-line 31, :hash "-667175773"} {:id "defn-/emit-seq-element-cast", :kind "defn-", :line 33, :end-line 69, :hash "-321324069"} {:id "defn/emit-int->string", :kind "defn", :line 71, :end-line 79, :hash "726986209"} {:id "defn/emit-string-eq", :kind "defn", :line 81, :end-line 90, :hash "-592416675"} {:id "defn/emit-string-concat", :kind "defn", :line 92, :end-line 101, :hash "30394137"} {:id "defn/emit-string-split-on", :kind "defn", :line 103, :end-line 125, :hash "-1524365948"} {:id "defn/emit-string-char-at", :kind "defn", :line 127, :end-line 139, :hash "-1710653863"} {:id "defn/emit-string-substring", :kind "defn", :line 141, :end-line 151, :hash "1619683601"} {:id "defn/emit-string->int", :kind "defn", :line 153, :end-line 161, :hash "219712908"} {:id "defn/emit-string-length", :kind "defn", :line 163, :end-line 171, :hash "411227571"} {:id "defn/emit-string-trim", :kind "defn", :line 173, :end-line 181, :hash "-1164436646"} {:id "defn/emit-string-empty", :kind "defn", :line 183, :end-line 191, :hash "-466965248"} {:id "defn/emit-seq-empty", :kind "defn", :line 193, :end-line 213, :hash "-1697564915"} {:id "defn/emit-seq-length", :kind "defn", :line 215, :end-line 225, :hash "-473209208"} {:id "defn/emit-seq-first", :kind "defn", :line 227, :end-line 240, :hash "1654991180"} {:id "defn/emit-seq-get", :kind "defn", :line 242, :end-line 255, :hash "1066037583"} {:id "defn-/emit-seq-as-list", :kind "defn-", :line 257, :end-line 266, :hash "599544557"} {:id "defn/emit-seq-rest", :kind "defn", :line 268, :end-line 328, :hash "-1409108672"} {:id "defn/emit-seq-concat", :kind "defn", :line 330, :end-line 349, :hash "-1653446077"} {:id "defn/emit-io-read-line", :kind "defn", :line 351, :end-line 361, :hash "-558478557"} {:id "defn/emit-io-print", :kind "defn", :line 363, :end-line 376, :hash "-1466442218"}]}
;; clj-mutate-manifest-end
