(ns airj.jvm-emit-text
  (:import (clojure.asm MethodVisitor Opcodes)))

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
;; {:version 1, :tested-at "2026-03-15T12:34:14.128202-05:00", :module-hash "1410345645", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1178648239"} {:id "defn-/emit-clojure-var", :kind "defn-", :line 4, :end-line 13, :hash "487263659"} {:id "defn-/emit-require", :kind "defn-", :line 15, :end-line 31, :hash "-667175773"} {:id "defn/emit-int->string", :kind "defn", :line 33, :end-line 41, :hash "726986209"} {:id "defn/emit-string-eq", :kind "defn", :line 43, :end-line 52, :hash "-592416675"} {:id "defn/emit-string-concat", :kind "defn", :line 54, :end-line 63, :hash "30394137"} {:id "defn/emit-string-split-on", :kind "defn", :line 65, :end-line 87, :hash "-1524365948"} {:id "defn/emit-string-char-at", :kind "defn", :line 89, :end-line 101, :hash "-1710653863"} {:id "defn/emit-string-substring", :kind "defn", :line 103, :end-line 113, :hash "1619683601"} {:id "defn/emit-string->int", :kind "defn", :line 115, :end-line 123, :hash "219712908"} {:id "defn/emit-string-length", :kind "defn", :line 125, :end-line 133, :hash "411227571"} {:id "defn/emit-string-trim", :kind "defn", :line 135, :end-line 143, :hash "-1164436646"} {:id "defn/emit-string-empty", :kind "defn", :line 145, :end-line 153, :hash "-466965248"} {:id "defn/emit-io-read-line", :kind "defn", :line 155, :end-line 165, :hash "-558478557"} {:id "defn/emit-io-print", :kind "defn", :line 167, :end-line 180, :hash "-1466442218"}]}
;; clj-mutate-manifest-end
