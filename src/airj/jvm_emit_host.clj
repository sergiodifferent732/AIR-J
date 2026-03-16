(ns airj.jvm-emit-host
  (:import (clojure.asm MethodVisitor Opcodes Type)))

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

(defn emit-env-get
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-require mv "airj.host-runtime")
  (emit-clojure-var mv "airj.host-runtime" "env-get")
  (emit-expr mv (:arg expr) env)
  (.visitLdcInsn mv (Type/getObjectType (:root-class-name expr)))
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
                    true)
  (.visitTypeInsn mv Opcodes/CHECKCAST (:jvm-type expr)))

(defn emit-process-run
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-require mv "airj.host-runtime")
  (emit-clojure-var mv "airj.host-runtime" "process-run")
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (.visitLdcInsn mv (Type/getObjectType (:root-class-name expr)))
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
	                    true)
  (.visitTypeInsn mv Opcodes/CHECKCAST (:jvm-type expr)))

(defn emit-http-listen
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-require mv "airj.host-runtime")
  (emit-clojure-var mv "airj.host-runtime" "http-listen")
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKESTATIC
                    "java/lang/Integer"
                    "valueOf"
                    "(I)Ljava/lang/Integer;"
                    false)
  (.visitLdcInsn mv (Type/getObjectType (:root-class-name expr)))
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
                    true)
  (.visitTypeInsn mv Opcodes/CHECKCAST (:jvm-type expr)))

(defn emit-http-port
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-require mv "airj.host-runtime")
  (emit-clojure-var mv "airj.host-runtime" "http-port")
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;)Ljava/lang/Object;"
                    true)
  (.visitTypeInsn mv Opcodes/CHECKCAST "java/lang/Integer")
  (.visitMethodInsn mv
                    Opcodes/INVOKEVIRTUAL
                    "java/lang/Integer"
                    "intValue"
                    "()I"
                    false))

(defn emit-http-accept
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-require mv "airj.host-runtime")
  (emit-clojure-var mv "airj.host-runtime" "http-accept")
  (emit-expr mv (:arg expr) env)
  (.visitLdcInsn mv (Type/getObjectType (:root-class-name expr)))
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
                    true)
  (.visitTypeInsn mv Opcodes/CHECKCAST (:jvm-type expr)))

(defn emit-http-respond
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-require mv "airj.host-runtime")
  (emit-clojure-var mv "airj.host-runtime" "http-respond")
  (emit-expr mv (first (:args expr)) env)
  (emit-expr mv (second (:args expr)) env)
  (emit-expr mv (nth (:args expr) 2) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;Ljava/lang/Object;Ljava/lang/Object;)Ljava/lang/Object;"
                    true)
  (.visitInsn mv Opcodes/POP))

(defn emit-http-close
  [^MethodVisitor mv expr env {:keys [emit-expr]}]
  (emit-require mv "airj.host-runtime")
  (emit-clojure-var mv "airj.host-runtime" "http-close")
  (emit-expr mv (:arg expr) env)
  (.visitMethodInsn mv
                    Opcodes/INVOKEINTERFACE
                    "clojure/lang/IFn"
                    "invoke"
                    "(Ljava/lang/Object;)Ljava/lang/Object;"
                    true)
  (.visitInsn mv Opcodes/POP))

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-16T07:41:50.387732-05:00", :module-hash "46652372", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1892005050"} {:id "defn-/emit-clojure-var", :kind "defn-", :line 4, :end-line 13, :hash "487263659"} {:id "defn-/emit-require", :kind "defn-", :line 15, :end-line 31, :hash "-667175773"} {:id "defn/emit-env-get", :kind "defn", :line 33, :end-line 45, :hash "-1038812300"} {:id "defn/emit-process-run", :kind "defn", :line 47, :end-line 60, :hash "-1025864330"} {:id "defn/emit-http-listen", :kind "defn", :line 62, :end-line 80, :hash "1575721643"} {:id "defn/emit-http-port", :kind "defn", :line 82, :end-line 99, :hash "362015889"} {:id "defn/emit-http-accept", :kind "defn", :line 101, :end-line 113, :hash "1571508119"} {:id "defn/emit-http-respond", :kind "defn", :line 115, :end-line 128, :hash "2127378434"} {:id "defn/emit-http-close", :kind "defn", :line 130, :end-line 141, :hash "-2086092339"}]}
;; clj-mutate-manifest-end
