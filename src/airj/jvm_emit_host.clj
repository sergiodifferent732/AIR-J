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

;; clj-mutate-manifest-begin
;; {:version 1, :tested-at "2026-03-14T20:25:53.943121-05:00", :module-hash "-719533276", :forms [{:id "form/0/ns", :kind "ns", :line 1, :end-line 2, :hash "1892005050"} {:id "defn-/emit-clojure-var", :kind "defn-", :line 4, :end-line 13, :hash "487263659"} {:id "defn-/emit-require", :kind "defn-", :line 15, :end-line 31, :hash "-667175773"} {:id "defn/emit-env-get", :kind "defn", :line 33, :end-line 45, :hash "-1038812300"} {:id "defn/emit-process-run", :kind "defn", :line 47, :end-line 60, :hash "-1025864330"}]}
;; clj-mutate-manifest-end
