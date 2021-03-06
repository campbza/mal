(ns clj-mal.step3-env
  (:require [clj-mal.env :as env]
            [clj-mal.printer :as printer]
            [clj-mal.reader :as reader]
            [clojure.repl :as cr])
  (:gen-class))

(defn wrap-res [form env]
  {:form form :env env})

(def EVAL)

(defn eval-coll [ast env coll-fn]
  (wrap-res (->> ast
                 (map (comp :form #(EVAL % env)))
                 doall
                 coll-fn)
            env))

(defn eval-ast [ast env]
  (cond
    (symbol? ast) (wrap-res (env/get-env env ast) env)
    (seq? ast)    (eval-coll ast env seq)
    (vector? ast) (eval-coll ast env vec)
    (map? ast)    (eval-coll (apply concat ast)
                             env
                             (partial apply hash-map))
    :else         (wrap-res ast env)))

(defn eval-def [x1 x2 env]
  (let [m (EVAL x2 env)]
    (update m :env #(env/set-env % x1 (:form m)))))

(defn eval-let [x1 x2 env]
  (let [bindings (partition 2 x1)
        new-env (reduce (fn [acc [k v]]
                          (assoc acc k (:form (EVAL v acc))))
                        (env/make-env env)
                        bindings)]
    (wrap-res (:form (EVAL x2 new-env)) env)))

(defn READ [& [s]]
  (reader/read-str s))

(defn EVAL [ast env]
  (if-not (seq? ast)
    (eval-ast ast env)
    (let [[x0 x1 x2] ast]
      (cond
        (empty? ast) (wrap-res ast env)
        (= 'def! x0) (eval-def x1 x2 env)
        (= 'let* x0) (eval-let x1 x2 env)
        :else        (let [m (eval-ast ast env)]
                       (update m :form #(apply (first %)
                                               (rest %))))))))

(defn PRINT [m]
  (update m :form printer/print-string))

(def repl-env
  {'+ +
   '- -
   '* *
   '/ /})

(defn rep [line env]
  (try
    (-> line
        READ
        (EVAL env)
        PRINT)
    (catch Throwable e (cr/pst e)
           (wrap-res nil env))))

;;repl loop
(defn repl-loop []
  (loop [env (env/make-env repl-env)]
    (println "user> ")
    (let [line (read-line)]
      (when line
        (let [{:keys [form env]} (rep line env)]
          (println form)
          (recur env))))))

(defn -main [& args]
  (repl-loop))
