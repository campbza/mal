(ns clj-mal.step3-env
  (:require [clj-mal.env :as env]
            [clj-mal.printer :as printer]
            [clj-mal.reader :as reader]
            [clojure.repl :as cr])
  (:gen-class))

(def EVAL)

(defn eval-coll [ast env coll-fn]
  (->> ast
       (map (comp :form #(EVAL % env)))
       doall
       coll-fn))

(defn eval-ast [ast env]
  (cond
    (symbol? ast) {:form (env/get-env env ast)
                   :env env}
    (seq? ast)    {:form (eval-coll ast env seq)
                   :env env}
    (vector? ast) {:form (eval-coll ast env vec)
                   :env env}
    (map? ast)    {:form (eval-coll (apply concat ast)
                                    env
                                    (partial apply hash-map))
                   :env env}
    :else         {:form ast
                   :env env}))

(defn READ [& [s]]
  (reader/read-str s))

(defn update-env [{:keys [form env] :as m} k]
  (println 111111111 m)
  {:form form
   :env (env/set-env env k form)})

(defn eval-def [x1 x2 env]
  (try
    (-> x2
        (EVAL env)
        (update-env x1))
    (catch Throwable e
      (cr/pst e)
      {:env env
       :form nil})))

(defn EVAL [ast env]
  (if-not (seq? ast)
    (eval-ast ast env)
    (let [[x0 x1 x2] ast]
      (cond
        (empty? ast) {:form ast :env env}
        (= 'def! x0) (eval-def x1 x2 env)
        (= 'let* x0) (let [bindings (partition 2 x1)
                           new-env
                           (reduce (fn [acc [k v]]
                                     (assoc acc k (:form (EVAL v acc))))
                                   (env/make-env env)
                                   bindings)]
                       {:form (:form (EVAL x2 new-env))
                        :env env})
        :else        (let [{:keys [form env]} (eval-ast ast env)]
                       {:form (apply (first form) (rest form))
                        :env env})))))

#_(EVAL (READ "(let* (c 2) c)") repl-env)

(defn PRINT [{:keys [form env]}]
  {:form (printer/print-string form)
   :env env})

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
    (catch Throwable e (cr/pst e))))

;;repl loop
(defn repl-loop []
  (loop [env repl-env]
    (println "user> ")
    (let [line (read-line)]
      (when line
        (let [{:keys [form env]} (rep line env)]
          (println form)
          (recur env))))))

(defn -main [& args]
  (repl-loop))
