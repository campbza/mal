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
    (symbol? ast) {:form (env ast)
                   :env env}
    (seq? ast)    {:form (eval-coll ast env seq)
                   :env env}
    (vector? ast) {:form (eval-coll ast env vec)
                   :env env}
    (map? ast)    {:form (eval-coll (apply concat ast) env (partial apply hash-map))
                   :env env}
    :else         {:form ast :env env}))

(defn READ [& [s]]
  (reader/read-str s))

(defn EVAL [ast env]
  (cond
    (not (seq? ast)) (eval-ast ast env)
    (empty? ast)     {:form ast :env env}
    :else            (let [{:keys [form env]} (eval-ast ast env)]
                       {:form (apply (first form) (rest form)) :env env})))

#_(EVAL (READ "(+ 5 6)") repl-env)

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
  (println "user> ")
  (loop [env repl-env]
    (let [line (read-line)]
      (when line
        (let [{:keys [form env]} (rep line env)]
          (println form)
          (recur env))))))

(defn -main [& args]
  (repl-loop))

(comment
  (-main)
  )
