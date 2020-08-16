(ns clj-mal.step2-eval
  (:require [clj-mal.printer :as printer]
            [clj-mal.reader :as reader]
            [clojure.repl :as cr])
  (:gen-class))

(def EVAL)

(defn eval-ast [ast env]
  (cond
    (symbol? ast) (or (env ast)
                      (throw (Exception.
                              (str ast " not found!"))))
    (seq? ast)    (->> ast
                       (map #(EVAL % env))
                       doall)
    (vector? ast) (->> ast
                       (map #(EVAL % env))
                       doall
                       vec)
    (map? ast)    (->> ast
                       (apply concat)
                       (map #(EVAL % env))
                       doall
                       (apply hash-map))
    :else         ast))

(defn READ [& [s]]
  (reader/read-str s))

(defn EVAL [ast env]
  (cond
    (not (seq? ast)) (eval-ast ast env)
    (empty? ast)     ast
    :else            (let [xs (eval-ast ast env)]
                       (apply (first xs) (rest xs)))))

(defn PRINT [form]
  (printer/print-string form))

(def repl-env
  {'+ +
   '- -
   '* *
   '/ /})

(defn rep [x] (-> x
                  READ
                  (EVAL repl-env)
                  PRINT))

;;repl loop
(defn repl-loop []
  (println "user> ")
  (let [line (read-line)]
    (when line
      (try
        (println (rep line))
        (catch Throwable e (cr/pst e)))
      (recur))))

(defn -main [& args]
  (repl-loop))
