(ns clj-mal.step1-read-print
  (:require [clj-mal.printer :as printer]
            [clj-mal.reader :as reader]
            [clojure.repl :as cr])
  (:gen-class))

(defn READ [& [s]] (reader/read-str s))

(defn EVAL [x] x)

(defn PRINT [form] (printer/print-string form))

(defn rep [x] (-> x READ EVAL PRINT))

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
