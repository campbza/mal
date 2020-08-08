(ns clj-mal.step0-repl
  (:gen-class))

(defn READ [x] x)

(defn EVAL [x] x)

(defn PRINT [x] x)

(defn rep [x] (-> x READ EVAL PRINT))

;;repl loop
(defn repl-loop []
  (println "user> ")
  (let [line (read-line)]
    (when line
      (println (rep line))
      (recur))))

(defn -main [& args]
  (repl-loop))
