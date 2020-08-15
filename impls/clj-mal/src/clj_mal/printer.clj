(ns clj-mal.printer
  (:require [clojure.string :as s]))

(defn escape [s]
  (-> s
      (s/replace "\\" "\\\\")
      (s/replace "\"" "\\\"")
      (s/replace "\n" "\\n")))


(defn print-string [form]
  (cond
    (= nil form)   "nil"
    (string? form) (str "\"" (escape form) "\"")
    (list? form)   (str "(" (s/join " " (map print-string form)) ")")
    (vector? form) (str "[" (s/join " " (map print-string form)) "]")
    (map? form)    (str "{" (s/join " " (map (fn [[k v]]
                                               (str (print-string k) " "
                                                    (print-string v)))
                                             form)) "}")
    :else          (str form)))
