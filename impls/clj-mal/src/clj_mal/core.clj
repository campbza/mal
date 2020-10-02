(ns clj-mal.core
  (:require [clojure.string :refer [join]]
            [clj-mal.printer :as printer]))

(def core-ns
  {'prn    (fn [& args]
             (->> args
                  (map #(printer/print-string % true))
                  (join " ")
                  println))
   'list   list
   'list?  seq?
   'empty? empty?
   'count  count

   '+  +
   '-  -
   '/  /
   '*  *
   '=  =
   '<  <
   '>  >
   '<= <=
   '>= >=})
