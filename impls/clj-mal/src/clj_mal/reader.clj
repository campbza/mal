(ns clj-mal.reader
  (:require [clojure.string :as s]))

(def token-regex
  #"[\s,]*(~@|[\[\]{}()'`~^@]|\"(?:[\\].|[^\\\"])*\"?|;.*|[^\s\[\]{}()'\"`@,;]+)")

(def int-regex #"^-?[0-9]+$")
(def str-re #"^\"((?:[\\].|[^\\\"])*)\"$")
(def badstr-re #"^\"")

(defn unescape [s]
  (-> s
      (s/replace "\\\\" "\u029e")
      (s/replace "\\\"" "\"")
      (s/replace "\\n" "\n")
      (s/replace "\u029e" "\\")))

(def seq-start->end {"(" ")", "{" "}", "[" "]"})
(def seq-type->fn {"(" (partial apply list)
                   "[" (partial vec)
                   "{" (partial apply hash-map)})

(defn tokenize [s]
  (map second (re-seq token-regex s)))

(def read-form)

(defn read-list [rdr end]
  (let [{:keys [tokens position]}
        (update rdr :position inc)]
    (loop [lst []
           p position]
      (let [token (nth tokens p nil)]
        (cond
          (= token end) {:tokens tokens
                         :position p
                         :form lst}
          (not token) (throw (Exception.
                              (str "expected '" end "', got EOF")))
          :else (let [rdr (read-form (assoc rdr :position p))]
                  (recur (conj lst (:form rdr))
                         (inc (:position rdr)))))))))

(defn read-seq [rdr token]
  (let [rdr (read-list rdr (seq-start->end token))]
    (assoc rdr
           :form ((seq-type->fn token) (:form rdr))
           :position (:position rdr))))

(defn read-form [{:keys [tokens position] :as rdr}]
  (let [token (nth tokens position nil)
        seq-starts (set (keys seq-start->end))]
    (cond
      (= "'" token)             (update (assoc rdr
                                               :form
                                               (list 'quote
                                                     (-> rdr
                                                         (update :position inc)
                                                         read-form
                                                         :form)))
                                        :position
                                        inc)
      (= "`" token)             (update (assoc rdr
                                               :form
                                               (list 'quasiquote
                                                     (-> rdr
                                                         (update :position inc)
                                                         read-form
                                                         :form)))
                                        :position
                                        inc)
      (= "~" token)             (update (assoc rdr
                                               :form
                                               (list 'unquote
                                                     (-> rdr
                                                         (update :position inc)
                                                         read-form
                                                         :form)))
                                        :position
                                        inc)
      (= "~@" token)            (update (assoc rdr
                                               :form
                                               (list 'splice-unquote
                                                     (-> rdr
                                                         (update :position inc)
                                                         read-form
                                                         :form)))
                                        :position
                                        inc)
      (= "^" token)             (let [rdr (read-form (update rdr
                                                             :position
                                                             inc))]
                                  (assoc rdr
                                         :form
                                         (list 'with-meta
                                               (-> rdr
                                                   (update :position inc)
                                                   read-form
                                                   :form))))
      (= "@" token)             (update (assoc rdr
                                               :form
                                               (list 'deref
                                                     (-> rdr
                                                         (update :position inc)
                                                         read-form
                                                         :form)))
                                        :position
                                        inc)
      (seq-starts token)        (read-seq rdr token)
      (re-seq int-regex token)  (assoc rdr :form (Integer/parseInt token))
      (= "nil" token)           (assoc rdr :form nil)
      (#{"true" "false"} token) (assoc rdr :form (symbol token))
      (re-seq str-re token)     (assoc rdr :form (->> token
                                                      (re-find str-re)
                                                      second
                                                      unescape))
      (re-seq badstr-re token)  (throw (Exception.
                                        (str "expected '\"', got EOF")))
      :else                     (assoc rdr :form (symbol token)))))

(defn reader [tokens]
  {:tokens (vec tokens) :position 0})

(defn read-str [s]
  (-> s
      tokenize
      reader
      read-form
      :form))
