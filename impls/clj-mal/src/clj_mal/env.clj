(ns clj-mal.env)

(defn make-env [& [outer]] {:outer outer})

(defn set-env [env k v] (assoc env k v))

(defn env-find [env sym]
  (cond
    (contains? env sym) env
    (:outer env)        (env-find (:outer env) sym)
    :else               nil))

(defn get-env
  [env sym]
  (let [e (env-find env sym)]
    (if e
      (e sym)
      (throw (Exception.
              (str "'" sym "' not found"))))))
