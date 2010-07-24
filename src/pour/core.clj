(ns pour.core
  (:use [pour.assert-args :only [assert-args]]))

;;; TODO: Refactor?
(defn validate [param forms]
  (if (empty? forms)
    [param nil]
    (let [f (first forms)
          err (first (next forms))
          v (f param)]
      (if-not (nil? v)
        (recur v (nnext forms))
        [nil err]))))

(defn- validator [forms]
  `(fn [param#] (validate param# ~forms)))

(defn form* [forms]
  (apply merge
         (for [[k v] (apply array-map forms)]
           {k (validator v)})))

(defn- result-map [place results]
  (zipmap (keys results) (map place (vals results))))

(defn value-map [results]
  (result-map first results))

(defn error-map [results]
  (result-map last (remove (fn [[k v]] (nil? (last v))) results)))

(def errors :errors)
(def values :values)
(defn errors? [results] (not (empty? (errors results))))

(defn transform-results [results]
  (assoc {values (value-map results)} errors (error-map results)))

(defn- every-other [f coll]
  (keep-indexed (fn [i v] (if (f i) v nil)) coll))

(defmacro defform
  "Define a web form for validation"
  [name & forms]
  (assert-args
   defform
   (even? (count forms)) "even count for forms vector"
   (every? keyword? (every-other even? forms)) "keys must be keywords"
   (every? vector? (every-other odd? forms)) "values must be vectors")
  (let [validators (form* forms)]
    `(defn ~name [params#]
       (let [vs# ~validators]
         (transform-results
          (apply merge
                 (for [[k# v#] vs#
                       :let [input# (get params# k#)]]
                   {k# ((k# vs#) input#)})))))))


(comment
  (defform test-form
    :login [required "Login is required"
            email-address "Login should be a valid email address"]
    :password [required "Password is required"
               (minimum-length 5) "Password should be longer than 4 chars"])
  )

(comment
  (defform converter-example
    :age [required "Age is required"
          a-number "Age must be a numeric value"
          #(if (> % 13) % nil) "Must be older than 13 years"])
  )
