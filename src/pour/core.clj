(ns pour.core
  (:use [pour.assert-args :only [assert-args]]))

(def stop ::stop)
(defn stop-with-value [v] [stop v])
(defn stop? [v] (= stop (first v)))
(defn stop-value [v] (last v))

(defn pred-to-validator
  "Converts a simple predicate to a validator"
  [pred]
  (fn [param] (when (pred param) param)))

(defmacro pred-to-validator->
  "-> threading version of pred-to-validator"
  [& preds]
  `(fn [param#] (when (-> param# ~@preds) param#)))

(defmacro pred-to-validator->>
  "->> threading version of pred-to-validator"
  [& preds]
  `(fn [param#] (when (->> param# ~@preds) param#)))

(defmacro pred-to-validator-do->
  "-> threading version of pred-to-validator which returns the chain result"
  [& preds]
  `(fn [param#] (when-let [v# (-> param# ~@preds)] v#)))

(defmacro pred-to-validator-do->>
  "->> threading version of pred-to-validator which returns the chain result"
  [& preds]
  `(fn [param#] (when-let [v# (->> param# ~@preds)] v#)))

;;; TODO: Refactor?
(defn validate [param forms]
  (if (empty? forms)
    [param nil]
    (let [f (first forms)
          err (first (next forms))
          v (f param)]
      (cond
       (and (vector? v) (stop? v)) [(stop-value v) nil]
       (nil? v) [nil err]
       :else (recur v (nnext forms))))))

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

(comment
  (defform optional-form
    :age [(optional 999) ""
          a-number "Age must be a numeric value"])
  )
