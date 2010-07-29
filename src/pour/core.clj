(ns pour.core
  (:use [pour.assert-args :only [assert-args]]))

;;; Short circuiting fns
(def stop ::stop)
(defn stop-with-value [v] [stop v])
(defn stop? [s] (= stop (first s)))
(defn stop-value [s] (second s))

;;; Accessors and predicates for defform results
(def errors :errors)
(def values :values)
(defn errors? [results] (not (empty? (errors results))))

(defmacro fields
  "Return a vector of fields defined in a form.

  Field order is not retained"
  [form]
  `(:fields (meta (var ~form))))


;;; Predicate creation helpers
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
(defn validate
  "Validates param against a vector of validator forms"
  [param forms]
  (if (empty? forms)
    [param nil]
    (let [f (first forms)
          err (first (next forms))
          v (f param)]
      (cond
       (and (vector? v) (stop? v)) [(stop-value v) nil]
       (nil? v) [nil err]
       :else (recur v (nnext forms))))))

(defn- value-map
  "Transform defform result map to just contain the values"
  [results]
  (into {} (for [[k v] results] {k (first v)})))

(defn- error-map
  "Transform defform result map to just contain the errors"
  [results]
  (into {} (for [[k v] results
                 :let [err (second v)]
                 :when (not (nil? err))]
             {k err})))

(defn transform-results
  "Transforms defform result map to a more usable format"
  [results]
  {values (value-map results) errors (error-map results)})

(defn- every-other
  "Returns a vector with every other value of coll retained"
  [coll]
  (loop [coll coll
         acc []]
    (if (empty? coll) acc
        (recur (nnext coll) (conj acc (first coll))))))

(defn- validator
  "Returns a quoted anonymous validator function for validator forms"
  [forms]
  `(fn [param#] (validate param# ~forms)))

(defn transform-validator-forms
  "Convert validator forms to a map of fields and validator functions,
  where the validator function runs through the whole chain."
  [forms]
  (apply merge
         (for [[k v] (apply array-map forms)]
           {k (validator v)})))

(defn form*
  "Checks params against computed validators.

  Computed validators means that they have been run through
  transform-validator-forms function to convert from vectors to proper maps."
  [params validators]
  (transform-results
   (apply merge
          (for [[k v] validators
                :let [input (get params k)]]
            {k ((k validators) input)}))))

(defmacro defform
  "Define a web form for validation

  It's a macro which allows for the special syntax, and as a useful feature
  it precomputes the validator forms transformation."
  [name & forms]
  (assert-args
   defform
   (even? (count forms)) "even count for forms vector"
   (every? keyword? (every-other forms)) "keys must be keywords"
   (every? vector? (every-other (next forms))) "values must be vectors")
  (let [validators (transform-validator-forms forms)
        fields (set (keys validators))]
    `(defn ~(with-meta name (assoc (meta name) :fields fields))
       [params#]
       (form* params# ~validators))))


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
