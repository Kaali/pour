(ns pour.validators
  (:use [pour.core
         :only [stop-with-value pred-to-validator->> pred-to-validator->]]))

;;; TODO: Fix for integers
(def required (pred-to-validator->> (count) (pos?)))

(defn minimum-length [minlen]
  (pred-to-validator-> (count) (>= minlen)))

(defn maximum-length [maxlen]
  (pred-to-validator-> (count) (<= maxlen)))

(defmulti a-number type)
(defmethod a-number Number [param] param)
(defmethod a-number String
  [param]
  (try (do (Integer/parseInt param))
       (catch NumberFormatException _
         (try (do (Double/parseDouble param))
              (catch NumberFormatException _ nil)))))

; This regex is from James Watts and Francisco Jose Martin Moreno
(def email-regex #"^([\w\!\#$\%\&\'\*\+\-\/\=\?\^\`{\|\}\~]+\.)*[\w\!\#$\%\&\'\*\+\-\/\=\?\^\`{\|\}\~]+@((((([a-z0-9]{1}[a-z0-9\-]{0,62}[a-z0-9]{1})|[a-z])\.)+[a-z]{2,6})|(\d{1,3}\.){3}\d{1,3}(\:\d{1,5})?)$")

(def email-address (pred-to-validator->> (re-matches email-regex)))

(defn optional [value-to-return]
  "This validator short-circuits all validators after this and returns
   the value given as the argument."
  (fn [param] (if (nil? param) (stop-with-value value-to-return) param)))
