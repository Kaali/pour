(ns pour.validators)

;;; TODO: Fix for integers
(defn required [param] (if (pos? (count param)) param nil))

(defn minimum-length [minlen]
  (fn [param] (if (>= (count param) minlen) param nil)))

(defn maximum-length [maxlen]
  (fn [param] (if (<= (count param) maxlen) param nil)))

(defn a-number [param]
  (try (do (Integer/parseInt param))
       (catch NumberFormatException _ nil)))

; This regex is from James Watts and Francisco Jose Martin Moreno
(def email-regex #"^([\w\!\#$\%\&\'\*\+\-\/\=\?\^\`{\|\}\~]+\.)*[\w\!\#$\%\&\'\*\+\-\/\=\?\^\`{\|\}\~]+@((((([a-z0-9]{1}[a-z0-9\-]{0,62}[a-z0-9]{1})|[a-z])\.)+[a-z]{2,6})|(\d{1,3}\.){3}\d{1,3}(\:\d{1,5})?)$")

(defn email-address [param]
  (if (re-matches email-regex param) param nil))
