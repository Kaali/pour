(ns pour.validators-test
  (:use [pour.validators] :reload-all)
  (:use [pour.core :only [stop? stop-value]] :reload-all)
  (:use [clojure.test]))

(defmacro validator-test
  ([pred value] `(is (= ~value (~pred ~value))))
  ([pred value expected] `(is (= ~expected (~pred ~value)))))

(deftest required-validator
  (testing "succeeds on any seq with more than one item"
    (validator-test required "foo")
    (validator-test required [1 2 3]))
  (testing "nil is a failure"
    (validator-test required nil))
  (testing "fails on empty seqs"
    (validator-test required "" nil)
    (validator-test required [] nil)))

(deftest minimum-length-validator
  (is (fn? (minimum-length 1)))
  (validator-test (minimum-length 5) "123456")
  (validator-test (minimum-length 5) "12345")
  (validator-test (minimum-length 5) "1234" nil))

(deftest maximum-length-validator
  (is (fn? (maximum-length 1)))
  (validator-test (maximum-length 5) "123456" nil)
  (validator-test (maximum-length 5) "12345")
  (validator-test (maximum-length 5) "1234"))

(deftest a-number-validator
  (validator-test a-number "123" 123)
  (validator-test a-number "123.5" 123.5)
  (validator-test a-number 123)
  (validator-test a-number 123.5)
  (validator-test a-number "invalid number" nil))

;;; Just a basic test of validity, the regex used has been tested more
;;; thoroughly by the original creators.
(deftest email-address-validator
  (validator-test email-address "foo@bar.com")
  (validator-test email-address "foo" nil))

(deftest optional-validator
  (is (fn? (optional nil)))
  (is (stop? ((optional nil) nil)))
  (is (= :default (stop-value ((optional :default) nil))))
  (validator-test (optional :default) "foo"))
