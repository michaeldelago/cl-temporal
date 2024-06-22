(defpackage cl-temporal/tests/main
  (:use :cl
        :cl-temporal
        :rove))
(in-package :cl-temporal/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-temporal)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
