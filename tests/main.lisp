(defpackage cl-sample/tests/main
  (:use :cl
        :cl-sample
        :rove))
(in-package :cl-sample/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cl-sample)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
