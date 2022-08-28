(defpackage markr/tests/main
  (:use :cl
        :markr
        :rove))
(in-package :markr/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :markr)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
