(defpackage understat/tests/main
  (:use :cl
        :understat
        :rove))
(in-package :understat/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :understat)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
