(defpackage fcl/tests/main
  (:use :cl
        :fcl
        :rove))
(in-package :fcl/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :fcl)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
