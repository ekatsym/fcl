(defpackage :fcl/tests.either
  (:nicknames :fcl/tests.data.either :fcl/t.et)
  (:use :common-lisp :fiveam :fcl/tests.util :fcl.either)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose #:partial #:curry))
(in-package :fcl/tests.either)


(defun gen-either (&optional element)
  (gen-one-element
    (left nil)
    (left (funcall (or element (gen-object))))
    (right (funcall (or element (gen-object))))))


(def-suite* fcl/tests.either :in :fcl/tests)


(fcl/tests.functor:functor-test
  either1
  (gen-either)
  (gen-function)
  (gen-function))

(fcl/tests.functor:functor-test
  either2
  (gen-either (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.applicative:applicative-test
  either1
  either
  (gen-object)
  (gen-either)
  (gen-function)
  (gen-function))

(fcl/tests.applicative:applicative-test
  either2
  either
  (gen-integer :min -100 :max 100)
  (gen-either (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.monad:monad-test
  either1
  either
  (gen-object)
  (gen-either)
  (gen-one-element
    (constantly (left nil))
    #'left
    (lambda (a) (left (format nil "~S" a)))
    #'right
    (lambda (a) (right (make-list 3 :initial-element a)))
    (lambda (a) (right (make-array 4 :initial-element a))))
  (gen-one-element
    (constantly (left nil))
    #'left
    (lambda (a) (left (format nil "~S" a)))
    #'right
    (lambda (a) (right (make-list 5 :initial-element a)))
    (lambda (a) (right (make-array 6 :initial-element a)))))

(fcl/tests.monad:monad-test
  either2
  either
  (gen-integer :min -100 :max 100)
  (gen-either (gen-integer :min -100 :max 100))
  (gen-one-element
    (constantly (left nil))
    #'left
    (lambda (a) (left (format nil "~S" a)))
    #'right
    (lambda (a) (right (+ a a)))
    (lambda (a) (right (* a a))))
  (gen-one-element
    (constantly (left nil))
    #'left
    (lambda (a) (left (format nil "~S" a)))
    #'right
    (lambda (a) (right (+ a a a)))
    (lambda (a) (right (* a a a)))))

(fcl/tests.monoid:monoid-test
  either
  either
  (gen-either (gen-list :length (gen-integer :min 0 :max 30)
                        :elements (gen-object))))
