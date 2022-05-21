(defpackage fcl/tests.maybe
  (:nicknames :fcl/tests.data.maybe :fcl/t.mb)
  (:use :common-lisp :fiveam :fcl/tests.util :fcl.maybe)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose #:partial #:curry))
(in-package :fcl/tests.maybe)


(defun gen-maybe (&optional element)
  (gen-one-element
    (nothing)
    (just (funcall (or element (gen-object))))))


(def-suite* fcl/tests.maybe :in :fcl/tests)


(fcl/tests.functor:functor-test
  maybe1
  (gen-maybe)
  (gen-function)
  (gen-function))

(fcl/tests.functor:functor-test
  maybe2
  (gen-maybe (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.applicative:applicative-test
  maybe1
  maybe
  (gen-object)
  (gen-maybe)
  (gen-function)
  (gen-function))

(fcl/tests.applicative:applicative-test
  maybe2
  maybe
  (gen-integer :min -100 :max 100)
  (gen-maybe (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.monad:monad-test
  maybe1
  maybe
  (gen-object)
  (gen-maybe)
  (gen-one-element
    (constantly (nothing))
    #'just
    (lambda (a) (just (make-list 3 :initial-element a)))
    (lambda (a) (just (make-array 4 :initial-element a))))
  (gen-one-element
    (constantly (nothing))
    #'just
    (lambda (a) (just (make-list 5 :initial-element a)))
    (lambda (a) (just (make-array 6 :initial-element a)))))

(fcl/tests.monad:monad-test
  maybe2
  maybe
  (gen-integer :min -100 :max 100)
  (gen-maybe (gen-integer :min -100 :max 100))
  (gen-one-element
    (constantly (nothing))
    #'just
    (lambda (a) (just (+ a a)))
    (lambda (a) (just (* a a))))
  (gen-one-element
    (constantly (nothing))
    #'just
    (lambda (a) (just (+ a a a)))
    (lambda (a) (just (* a a a)))))
