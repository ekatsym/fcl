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


(def-suite* maybe-tests :in :fcl/tests)

(def-suite* pattern-match :in maybe-tests)

(test match-nothing
  "Pattern Match for NOTHING"
  (match (nothing)
    ((nothing) (pass))
    (_ (fail)))
  )

(test match-just
  "Pattern Match for JUST"
  (for-all ((a (gen-object)))
    (match (just a)
      ((just b) (is (data= a b)))
      (_ (fail)))))

(def-suite* monad-plus :in maybe-tests)

(test unit=just
  "Equality of UNIT and JUST"
  (for-all ((a (gen-object)))
    (is (data= (unit 'maybe a) (just a)))))

(test mzero=nothing
  "Equality of MZERO and NOTHING"
  (is (data= (mzero 'maybe) (nothing))))

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

(fcl/tests.monoid:monoid-test
  maybe
  maybe
  (gen-maybe (gen-list :length (gen-integer :min 0 :max 30)
                       :elements (gen-object))))
