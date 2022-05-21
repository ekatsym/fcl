(defpackage fcl/tests.promise
  (:nicknames :fcl/tests.data.promise :fcl/t.pm)
  (:use :common-lisp :fiveam :fcl/tests :fcl/tests.util :fcl.promise)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match))
(in-package fcl/tests.promise)


(defun gen-promise (&optional element)
  (lambda ()
    (if element
        (delay (funcall element))
        (delay (funcall (gen-object))))))


(def-suite* fcl/tests.promise :in :fcl/tests)

(test pattern-match
  "Pattern Match"
  (for-all ((a (gen-object)))
    (match (delay a)
      ((delay b) (is (data= a b)))
      (_ (fail)))))

(test unit=delay
  "Equality of UNIT and DELAY"
  (for-all ((a (gen-object)))
    (is (data= (unit 'promise a) (delay a)))))

(fcl/tests.functor:functor-test
  promise1
  (gen-promise)
  (gen-function)
  (gen-function))

(fcl/tests.functor:functor-test
  promise2
  (gen-promise (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.applicative:applicative-test
  promise1
  promise
  (gen-object)
  (gen-promise)
  (gen-function)
  (gen-function))

(fcl/tests.applicative:applicative-test
  promise2
  promise
  (gen-integer :min -100 :max 100)
  (gen-promise (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.monad:monad-test
  promise1
  promise
  (gen-object)
  (gen-promise)
  (gen-one-element
    (lambda (a) (delay a))
    (lambda (a) (delay (make-list 3 :initial-element a)))
    (lambda (a) (delay (make-array 4 :initial-element a))))
  (gen-one-element
    (lambda (b) (delay b))
    (lambda (b) (delay (make-list 5 :initial-element b)))
    (lambda (b) (delay (make-array 6 :initial-element b)))))

(fcl/tests.monad:monad-test
  promise2
  promise
  (gen-integer :min -100 :max 100)
  (gen-promise (gen-integer :min -100 :max 100))
  (gen-one-element
    (lambda (a) (delay a))
    (lambda (a) (delay (+ a a)))
    (lambda (a) (delay (* a a))))
  (gen-one-element
    (lambda (b) (delay b))
    (lambda (b) (delay (+ b b b)))
    (lambda (b) (delay (* b b b)))))
