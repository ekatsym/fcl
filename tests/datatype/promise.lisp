(defpackage fcl/tests.promise
  (:nicknames :fcl/tests.data.promise :fcl/t.pm)
  (:use :common-lisp :fiveam :fcl/tests :fcl/tests.util :fcl.promise)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match))
(in-package fcl/tests.promise)


(defun gen-promise (&optional elements)
  (lambda ()
    (if elements
        (delay (funcall elements))
        (delay (funcall (gen-object))))))


(def-suite* fcl/tests.promise :in :fcl/tests)

(test matching
  "DELAY Matching"
  (for-all ((a (gen-object)))
    (match (delay a)
      ((delay b)
       (is (equal a b))))))

(test delay=unit
  "Equality of DELAY and UNIT"
  (for-all ((a (gen-object)))
    (is (data= (delay a) (unit 'promise a)))))


(def-suite* fcl/tests.promise.functor :in fcl/tests.promise)

(test identity-of-functor
  "Identity of Functor"
  (for-all ((a* (gen-promise)))
    (fcl/tests.functor:identity-test a*)))

(test composition-of-functor
  "Composition of Functor"
  (for-all ((a* (gen-promise))
            (a->b (gen-function))
            (b->c (gen-function)))
    (fcl/tests.functor:composition-test b->c a->b a*))
  (for-all ((a* (gen-promise (gen-integer :min -100 :max 100)))
            (a->b (gen-num-function))
            (b->c (gen-num-function)))
    (fcl/tests.functor:composition-test b->c a->b a*)))


(def-suite* fcl/tests.promise.applicative :in fcl/tests.promise)

(test identity-of-applicative
  "Identity of Applicative"
  (for-all ((a* (gen-promise)))
    (fcl/tests.applicative:identity-test 'promise a*)))

(test composition-of-applicative
  "Composition of Applicative"
  (for-all ((a* (gen-promise))
            (a->*b (gen-promise (gen-function)))
            (b->*c (gen-promise (gen-function))))
    (fcl/tests.applicative:composition-test 'promise b->*c a->*b a*))
  (for-all ((a* (gen-promise (gen-integer :min -100 :max 100)))
            (a->*b (gen-promise (gen-num-function)))
            (b->*c (gen-promise (gen-num-function))))
    (fcl/tests.applicative:composition-test 'promise b->*c a->*b a*)))

(test homomorphism-of-applicative
  "Homomorphism of Applicative"
  (for-all ((a (gen-object))
            (a->b (gen-function)))
    (fcl/tests.applicative:homomorphism-test 'promise a->b a))
  (for-all ((a (gen-integer :min -100 :max 100))
            (a->b (gen-num-function)))
    (fcl/tests.applicative:homomorphism-test 'promise a->b a)))

(test interchange-of-applicative
  "Interchange of Applicative"
  (for-all ((a (gen-object))
            (a->*b (gen-promise (gen-function))))
    (fcl/tests.applicative:interchange-test 'promise a->*b a))
  (for-all ((a (gen-integer :min -100 :max 100))
            (a->*b (gen-promise (gen-num-function))))
    (fcl/tests.applicative:interchange-test 'promise a->*b a)))


(def-suite* fcl/tests.promise.monad :in fcl/tests.promise)

(test left-identity-of-monad
  "Left Identity of Monad"
  (for-all ((a (gen-object))
            (a->b (gen-function)))
    (let ((a->b* (lambda (a) (unit 'promise (funcall a->b a)))))
      (fcl/tests.monad:left-identity-test 'promise a->b* a)))
  (for-all ((a (gen-integer :min -100 :max 100))
            (a->b (gen-num-function)))
    (let ((a->b* (lambda (a) (unit 'promise (funcall a->b a)))))
      (fcl/tests.monad:left-identity-test 'promise a->b* a))))

(test right-identity-of-monad
  "Right Identity of Monad"
  (for-all ((a* (gen-promise)))
    (fcl/tests.monad:right-identity-test 'promise a*)))

(test associativity-of-monad
  "Associativity of Monad"
  (for-all ((a* (gen-promise))
            (a->b (gen-function))
            (b->c (gen-function)))
    (let ((a->b* (lambda (a) (unit 'promise (funcall a->b a))))
          (b->c* (lambda (b) (unit 'promise (funcall b->c b)))))
      (fcl/tests.monad:associativity-test a->b* b->c* a*)))
  (for-all ((a* (gen-promise (gen-integer :min -100 :max 100)))
            (a->b (gen-num-function))
            (b->c (gen-num-function)))
    (let ((a->b* (lambda (a) (unit 'promise (funcall a->b a))))
          (b->c* (lambda (b) (unit 'promise (funcall b->c b)))))
      (fcl/tests.monad:associativity-test a->b* b->c* a*))))


#|
(deftest monad
  (testing "Left Identity"
    (dotimes (i 10)
      (let* ((a     (random-number -1.0d6 1.0d6))
             (a->b  (random-function))
             (a->b* (lambda (a) (delay (funcall a->b a)))))
        (fcl/tests.monad:left-identity-test 'promise a->b* a))))
  (testing "Right Identity"
    (dotimes (i 10)
      (let ((a* (delay (random-object))))
        (fcl/tests.monad:right-identity-test 'promise a*))))
  (testing "Associativity"
    (dotimes (i 10)
      (let* ((a*    (delay (random-number -1.0d6 1.0d6)))
             (a->b  (random-function))
             (b->c  (random-function))
             (a->b* (lambda (a) (delay (funcall a->b a))))
             (b->c* (lambda (b) (delay (funcall b->c b)))))
        (fcl/tests.monad:associativity-test a->b* b->c* a*)))))
|#
