(defpackage fcl/tests.promise
  (:nicknames :fcl/tests.data.promise :fcl/t.pm)
  (:use :common-lisp :rove :fcl/tests.util :fcl.promise)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match))
(in-package fcl/tests.promise)


(deftest matching
  (testing "DELAY"
    (dotimes (i 10)
      (let ((a (random-object)))
        (ok (match (delay a)
              ((delay b) (equal a b))))))))

(deftest delay=unit
  (testing "Equality of DELAY and UNIT"
    (dotimes (i 10)
      (let ((a (random-object)))
        (ok (data= (delay a) (unit 'promise a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 10)
      (let ((a* (delay (random-object))))
        (fcl/tests.functor:identity-test a*))))
  (testing "Composition"
    (dotimes (i 10)
      (let ((a*   (delay (random-number -1.0d6 1.0d6)))
            (a->b (random-function))
            (b->c (random-function)))
        (fcl/tests.functor:composition-test b->c a->b a*)))))

(deftest applicative
  (testing "Identity"
    (dotimes (i 10)
      (let ((a* (delay (random-object))))
        (fcl/tests.applicative:identity-test 'promise a*))))
  (testing "Composition"
    (dotimes (i 10)
      (let ((a*    (delay (random-number -1.0d6 1.0d6)))
            (a->*b (delay (random-function)))
            (b->*c (delay (random-function))))
        (fcl/tests.applicative:composition-test 'promise b->*c a->*b a*))))
  (testing "Homomorphism"
    (dotimes (i 10)
      (let ((a    (random-number -1.0d6 1.0d6))
            (a->b (random-function)))
        (fcl/tests.applicative:homomorphism-test 'promise a->b a))))
  (testing "Interchange"
    (dotimes (i 10)
      (let ((a     (random-number -1.0d6 1.0d6))
            (a->*b (delay (random-function))))
        (fcl/tests.applicative:interchange-test 'promise a->*b a)))))

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
