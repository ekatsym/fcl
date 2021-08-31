(defpackage fcl/tests.maybe
  (:nicknames :fcl/tests.data.maybe :fcl/t.mb)
  (:use :common-lisp :rove :fcl/tests.util :fcl.maybe)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose #:partial #:curry))
(in-package :fcl/tests.maybe)


(deftest matching
  (testing "NOTHING"
    (ok (match (nothing)
          ((nothing) t)
          ((just _) nil))))
  (testing "JUST"
    (dotimes (i 100)
      (let ((a (random-object)))
        (ok (match (just a)
              ((nothing) nil)
              ((just b)  (data= a b))))))))

(deftest nothing=mzero
  (testing "Equality of NOTHING and MZERO"
    (ok (data= (nothing) (mzero 'maybe)))))

(deftest just=unit
  (testing "Equality of JUST and UNIT"
    (dotimes (i 100)
      (let ((a (random-object)))
        (ok (data= (just a) (unit 'maybe a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 100)
      (mlet ((a* (list (nothing) (just (random-object)))))
        (fcl/tests.functor:identity-test a*))))
  (testing "Composition"
    (dotimes (i 100)
      (let ((a->b (random-function))
            (b->c (random-function)))
        (mlet ((a* (list (nothing) (just (random-number -1.0d6 1.0d6)))))
          (fcl/tests.functor:composition-test b->c a->b a*))))))

(deftest applicative
  (testing "Identity"
    (dotimes (i 100)
      (mlet ((a* (list (nothing) (just (random-object)))))
        (fcl/tests.applicative:identity-test 'maybe a*))))
  (testing "Composition"
    (dotimes (i 100)
      (mlet ((a*    (list (nothing) (just (random-number -1.0d6 1.0d6))))
             (a->*b (list (nothing) (just (random-function))))
             (b->*c (list (nothing) (just (random-function)))))
        (fcl/tests.applicative:composition-test 'maybe b->*c a->*b a*))))
  (testing "Homomorphism"
    (dotimes (i 100)
      (let ((a    (random-number -1.0d6 1.0d6))
            (a->b (random-function)))
        (fcl/tests.applicative:homomorphism-test 'maybe a->b a))))
  (testing "Interchange"
    (dotimes (i 100)
      (let ((a (random-number -1.0d6 1.0d6)))
        (mlet ((a->*b (list (nothing) (just (random-function)))))
          (fcl/tests.applicative:interchange-test 'maybe a->*b a))))))

(deftest monad
  (testing "Left Identity"
    (dotimes (i 100)
      (let ((a     (random-number -1.0d6 1.0d6))
            (a->b  (random-function)))
        (mlet ((a->b* (list (constantly (nothing))
                            (lambda (a) (just (funcall a->b a))))))
          (fcl/tests.monad:left-identity-test 'maybe a->b* a)))))
  (testing "Right Identity"
    (dotimes (i 100)
      (mlet ((a* (list (nothing) (just (random-object)))))
        (fcl/tests.monad:right-identity-test 'maybe a*))))
  (testing "Associativity"
    (dotimes (i 100)
      (let ((a->b (random-function))
            (b->c (random-function)))
        (mlet ((a*    (list (nothing)
                            (just (random-number -1.0d6 1.0d6))))
               (a->b* (list (constantly (nothing))
                            (lambda (a) (just (funcall a->b a)))))
               (b->c* (list (constantly (nothing))
                            (lambda (b) (just (funcall b->c b))))))
          (fcl/tests.monad:associativity-test a->b* b->c* a*))))))

(deftest monoid
  (testing "Left Identity"
    (dotimes (i 100)
      (mlet ((a* (list (nothing)
                       (just (random-object)))))
        (fcl/tests.monoid:left-identity-test 'maybe a*))))
  (testing "Right Identity"
    (dotimes (i 100)
      (mlet ((a* (list (nothing)
                       (just (random-object)))))
        (fcl/tests.monoid:right-identity-test 'maybe a*))))
  (testing "Associativity"
    (dotimes (i 100)
      (mlet ((a* (list (nothing) (just (random-list 0 1000))))
             (b* (list (nothing) (just (random-list 0 1000))))
             (c* (list (nothing) (just (random-list 0 1000)))))
        (fcl/tests.monoid:associativity-test a* b* c*)))))
