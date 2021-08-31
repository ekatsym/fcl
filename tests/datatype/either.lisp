(defpackage :fcl/tests.either
  (:nicknames :fcl/tests.data.either :fcl/t.et)
  (:use :common-lisp :rove :fcl/tests.util :fcl.either)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose #:partial #:curry))
(in-package :fcl/tests.either)


(deftest matching
  (testing "LEFT"
    (dotimes (i 100)
      (let ((a (random-object)))
        (ok (match (left a)
              ((left b)  (data= a b))
              ((right b) nil))))))
  (testing "RIGHT"
    (dotimes (i 100)
      (let ((a (random-object)))
        (ok (match (right a)
              ((left _)  nil)
              ((right b) (data= a b))))))))

(deftest left=mzero
  (testing "Equality of LEFT and MZERO"
    (ok (data= (left nil) (mzero 'either)))))

(deftest right=unit
  (testing "Equality of RIGHT and UNIT"
    (dotimes (i 100)
      (let ((a (random-object)))
        (ok (data= (right a) (unit 'either a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 100)
      (mlet ((a* (list (left (random-object))
                       (right (random-object)))))
        (fcl/tests.functor:identity-test a*)
        '())))
  (testing "Composition"
    (dotimes (i 100)
      (let ((a->b (random-function))
            (b->c (random-function)))
        (mlet ((a* (list (left (random-object))
                         (right (random-number -1.0d6 1.0d6)))))
          (fcl/tests.functor:composition-test b->c a->b a*)
          '())))))

(deftest applicative
  (testing "Identity"
    (dotimes (i 100)
      (mlet ((a*    (list (left (random-object))
                          (right (random-object)))))
        (fcl/tests.applicative:identity-test 'either a*)
        '())))
  (testing "Composition"
    (dotimes (i 100)
      (mlet ((a*    (list (left (random-object))
                          (right (random-number -1.0d6 1.0d6))))
             (a->*b (list (left (random-object))
                          (right (random-function))))
             (b->*c (list (left (random-object))
                          (right (random-function)))))
        (fcl/tests.applicative:composition-test 'either b->*c a->*b a*)
        '())))
  (testing "Homomorphism"
    (dotimes (i 100)
      (let ((a    (random-number -1.0d6 1.0d6))
            (a->b (random-function)))
        (fcl/tests.applicative:homomorphism-test 'either a->b a))))
  (testing "Interchange"
    (dotimes (i 100)
      (let ((a (random-number -1.0d6 1.0d6)))
        (mlet ((a->*b (list (left (random-object))
                            (right (random-function)))))
          (fcl/tests.applicative:interchange-test 'either a->*b a)
          '())))))

(deftest monad
  (testing "Left Identity"
    (dotimes (i 100)
      (let ((a    (random-number -1.0d6 1.0d6))
            (a->b (random-function)))
        (mlet ((a->b* (list (constantly (left (random-object)))
                            (lambda (a) (right (funcall a->b a))))))
          (fcl/tests.monad:left-identity-test 'either a->b* a)
          '()))))
  (testing "Right Identity"
    (dotimes (i 100)
      (mlet ((a* (list (left (random-object))
                       (right (random-object)))))
        (fcl/tests.monad:right-identity-test 'either a*)
        '())))
  (testing "Associativity"
    (dotimes (i 100)
      (let ((a->b (random-function))
            (b->c (random-function)))
        (mlet ((a*    (list (left (random-object))
                            (right (random-number -1.0d6 1.0d6))))
               (a->b* (list (constantly (left (random-object)))
                            (lambda (a) (right (funcall a->b a)))))
               (b->c* (list (constantly (left (random-object)))
                            (lambda (b) (right (funcall b->c b))))))
          (fcl/tests.monad:associativity-test a->b* b->c* a*)
          '())))))

(deftest monoid
  (testing "Left Identity"
    (dotimes (i 100)
      (mlet ((a* (list (left (random-object))
                       (right (random-object)))))
        (fcl/tests.monoid:left-identity-test 'either a*)
        '())))
  (testing "Right Identity"
    (dotimes (i 100)
      (mlet ((a* (list (left (random-object))
                       (right (random-object)))))
        (fcl/tests.monoid:right-identity-test 'either a*)
        '())))
  (testing "Associativity"
    (dotimes (i 100)
      (mlet ((a* (list (left (random-list 0 100))
                       (right (random-list 0 100))))
             (b* (list (left (random-list 0 100))
                       (right (random-list 0 100))))
             (c* (list (left (random-list 0 100))
                       (right (random-list 0 100)))))
        (ok (data= (mplus (mplus a* b*) c*)
                   (mplus a* (mplus b* c*))))
        '()))))
