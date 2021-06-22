(defpackage :fcl/tests.maybe
  (:nicknames :fcl/tests.data.mb :fcl/t.mb)
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
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (match (just a)
              ((nothing) nil)
              ((just b)  (data= a b))))))))

(deftest nothing=mzero
  (testing "Equality of NOTHING and MZERO"
    (ok (data= (nothing) (mzero 'maybe)))))

(deftest just=unit
  (testing "Equality of JUST and UNIT"
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (data= (just a) (unit 'maybe a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 1000)
      (mlet ((a* (list (nothing)
                       (just (random-object)))))
        (ok (data= (fmap #'identity a*)
                   a*))
        '())))
  (testing "Composition"
    (dotimes (i 1000)
      (mlet ((a* (list (nothing)
                       (just (random-number -1000000 1000000))
                       (just (random-number -1.0e6 1.0e6))))
             (a->b (list (lambda (x) (* x x))))
             (b->c (list (lambda (x) (+ x x)))))
        (ok (data= (fmap (compose b->c a->b) a*)
                   (fmap b->c (fmap a->b a*))))
        '()))))

(deftest applicative
  (testing "Identity"
    (dotimes (i 1000)
      (mlet ((a* (list (nothing)
                       (just (random-object)))))
        (ok (data= (amap (just #'identity) a*)
                   a*))
        '())))
  (testing "Composition"
    (dotimes (i 1000)
      (mlet ((a->*b (list (nothing) 
                          (just (lambda (x) (* x x)))))
             (b->*c (list (nothing)
                          (just (lambda (x) (+ x x)))))
             (a* (list (nothing)
                       (just (random-number -1000000 1000000))
                       (just (random-number -1.0e6 1.0e6)))))
        (ok (data= (amap (amap (amap (just (curry #'compose)) b->*c) a->*b) a*)
                   (amap b->*c (amap a->*b a*))))
        '())))
  (testing "Homomorphism"
    (dotimes (i 1000)
      (mlet ((a->b (list (lambda (x) (* x x))))
             (a (list (random-number -1000000 1000000)
                      (random-number -1.0e6 1.0e6))))
        (ok (data= (amap (just a->b) (just a))
                   (just (funcall a->b a))))
        '())))
  (testing "Interchange"
    (dotimes (i 1000)
      (mlet ((a->*b (list (nothing)
                          (just (lambda (x) (* x x)))))
             (a (list (random-number -1000000 1000000)
                      (random-number -1.0e6 1.0e6))))
        (ok (data= (amap a->*b (just a))
                   (amap (just (lambda (a->b) (funcall a->b a))) a->*b)))
        '()))))

(deftest monad
  (testing "Left Identity"
    (dotimes (i 1000)
      (mlet ((a->b* (list (lambda (x) (just (* x x)))))
             (a (list (random-number -1000000 1000000)
                      (random-number -1.0e6 1.0e6))))
        (ok (data= (mmap a->b* (just a))
                   (funcall a->b* a)))
        '())))
  (testing "Right Identity"
    (dotimes (i 1000)
      (let ((a* (just (random-object))))
        (ok (data= (mmap #'just a*)
                   a*)))))
  (testing "Associativity"
    (dotimes (i 1000)
      (mlet ((a->b* (list (lambda (x) (just (* x x)))))
             (b->c* (list (lambda (x) (just (+ x x)))))
             (a* (list (just (random-number -1000000 1000000))
                       (just (random-number -1.0e6 1.0e6)))))
        (ok (data= (mmap (lambda (a) (mmap b->c* (funcall a->b* a))) a*)
                   (mmap b->c* (mmap a->b* a*))))
        '()))))

(deftest monoid
  (testing "Identity"
    (dotimes (i 1000)
      (mlet ((a* (list (nothing)
                       (just (random-object)))))
        (ok (and (data= (mplus (nothing) a*) a*)
                 (data= (mplus a* (nothing)) a*)))
        '())))
  (testing "Associativity"
    (dotimes (i 1000)
      (mlet ((a* (list (nothing)
                       (just (random-list 0 1000))))
             (b* (list (nothing)
                       (just (random-list 0 1000))))
             (c* (list (nothing)
                       (just (random-list 0 1000)))))
        (ok (data= (mplus (mplus a* b*) c*)
                   (mplus a* (mplus b* c*))))
        '()))))
