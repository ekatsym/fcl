(defpackage :fcl/tests.either
  (:nicknames :fcl/tests.data.either :fcl/t.et)
  (:use :common-lisp :rove :fcl/tests.util :fcl.either)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose #:partial #:curry))
(in-package :fcl/tests.either)


(deftest matching
  (testing "LEFT"
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (match (left a)
              ((left b)  (data= a b))
              ((right b) nil))))))
  (testing "RIGHT"
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (match (right a)
              ((left _)  nil)
              ((right b) (data= a b))))))))

(deftest left=mzero
  (testing "Equality of LEFT and MZERO"
    (ok (data= (left nil) (mzero 'either)))))

(deftest right=unit
  (testing "Equality of RIGHT and UNIT"
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (data= (right a) (unit 'either a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 1000)
      (mlet ((a* (list (left (random-object))
                       (right (random-object)))))
        (ok (data= (fmap #'identity a*)
                   a*))
        '())))
  (testing "Composition"
    (dotimes (i 1000)
      (mlet ((a* (list (left (random-object))
                       (right (random-number -1000000 1000000))
                       (right (random-number -1.0e6 1.0e6))))
             (a->b (list (lambda (x) (* x x))))
             (b->c (list (lambda (x) (+ x x)))))
        (ok (data= (fmap (compose b->c a->b) a*)
                   (fmap b->c (fmap a->b a*))))
        '()))))

(deftest applicative
  (testing "Identity"
    (dotimes (i 1000)
      (mlet ((a* (list (left (random-object))
                       (right (random-object)))))
        (ok (data= (amap (right #'identity) a*)
                   a*))
        '())))
  (testing "Composition"
    (dotimes (i 1000)
      (mlet ((a->*b (list (left (random-object))
                          (right (lambda (x) (* x x)))))
             (b->*c (list (left (random-object))
                          (right (lambda (x) (+ x x)))))
             (a* (list (left (random-object))
                       (right (random-number -1000000 1000000))
                       (right (random-number -1.0e6 1.0e6)))))
        (ok (data= (amap (amap (amap (right (curry #'compose)) b->*c) a->*b) a*)
                   (amap b->*c (amap a->*b a*))))
        '())))
  (testing "Homomorphism"
    (dotimes (i 1000)
      (mlet ((a->b (list (lambda (x) (* x x))))
             (a (list (random-number -1000000 1000000)
                      (random-number -1.0e6 1.0e6))))
        (ok (data= (amap (right a->b) (right a))
                   (right (funcall a->b a))))
        '())))
  (testing "Interchange"
    (dotimes (i 1000)
      (mlet ((a->*b (list (left (random-object))
                          (right (lambda (x) (* x x)))))
             (a (list (random-number -1000000 1000000)
                      (random-number -1.0e6 1.0e6))))
        (ok (data= (amap a->*b (right a))
                   (amap (right (lambda (a->b) (funcall a->b a))) a->*b)))
        '()))))

(deftest monad
  (testing "Left Identity"
    (dotimes (i 1000)
      (mlet ((a->b* (list (lambda (x) (right (* x x)))))
             (a (list (random-number -1000000 1000000)
                      (random-number -1.0e6 1.0e6))))
        (ok (data= (mmap a->b* (right a))
                   (funcall a->b* a)))
        '())))
  (testing "Right Identity"
    (dotimes (i 1000)
      (let ((a* (right (random-object))))
        (ok (data= (mmap #'right a*)
                   a*)))))
  (testing "Associativity"
    (dotimes (i 1000)
      (mlet ((a->b* (list (lambda (x) (right (* x x)))))
             (b->c* (list (lambda (x) (right (+ x x)))))
             (a* (list (right (random-number -1000000 1000000))
                       (right (random-number -1.0e6 1.0e6)))))
        (ok (data= (mmap (lambda (a) (mmap b->c* (funcall a->b* a))) a*)
                   (mmap b->c* (mmap a->b* a*))))
        '()))))

(deftest monoid
  (testing "Identity"
    (dotimes (i 1000)
      (mlet ((a* (list (left (random-list 0 1000))
                       (right (random-object)))))
        (ok (and (data= (mplus (left nil) a*) a*)
                 (data= (mplus a* (left nil)) a*)))
        '())))
  (testing "Associativity"
    (dotimes (i 1000)
      (mlet ((a* (list (left (random-list 0 10))
                       (right (random-list 0 1000))))
             (b* (list (left (random-list 0 10))
                       (right (random-list 0 1000))))
             (c* (list (left (random-list 0 10))
                       (right (random-list 0 1000)))))
        (ok (data= (mplus (mplus a* b*) c*)
                   (mplus a* (mplus b* c*))))
        '()))))
