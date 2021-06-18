(defpackage fcl/tests.promise
  (:nicknames :fcl/tests.data.promise :fcl/t.pm)
  (:use :common-lisp :rove :fcl/tests.util :fcl.promise)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose #:partial #:curry))
(in-package fcl/tests.promise)


(deftest matching
  (testing "DELAY"
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (match (delay a)
              ((delay b) (equal a b))))))))

(deftest delay=unit
  (testing "Equality of DELAY and UNIT"
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (data= (delay a) (unit 'promise a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 1000)
      (let ((a* (delay (random-object))))
        (ok (data= (fmap #'identity a*)
                   a*)))))
  (testing "Composition"
    (dotimes (i 1000)
      (mlet ((a* (list (delay (random-number -1000000 1000000))
                       (delay (random-number -1.0e6 1.0e6))))
             (a->b (list (lambda (x) (* x x))))
             (b->c (list (lambda (x) (+ x x)))))
        (ok (data= (fmap (compose b->c a->b) a*)
                   (fmap b->c (fmap a->b a*))))
        '()))))

(deftest applicative
  (testing "Identity"
    (dotimes (i 1000)
      (let ((a* (delay (random-object))))
        (ok (data= (amap (delay #'identity) a*)
                   a*)))))
  (testing "Composition"
    (dotimes (i 1000)
      (mlet ((a->*b (list (delay (lambda (x) (* x x)))))
             (b->*c (list (delay (lambda (x) (+ x x)))))
             (a* (list (delay (random-number -1000000 1000000))
                       (delay (random-number -1.0e6 1.0e6)))))
        (ok (data= (amap (amap (amap (delay (curry #'compose)) b->*c) a->*b) a*)
                   (amap b->*c (amap a->*b a*))))
        '())))
  (testing "Homomorphism"
    (dotimes (i 1000)
      (mlet ((a->b (list (lambda (x) (* x x))))
             (a (list (random-number -1000000 1000000)
                      (random-number -1.0e6 1.0e6))))
        (ok (data= (amap (delay a->b) (delay a))
                   (delay (funcall a->b a))))
        '())))
  (testing "Interchange"
    (dotimes (i 1000)
      (mlet ((a->*b (list (delay (lambda (x) (* x x)))))
             (a (list (random-number -1000000 1000000)
                      (random-number -1.0e6 1.0e6))))
        (ok (data= (amap a->*b (delay a))
                   (amap (delay (lambda (a->b) (funcall a->b a))) a->*b)))
        '()))))

(deftest monad
  (testing "Left Identity"
    (dotimes (i 1000)
      (mlet ((a->b* (list (lambda (x) (delay (* x x)))))
             (a (list (random-number -1000000 1000000)
                      (random-number -1.0e6 1.0e6))))
        (ok (data= (mmap a->b* (delay a))
                   (funcall a->b* a)))
        '())))
  (testing "Right Identity"
    (dotimes (i 1000)
      (let ((a* (delay (random-object))))
        (ok (data= (mmap (lambda (a) (delay a)) a*)
                   a*)))))
  (testing "Associativity"
    (dotimes (i 1000)
      (mlet ((a->b* (list (lambda (x) (delay (* x x)))))
             (b->c* (list (lambda (x) (delay (+ x x)))))
             (a* (list (delay (random-number -1000000 1000000))
                       (delay (random-number -1.0e6 1.0e6)))))
        (ok (data= (mmap (lambda (a) (mmap b->c* (funcall a->b* a))) a*)
                   (mmap b->c* (mmap a->b* a*))))
        '()))))
