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
      (let ((a (random-object)))
        (ok (data= (fmap #'identity (delay a)) (delay a))))))
  (testing "Composition"
    (dotimes (i 1000)
      (let ((a (if (zerop (random 2))
                   (random-number -1000000 1000000)
                   (random-number -1.0e6 1.0e6))))
        (ok (data= (fmap (lambda (x) (1+ (* x x))) (delay a))
                   (fmap #'1+ (fmap (lambda (x) (* x x)) (delay a))))))
      (let ((a (random-character)))
        (ok (data= (fmap (compose #'code-char #'1+ #'char-code) (delay a))
                   (fmap (compose #'code-char #'1+) (fmap #'char-code (delay a))))))
      (let ((a (random-list 0 1000)))
        (ok (data= (fmap (compose #'reverse (partial #'mapcar #'1+)) (delay a))
                   (fmap #'reverse (fmap (partial #'mapcar #'1+) (delay a))))))
      (let ((a (random-string 0 1000)))
        (ok (data= (fmap (compose #'reverse (partial #'map 'string #'char-upcase))
                         (delay a))
                   (fmap #'reverse
                         (fmap (partial #'map 'string #'char-upcase) (delay a)))))))))

(deftest applicative
  (testing "Identity"
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (data= (amap (delay #'identity) (delay a))
                   (delay a))))))
  (testing "Composition"
    (dotimes (i 1000)
      (let ((f (lambda (x) (* x x)))
            (g (lambda (x) (+ x x)))
            (a (if (zerop (random 2))
                   (random-number -1000000 1000000)
                   (random-number -1.0e6 1.0e6))))
        (ok (data= (amap (amap (amap (delay (curry #'compose))
                                     (delay g))
                               (delay f))
                         (delay a))
                   (amap (delay g) (amap (delay f) (delay a))))))))
  (testing "Homomorphism"
    (dotimes (i 1000)
      (let ((f (lambda (x) (* x x)))
            (a (if (zerop (random 2))
                   (random-number -1000000 1000000)
                   (random-number -1.0e6 1.0e6))))
        (ok (data= (amap (delay f) (delay a))
                   (delay (funcall f a)))))))
  (testing "Interchange"
    (dotimes (i 1000)
      (let ((f (lambda (x) (* x x)))
            (a (if (zerop (random 2))
                   (random-number -1000000 1000000)
                   (random-number -1.0e6 1.0e6))))
        (ok (data= (amap (delay f) (delay a))
                   (amap (delay (lambda (f) (funcall f a))) (delay f))))))))

(deftest monad
  (testing "Left Identity"
    (dotimes (i 1000)
      (let ((f (lambda (x) (delay (* x x))))
            (a (if (zerop (random 2))
                   (random-number -1000000 1000000)
                   (random-number -1.0e6 1.0e6))))
        (ok (data= (mmap f (delay a))
                   (funcall f a))))))
  (testing "Right Identity"
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (data= (mmap (lambda (a) (delay a)) (delay a))
                   (delay a))))))
  (testing "Associativity"
    (dotimes (i 1000)
      (let ((f (lambda (x) (delay (* x x))))
            (g (lambda (x) (delay (+ x x))))
            (a (if (zerop (random 2))
                   (random-number -1000000 1000000)
                   (random-number -1.0e6 1.0e6))))
        (ok (data= (mmap (lambda (a) (mmap g (funcall f a))) (delay a))
                   (mmap g (mmap f (delay a)))))))))
