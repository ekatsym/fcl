(defpackage :fcl/tests.datatypes.maybe
  (:nicknames :fcl/t.dt.maybe :fcl/t.maybe)
  (:use
    :common-lisp
    :rove
    :fcl.data
    :fcl.match
    :fcl.maybe
    :fcl.function))
(in-package :fcl/tests.datatypes.maybe)


(deftest match
  (testing "NOTHING"
    (ok (match (nothing)
          ((nothing) t)
          ((just _) nil))))
  (testing "JUST"
    (ok (let ((a (random 1.0e9)))
          (match (just a)
            ((nothing) nil)
            ((just b) (= a b)))))))

(deftest nothing=mzero
  (testing "Equality of NOTHING and MZERO"
    (ok (data= (nothing) (mzero 'maybe)))))

(deftest just=unit
  (testing "Equality of JUST and UNIT"
    (let ((a (random 1.0e9)))
      (ok (data= (just a) (unit 'maybe a))))))

(deftest functor
  (testing "Identity"
    (let ((a (random 1.0e9)))
      (ok (data= (fmap #'identity (nothing))
                 (nothing)))
      (ok (data= (fmap #'identity (just a))
                 (just a)))))
  (testing "Composition"
    (let ((a (random 1.0e9)))
      (ok (data= (fmap (lambda (x) (1+ (* x x))) (nothing))
                 (fmap #'1+ (fmap (lambda (x) (* x x)) (nothing)))))
      (ok (data= (fmap (lambda (x) (1+ (* x x))) (just a))
                 (fmap #'1+ (fmap (lambda (x) (* x x)) (just a))))))))

(deftest applicative
  (testing "Identity"
    (let ((a (random 1.0e9)))
      (ok (data= (amap (just #'identity) (nothing))
                 (nothing)))
      (ok (data= (amap (just #'identity) (just a))
                 (just a)))))
  (testing "Composition"
    (let ((f (lambda (x) (* x x)))
          (g (lambda (x) (+ x x)))
          (a (random 1.0e9)))
      ;; (nothing) (nothing) (nothing)
      (ok (data= (amap (amap (amap (just (curry #'compose)) (nothing)) (nothing)) (nothing))
                 (amap (nothing) (amap (nothing) (nothing)))))
      ;; (nothing) (nothing) (just a)
      (ok (data= (amap (amap (amap (just (curry #'compose)) (nothing)) (nothing)) (just a))
                 (amap (nothing) (amap (nothing) (just a)))))
      ;; (nothing) (just f) (nothing)
      (ok (data= (amap (amap (amap (just (curry #'compose)) (nothing)) (just f)) (nothing))
                 (amap (nothing) (amap (just f) (nothing)))))
      ;; (nothing) (just f) (just a)
      (ok (data= (amap (amap (amap (just (curry #'compose)) (nothing)) (just f)) (just a))
                 (amap (nothing) (amap (just f) (just a)))))
      ;; (just g) (nothing) (nothing)
      (ok (data= (amap (amap (amap (just (curry #'compose)) (just g)) (nothing)) (nothing))
                 (amap (just g) (amap (nothing) (nothing)))))
      ;; (just g) (nothing) (just a)
      (ok (data= (amap (amap (amap (just (curry #'compose)) (just g)) (nothing)) (just a))
                 (amap (just g) (amap (nothing) (just a)))))
      ;; (just g) (just f) (nothing)
      (ok (data= (amap (amap (amap (just (curry #'compose)) (just g)) (just f)) (nothing))
                 (amap (just g) (amap (just f) (nothing)))))
      ;; (just g) (just f) (just a)
      (ok (data= (amap (amap (amap (just (curry #'compose)) (just g)) (just f)) (just a))
                 (amap (just g) (amap (just f) (just a)))))))
  (testing "Homomorphism"
    (let ((f (lambda (x) (* x x)))
          (a (random 1.0e9)))
      (ok (data= (amap (nothing) (nothing))
                 (nothing)))
      (ok (data= (amap (nothing) (just a))
                 (nothing)))
      (ok (data= (amap (just f) (nothing))
                 (nothing)))
      (ok (data= (amap (just f) (just a))
                 (just (funcall f a))))))
  (testing "Interchange"
    (let ((f (lambda (x) (* x x)))
          (a (random 1.0e9)))
      (ok (data= (amap (nothing) (nothing))
                 (amap (nothing) (nothing))))
      (ok (data= (amap (nothing) (just a))
                 (amap (just (lambda (f) (funcall f a))) (nothing))))
      (ok (data= (amap (just f) (nothing))
                 (amap (nothing) (just f))))
      (ok (data= (amap (just f) (just a))
                 (amap (just (lambda (f) (funcall f a))) (just f)))))))

(deftest monad
  (testing "Left Identity"
    (let ((f (lambda (x) (just (* x x))))
          (a (random 1.0e9)))
      (ok (data= (mmap f (nothing))
                 (nothing)))
      (ok (data= (mmap f (just a))
                 (funcall f a)))))
  (testing "Right Identity"
    (let ((a (random 1.0e9)))
      (ok (data= (mmap #'just (nothing))
                 (nothing)))
      (ok (data= (mmap #'just (just a))
                 (just a)))))
  (testing "Associativity"
    (let ((f (lambda (x) (just (* x x))))
          (f- (lambda (x) (declare (ignore x)) (nothing)))
          (g (lambda (x) (just (+ x x))))
          (g- (lambda (x) (declare (ignore x)) (nothing)))
          (a (random 1.0e9)))
      ;;; g- f- (nothing)
      (ok (data= (mmap (lambda (a) (mmap g- (funcall f- a))) (nothing))
                 (mmap g- (mmap f- (nothing)))))
      ;;; g- f- (just a)
      (ok (data= (mmap (lambda (a) (mmap g- (funcall f- a))) (just a))
                 (mmap g- (mmap f- (just a)))))
      ;;; g- f (nothing)
      (ok (data= (mmap (lambda (a) (mmap g- (funcall f a))) (nothing))
                 (mmap g- (mmap f (nothing)))))
      ;;; g- f (just a)
      (ok (data= (mmap (lambda (a) (mmap g- (funcall f a))) (just a))
                 (mmap g- (mmap f (just a)))))
      ;;; g f- (nothing)
      (ok (data= (mmap (lambda (a) (mmap g (funcall f- a))) (nothing))
                 (mmap g (mmap f- (nothing)))))
      ;;; g f- (just a)
      (ok (data= (mmap (lambda (a) (mmap g (funcall f- a))) (just a))
                 (mmap g (mmap f- (just a)))))
      ;;; g f (nothing)
      (ok (data= (mmap (lambda (a) (mmap g (funcall f a))) (nothing))
                 (mmap g (mmap f (nothing)))))
      ;;; g f (just a)
      (ok (data= (mmap (lambda (a) (mmap g (funcall f a))) (just a))
                 (mmap g (mmap f (just a))))))))

(deftest monoid
  (testing "Identity"
    (let ((a (random 1.0e9)))
      (ok (data= (mplus (nothing) (just a))
                 (just a)))
      (ok (data= (mplus (just a) (nothing))
                 (just a)))))
  (testing "Associativity"
    (let ((a (list (random 1.0e9)))
          (b (list (random 1.0e9)))
          (c (list (random 1.0e9))))
      ;; (nothing) (nothing) (nothing)
      (ok (data= (mplus (mplus (nothing) (nothing)) (nothing))
                 (mplus (nothing) (mplus (nothing) (nothing)))))
      ;; (nothing) (nothing) (just c)
      (ok (data= (mplus (mplus (nothing) (nothing)) (just c))
                 (mplus (nothing) (mplus (nothing) (just c)))))
      ;; (nothing) (just b) (nothing)
      (ok (data= (mplus (mplus (nothing) (just b)) (nothing))
                 (mplus (nothing) (mplus (just b) (nothing)))))
      ;; (nothing) (just b) (just c)
      (ok (data= (mplus (mplus (nothing) (just b)) (just c))
                 (mplus (nothing) (mplus (just b) (just c)))))
      ;; (just a) (nothing) (nothing)
      (ok (data= (mplus (mplus (just a) (nothing)) (nothing))
                 (mplus (just a) (mplus (nothing) (nothing)))))
      ;; (just a) (nothing) (just c)
      (ok (data= (mplus (mplus (just a) (nothing)) (just c))
                 (mplus (just a) (mplus (nothing) (just c)))))
      ;; (just a) (just b) (nothing)
      (ok (data= (mplus (mplus (just a) (just b)) (nothing))
                 (mplus (just a) (mplus (just b) (nothing)))))
      ;; (just a) (just b) (just c)
      (ok (data= (mplus (mplus (just a) (just b)) (just c))
                 (mplus (just a) (mplus (just b) (just c))))))))
