(defpackage :fcl/tests.datatypes.either
  (:nicknames :fcl/t.dt.either :fcl/t.either)
  (:use
    :common-lisp
    :rove
    :fcl.data
    :fcl.match
    :fcl.either
    :fcl.function))
(in-package :fcl/tests.datatypes.either)


(deftest match
  (testing "LEFT"
    (let ((a (random 1.0e9)))
      (ok (match (left a)
            ((left x) (= x a))
            ((right _) nil)))))
  (testing "RIGHT"
    (let ((a (random 1.0e9)))
      (ok (match (right a)
            ((left _) nil)
            ((right x) (= x a)))))))

(deftest either=mzero
  (testing "Equality of EITHER and MZERO"
    (ok (data= (left nil) (mzero 'either)))))

(deftest right=unit
  (testing "Equality of RIGHT and UNIT"
    (let ((a (random 1.0e9)))
      (ok (data= (right a) (unit 'either a))))))

(deftest functor
  (testing "Identity"
    (let ((a (random 1.0e9)))
      (ok (data= (fmap #'identity (left a))
                 (left a)))
      (ok (data= (fmap #'identity (right a))
                 (right a)))))
  (testing "Composition"
    (let ((a (random 1.0e9)))
      (ok (data= (fmap (lambda (x) (1+ (* x x))) (left a))
                 (fmap #'1+ (fmap (lambda (x) (* x x)) (left a)))))
      (ok (data= (fmap (lambda (x) (1+ (* x x))) (right a))
                 (fmap #'1+ (fmap (lambda (x) (* x x)) (right a))))))))

(deftest applicative
  (testing "Identity"
    (let ((a (random 1.0e9)))
      (ok (data= (amap (right #'identity) (left a))
                 (left a)))
      (ok (data= (amap (right #'identity) (right a))
                 (right a)))))
  (testing "Composition"
    (let ((f (lambda (x) (* x x)))
          (g (lambda (x) (+ x x)))
          (a (random 1.0e9)))
      ;; (left a) (left a) (left a)
      (ok (data= (amap (amap (amap (right (curry #'compose)) (left a)) (left a)) (left a))
                 (amap (left a) (amap (left a) (left a)))))
      ;; (left a) (left a) (right a)
      (ok (data= (amap (amap (amap (right (curry #'compose)) (left a)) (left a)) (right a))
                 (amap (left a) (amap (left a) (right a)))))
      ;; (left a) (right f) (left a)
      (ok (data= (amap (amap (amap (right (curry #'compose)) (left a)) (right f)) (left a))
                 (amap (left a) (amap (right f) (left a)))))
      ;; (left a) (right f) (right a)
      (ok (data= (amap (amap (amap (right (curry #'compose)) (left a)) (right f)) (right a))
                 (amap (left a) (amap (right f) (right a)))))
      ;; (right g) (left a) (left a)
      (ok (data= (amap (amap (amap (right (curry #'compose)) (right g)) (left a)) (left a))
                 (amap (right g) (amap (left a) (left a)))))
      ;; (right g) (left a) (right a)
      (ok (data= (amap (amap (amap (right (curry #'compose)) (right g)) (left a)) (right a))
                 (amap (right g) (amap (left a) (right a)))))
      ;; (right g) (right f) (left a)
      (ok (data= (amap (amap (amap (right (curry #'compose)) (right g)) (right f)) (left a))
                 (amap (right g) (amap (right f) (left a)))))
      ;; (right g) (right f) (right a)
      (ok (data= (amap (amap (amap (right (curry #'compose)) (right g)) (right f)) (right a))
                 (amap (right g) (amap (right f) (right a)))))))
  (testing "Homomorphism"
    (let ((f (lambda (x) (* x x)))
          (a (random 1.0e9)))
      (ok (data= (amap (left a) (left a))
                 (left a)))
      (ok (data= (amap (left a) (right a))
                 (left a)))
      (ok (data= (amap (right f) (left a))
                 (left a)))
      (ok (data= (amap (right f) (right a))
                 (right (funcall f a))))))
  (testing "Interchange"
    (let ((f (lambda (x) (* x x)))
          (a (random 1.0e9)))
      (ok (data= (amap (left a) (left a))
                 (amap (left a) (left a))))
      (ok (data= (amap (left a) (right a))
                 (amap (right (lambda (f) (funcall f a))) (left a))))
      (ok (data= (amap (right f) (left a))
                 (amap (left a) (right f))))
      (ok (data= (amap (right f) (right a))
                 (amap (right (lambda (f) (funcall f a))) (right f)))))))

(deftest monad
  (testing "Left Identity"
    (let ((f (lambda (x) (right (* x x))))
          (a (random 1.0e9)))
      (ok (data= (mmap f (left a))
                 (left a)))
      (ok (data= (mmap f (right a))
                 (funcall f a)))))
  (testing "Right Identity"
    (let ((a (random 1.0e9)))
      (ok (data= (mmap #'right (left a))
                 (left a)))
      (ok (data= (mmap #'right (right a))
                 (right a)))))
  (testing "Associativity"
    (let* ((a (random 1.0e9))
           (f (lambda (x) (right (* x x))))
           (f- (lambda (x) (declare (ignore x)) (left a)))
           (g (lambda (x) (right (+ x x))))
           (g- (lambda (x) (declare (ignore x)) (left a))))
      ;;; g- f- (left a)
      (ok (data= (mmap (lambda (a) (mmap g- (funcall f- a))) (left a))
                 (mmap g- (mmap f- (left a)))))
      ;;; g- f- (right a)
      (ok (data= (mmap (lambda (a) (mmap g- (funcall f- a))) (right a))
                 (mmap g- (mmap f- (right a)))))
      ;;; g- f (left a)
      (ok (data= (mmap (lambda (a) (mmap g- (funcall f a))) (left a))
                 (mmap g- (mmap f (left a)))))
      ;;; g- f (right a)
      (ok (data= (mmap (lambda (a) (mmap g- (funcall f a))) (right a))
                 (mmap g- (mmap f (right a)))))
      ;;; g f- (left a)
      (ok (data= (mmap (lambda (a) (mmap g (funcall f- a))) (left a))
                 (mmap g (mmap f- (left a)))))
      ;;; g f- (right a)
      (ok (data= (mmap (lambda (a) (mmap g (funcall f- a))) (right a))
                 (mmap g (mmap f- (right a)))))
      ;;; g f (left a)
      (ok (data= (mmap (lambda (a) (mmap g (funcall f a))) (left a))
                 (mmap g (mmap f (left a)))))
      ;;; g f (right a)
      (ok (data= (mmap (lambda (a) (mmap g (funcall f a))) (right a))
                 (mmap g (mmap f (right a))))))))

(deftest monoid
  (testing "Identity"
    (let ((a (random 1.0e9)))
      (ok (data= (mplus (left a) (right a))
                 (right a)))
      (ok (data= (mplus (right a) (left a))
                 (right a)))))
  (testing "Associativity"
    (let ((a (list (random 1.0e9)))
          (b (list (random 1.0e9)))
          (c (list (random 1.0e9))))
      ;; (left a) (left a) (left a)
      (ok (data= (mplus (mplus (left a) (left a)) (left a))
                 (mplus (left a) (mplus (left a) (left a)))))
      ;; (left a) (left a) (right c)
      (ok (data= (mplus (mplus (left a) (left a)) (right c))
                 (mplus (left a) (mplus (left a) (right c)))))
      ;; (left a) (right b) (left a)
      (ok (data= (mplus (mplus (left a) (right b)) (left a))
                 (mplus (left a) (mplus (right b) (left a)))))
      ;; (left a) (right b) (right c)
      (ok (data= (mplus (mplus (left a) (right b)) (right c))
                 (mplus (left a) (mplus (right b) (right c)))))
      ;; (right a) (left a) (left a)
      (ok (data= (mplus (mplus (right a) (left a)) (left a))
                 (mplus (right a) (mplus (left a) (left a)))))
      ;; (right a) (left a) (right c)
      (ok (data= (mplus (mplus (right a) (left a)) (right c))
                 (mplus (right a) (mplus (left a) (right c)))))
      ;; (right a) (right b) (left a)
      (ok (data= (mplus (mplus (right a) (right b)) (left a))
                 (mplus (right a) (mplus (right b) (left a)))))
      ;; (right a) (right b) (right c)
      (ok (data= (mplus (mplus (right a) (right b)) (right c))
                 (mplus (right a) (mplus (right b) (right c))))))))

