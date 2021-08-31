(defpackage fcl/tests.vector
  (:nicknames :fcl/tests.data.vector :fcl/t.vc)
  (:use :common-lisp :rove :fcl/tests.util :fcl.vector)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose))
(in-package :fcl/tests.vector)


#|
(deftest matching
  (testing "Empty Vector"
    (ok (match #()
          ((vector) t)
          (_ nil))))
  (testing "Simple Vectors"
    (dotimes (i 100)
      (let ((a (random-object))
            (b (random-object))
            (c (random-object)))
        (ok (match (vector a)
              ((vector)       nil)
              ((vector x)     (data= a x))
              ((vector _ _)   nil)
              ((vector _ _ _) nil)))
        (ok (match (vector a b)
              ((vector)       nil)
              ((vector _)     nil)
              ((vector x y)   (and (data= a x) (data= b y)))
              ((vector _ _ _) nil)))
        (ok (match (vector a b c)
              ((vector)       nil)
              ((vector _)     nil)
              ((vector _ _)   nil)
              ((vector x y z) (and (data= a x) (data= b y) (data= c z))))))))
  (testing "Nested Vectors"
    (dotimes (i 100)
      (let ((a (random-object))
            (b (random-object))
            (c (random-object))
            (d (random-object))
            (e (random-object))
            (f (random-object)))
        (ok (match (vector (vector a b c d e f))
              ((vector (vector u v w x y z))
               (and (data= a u) (data= b v) (data= c w)
                    (data= d x) (data= e y) (data= f z)))
              ((vector (vector _ _ _)
                       (vector _ _ _))
               nil)
              ((vector (vector _ _)
                       (vector _ _)
                       (vector _ _))
               nil)
              ((vector _
                       (vector _ _)
                       (vector _
                               (vector _ _)))
               nil)))
        (ok (match (vector (vector a b c)
                           (vector d e f))
              ((vector (vector _ _ _ _ _ _))
               nil)
              ((vector (vector u v w)
                       (vector x y z))
               (and (data= a u) (data= b v) (data= c w)
                    (data= d x) (data= e y) (data= f z)))
              ((vector (vector _ _)
                       (vector _ _)
                       (vector _ _))
               nil)
              ((vector _
                       (vector _ _)
                       (vector _
                               (vector _ _)))
               nil)))
        (ok (match (vector (vector a b)
                           (vector c d)
                           (vector e f))
              ((vector (vector _ _ _ _ _ _))
               nil)
              ((vector (vector _ _ _)
                       (vector _ _ _))
               nil)
              ((vector (vector u v)
                       (vector w x)
                       (vector y z))
               (and (data= a u) (data= b v) (data= c w)
                    (data= d x) (data= e y) (data= f z)))
              ((vector _
                       (vector _ _)
                       (vector _
                               (vector _ _)))
               nil)))
        (ok (match (vector a
                           (vector b c)
                           (vector d
                                   (vector e f)))
              ((vector (vector _ _ _ _ _ _))
               nil)
              ((vector (vector _ _ _)
                       (vector _ _ _))
               nil)
              ((vector (vector _ _)
                       (vector _ _)
                       (vector _ _))
               nil)
              ((vector u
                       (vector v w)
                       (vector x
                               (vector y z)))
               (and (data= a u) (data= b v) (data= c w)
                    (data= d x) (data= e y) (data= f z)))))))))

(deftest empty=mzero
  (testing "Equality of empty VECTOR and MZERO"
    (ok (data= #() (mzero 'vector)))))

(deftest vector=unit
  (testing "Equality of single element VECTOR and unit"
    (dotimes (i 100)
      (let ((a (random-object)))
        (ok (data= (vector a) (unit 'vector a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 100)
      (mlet ((a* (list '()
                       (random-list 1 1000))))
        (ok (data= (fmap #'identity a*)
                   a*))
        '())))
  (testing "Composition"
    (dotimes (i 100)
      (mlet ((a* (list '()
                       (random-list 1 1000))))
        (let ((a->b (lambda (x) (* x x)))
              (b->c (lambda (x) (+ x x))))
          (ok (data= (fmap (compose b->c a->b) a*)
                     (fmap b->c (fmap a->b a*))))
          '())))))
|#
