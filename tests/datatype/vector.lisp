(defpackage fcl/tests.vector
  (:nicknames :fcl/tests.data.vector :fcl/t.vc)
  (:use :common-lisp :rove :fcl/tests.util :fcl.vector)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose))
(in-package :fcl/tests.vector)


(deftest matching
  (testing "Empty Vector"
    (ok (match #()
          ((vector) t)
          (_ nil))))
  (testing "Simple Vectors"
    (dotimes (i 10)
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
    (dotimes (i 10)
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
              ((vector (vector _)
                       (vector _)
                       (vector _)
                       (vector _)
                       (vector _)
                       (vector _))
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
              ((vector (vector _)
                       (vector _)
                       (vector _)
                       (vector _)
                       (vector _)
                       (vector _))
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
              ((vector (vector _)
                       (vector _)
                       (vector _)
                       (vector _)
                       (vector _)
                       (vector _))
               nil)))
        (ok (match (vector (vector a)
                           (vector b)
                           (vector c)
                           (vector d)
                           (vector e)
                           (vector f))
              ((vector (vector _ _ _ _ _ _))
               nil)
              ((vector (vector _ _ _)
                       (vector _ _ _))
               nil)
              ((vector (vector _ _)
                       (vector _ _)
                       (vector _ _))
               nil)
              ((vector (vector u)
                       (vector v)
                       (vector w)
                       (vector x)
                       (vector y)
                       (vector z))
               (and (data= a u) (data= b v) (data= c w)
                    (data= d x) (data= e y) (data= f z)))))))))

(deftest empty=mzero
  (testing "Equality of empty VECTOR and MZERO"
    (ok (data= #() (mzero 'vector)))))

(deftest vector=unit
  (testing "Equality of single element VECTOR and unit"
    (dotimes (i 10)
      (let ((a (random-object)))
        (ok (data= (vector a) (unit 'vector a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 10)
      (mlet ((a* (list #()
                       (random-vector 1 500 :random-fn #'random-object))))
        (fcl/tests.functor:identity-test a*))))
  (testing "Composition"
    (dotimes (i 10)
      (mlet ((a* (list #() (random-vector 1 500))))
        (let ((a->b (random-function))
              (b->c (random-function)))
          (fcl/tests.functor:composition-test b->c a->b a*))))))

(deftest applicative
  (testing "Identity"
    (dotimes (i 10)
      (mlet ((a* (list #() (random-vector 1 500 :random-fn #'random-object))))
        (fcl/tests.applicative:identity-test 'vector a*))))
  (testing "Composition"
    (dotimes (i 10)
      (mlet ((a*    (list #()
                          (random-vector 1 200)))
             (a->*b (list #()
                          (random-vector 1 5 :random-fn #'random-function)
                          (coerce (functions) 'vector)))
             (b->*c (list #()
                          (random-vector 1 5 :random-fn #'random-function)
                          (coerce (functions) 'vector))))
        (fcl/tests.applicative:composition-test 'vector b->*c a->*b a*))))
  (testing "Homomorphism"
    (dotimes (i 10)
      (let ((a    (random-number -1.0d6 1.0d6))
            (a->b (random-function)))
        (fcl/tests.applicative:homomorphism-test 'vector a->b a))))
  (testing "Interchange"
    (dotimes (i 10)
      (let ((a (random-number -1.0d6 1.0d6)))
        (mlet ((a->*b (list #()
                            (coerce (random-list 1 5 :random-fn #'random-function)
                                    'simple-vector)
                            (coerce (functions) 'simple-vector))))
          (fcl/tests.applicative:interchange-test 'vector a->*b a))))))

(deftest monad
  (testing "Left Identity"
    (dotimes (i 10)
      (let ((a (random-number -1.0d6 1.0d6)))
        (mlet ((a->*b (list (random-vector 1 5 :random-fn #'random-function)
                            (coerce (functions) 'vector)))
               (a->b* (list (constantly #())
                            (lambda (a)
                              (fmap (lambda (a->b) (funcall a->b a)) a->*b)))))
          (fcl/tests.monad:left-identity-test 'vector a->b* a)))))
  (testing "Right Identity"
    (dotimes (i 10)
      (mlet ((a* (list #() (random-vector 1 500 :random-fn #'random-function))))
        (fcl/tests.monad:right-identity-test 'vector a*))))
  (testing "Associativity"
    (dotimes (i 10)
      (mlet ((a*    (list #() (random-vector 1 200)))
             (a->*b (list (random-vector 1 5 :random-fn #'random-function)
                          (coerce (functions) 'vector)))
             (b->*c (list (random-vector 1 5 :random-fn #'random-function)
                          (coerce (functions) 'vector)))
             (a->b* (list (constantly #())
                          (lambda (a)
                            (fmap (lambda (a->b) (funcall a->b a)) a->*b))))
             (b->c* (list (constantly #())
                          (lambda (b)
                            (fmap (lambda (b->c) (funcall b->c b)) b->*c)))))
        (fcl/tests.monad:associativity-test a->b* b->c* a*)))))

(deftest monoid
  (testing "Left Identity"
    (dotimes (i 10)
      (mlet ((a* (list #() (random-vector 1 500 :random-fn #'random-object))))
        (fcl/tests.monoid:left-identity-test 'vector a*))))
  (testing "Right Identity"
    (dotimes (i 10)
      (mlet ((a* (list #() (random-vector 1 500 :random-fn #'random-object))))
        (fcl/tests.monoid:right-identity-test 'vector a*))))
  (testing "Associativity"
    (dotimes (i 10)
      (mlet ((a* (list #() (random-vector 1 500 :random-fn #'random-object)))
             (b* (list #() (random-vector 1 500 :random-fn #'random-object)))
             (c* (list #() (random-vector 1 500 :random-fn #'random-object))))
        (fcl/tests.monoid:associativity-test a* b* c*)))))
