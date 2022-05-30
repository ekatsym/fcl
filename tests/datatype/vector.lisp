(defpackage fcl/tests.vector
  (:nicknames :fcl/tests.data.vector :fcl/t.vc)
  (:use :common-lisp :fiveam :fcl/tests.util :fcl.vector)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose))
(in-package :fcl/tests.vector)


(defun gen-vector (&key (length (gen-integer :min 0 :max 10)) (elements (gen-integer :min -10 :max 10)))
  (lambda ()
    (let* ((len (funcall length))
           (vec (make-array len)))
      (dotimes (i len)
        (setf (svref vec i) (funcall elements)))
      vec)))

(def-suite* vector-tests :in :fcl/tests)

(def-suite* patten-match :in vector-tests)

(test empty-vector
  (match #()
    (#() (pass))
    (_ (fail)))
  (match #()
    ((vector) (pass))
    (_ (fail))))

(test simple-vectors
  (for-all ((a (gen-object))
            (b (gen-object))
            (c (gen-object)))
    (match (vector a)
      ((vector)       (fail))
      ((vector x)     (is (data= a x)))
      ((vector _ _)   (fail))
      ((vector _ _ _) (fail)))
    (match (vector a b)
      ((vector)       (fail))
      ((vector _)     (fail))
      ((vector x y)   (is (and (data= a x) (data= b y))))
      ((vector _ _ _) (fail)))
    (match (vector a b c)
      ((vector)       (fail))
      ((vector _)     (fail))
      ((vector _ _)   (fail))
      ((vector x y z) (is (and (data= a x) (data= b y) (data= c z)))))))

(test nested-vectors
  (for-all ((a (gen-object))
            (b (gen-object))
            (c (gen-object))
            (d (gen-object))
            (e (gen-object))
            (f (gen-object)))
    (match (vector (vector a b c d e f))
      ((vector (vector u v w x y z))
       (is (and (data= a u) (data= b v) (data= c w)
                (data= d x) (data= e y) (data= f z))))
      ((vector (vector _ _ _)
               (vector _ _ _))
       (fail))
      ((vector (vector _ _)
               (vector _ _)
               (vector _ _))
       (fail))
      ((vector (vector _)
               (vector _)
               (vector _)
               (vector _)
               (vector _)
               (vector _))
       (fail))
      (_ (fail)))
    (match (vector (vector a b c)
                   (vector d e f))
      ((vector (vector _ _ _ _ _ _))
       (fail))
      ((vector (vector u v w)
               (vector x y z))
       (is (and (data= a u) (data= b v) (data= c w)
                (data= d x) (data= e y) (data= f z))))
      ((vector (vector _ _)
               (vector _ _)
               (vector _ _))
       (fail))
      ((vector (vector _)
               (vector _)
               (vector _)
               (vector _)
               (vector _)
               (vector _))
       (fail))
      (_ (fail)))
    (match (vector (vector a b)
                   (vector c d)
                   (vector e f))
      ((vector (vector _ _ _ _ _ _))
       (fail))
      ((vector (vector _ _ _)
               (vector _ _ _))
       (fail))
      ((vector (vector u v)
               (vector w x)
               (vector y z))
       (is (and (data= a u) (data= b v) (data= c w)
                (data= d x) (data= e y) (data= f z))))
      ((vector (vector _)
               (vector _)
               (vector _)
               (vector _)
               (vector _)
               (vector _))
       (fail))
      (_ (fail)))
    (match (vector (vector a)
                   (vector b)
                   (vector c)
                   (vector d)
                   (vector e)
                   (vector f))
      ((vector (vector _ _ _ _ _ _))
       (fail))
      ((vector (vector _ _ _)
               (vector _ _ _))
       (fail))
      ((vector (vector _ _)
               (vector _ _)
               (vector _ _))
       (fail))
      ((vector (vector u)
               (vector v)
               (vector w)
               (vector x)
               (vector y)
               (vector z))
       (is (and (data= a u) (data= b v) (data= c w)
                (data= d x) (data= e y) (data= f z))))
      (_ (fail)))))

(def-suite* monad-plus :in vector-tests)

(test mzero=empty
  (is (data= (mzero 'vector) #())))

(test unit=vector
  (for-all ((a (gen-object)))
    (is (data= (unit 'vector a) (vector a)))))

(fcl/tests.functor:functor-test
  vector1
  (gen-vector :length (gen-integer :min 0 :max 30)
              :elements (gen-object))
  (gen-function)
  (gen-function))

(fcl/tests.functor:functor-test
  vector2
  (gen-vector :length (gen-integer :min 0 :max 30)
              :elements (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.applicative:applicative-test
  vector1
  vector
  (gen-object)
  (gen-vector :length (gen-integer :min 0 :max 30)
              :elements (gen-object))
  (gen-function)
  (gen-function))

(fcl/tests.applicative:applicative-test
  vector2
  vector
  (gen-integer :min -100 :max 100)
  (gen-vector :length (gen-integer :min 0 :max 30)
              :elements (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.monad:monad-test
  vector1
  vector
  (gen-object)
  (gen-vector :length (gen-integer :min 0 :max 30)
              :elements (gen-object))
  (gen-one-element
    (constantly #())
    #'vector
    (lambda (a) (make-array 10 :initial-element a))
    (lambda (a) (vector a (list a) (vector a))))
  (gen-one-element
    (constantly #())
    #'vector
    (lambda (b) (make-array 20 :initial-element b))
    (lambda (b) (vector b (vector b b) (vector b b b)))))

(fcl/tests.monad:monad-test
  vector2
  vector
  (gen-integer :min -100 :max 100)
  (gen-vector :length (gen-integer :min 0 :max 30)
              :elements (gen-integer :min -100 :max 100))
  (gen-one-element
    (constantly #())
    #'vector
    (lambda (a) (unfoldr 'vector #'zerop (lambda (x) (+ a x -5)) #'1- 10))
    (lambda (a) (concatenate 'vector
                             (unfoldr 'vector #'zerop (lambda (x) (* a x)) #'1- 5)
                             (unfoldr 'vector #'zerop (lambda (x) (- (* a x))) #'1- 5))))
  (gen-one-element
    (constantly #())
    #'vector
    (lambda (b) (unfoldr 'vector #'zerop (lambda (x) (+ b x -5)) #'1- 10))
    (lambda (b) (concatenate 'vector
                             (unfoldr 'vector #'zerop (lambda (x) (* b x)) #'1- 5)
                             (unfoldr 'vector #'zerop (lambda (x) (- (* b x))) #'1- 5)))))

(fcl/tests.monoid:monoid-test
  vector
  vector
  (gen-vector :length (gen-integer :min 0 :max 30)
              :elements (gen-object)))
