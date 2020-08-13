(defpackage fcl.monad
  (:use :common-lisp)
  (:import-from
    :fcl
    #:mmap
    #:monad-do
    #:mprogn
    #:mlet
    #:define-fmap-by-monad
    #:define-amap-by-monad)
  (:import-from
    :fcl.applicative
    #:unit
    #:fmap
    #:amap)
  (:import-from
    :fcl.util
    #:nlist?)
  (:export
    #:unit
    #:fmap
    #:amap
    #:mmap
    #:monad-do
    #:mprogn
    #:mlet
    #:define-fmap-by-monad
    #:define-amap-by-monad))

(in-package :fcl.monad)


;;; Core
(defgeneric mmap (a->b* a*)
  (:documentation
"Returns a value of class of B*, \"appended\" (FMAP A->B* A)
where A is a value included A*.
MMAP must satisfy the rules:
  Left identity:  (mmap #'a->b* (unit class a))
               == (a->b* a)
  Right identity: (mmap (partial #'unit class) a*)
               == a*
  Associativity:  (mmap (lambda (a) (mmap #'b->c* (a->b* a))) a*)
               == (mmap #'b->c* (mmap #'a->b* a*))"))

(defmacro monad-do (&body clauses)
  (reduce (lambda (clause body)
            (if (listp clause)
                (case (first clause)
                  (:in
                    (assert (nlist? 3 clause) (clause))
                    `(mlet ((,(second clause) ,(third clause))) ,body))
                  (:let
                    (assert (nlist? 3 clause) (clause))
                    `(let ((,(second clause) ,(third clause))) ,body))
                  (otherwise
                    `(mprogn ,clause ,body)))
                `(mprogn ,clause ,body)))
          clauses
          :from-end t))

(defmacro mprogn (&rest monads)
  (let ((g!_ (gensym "G!_")))
    (reduce (lambda (m body)
              `(mmap (lambda (,g!_)
                       (declare (ignore ,g!_))
                       ,body)
                     ,m))
            monads
            :from-end t)))

(defmacro mlet ((&rest bindings) &body body)
  (every (lambda (binding)
           (check-type binding list)
           (assert (nlist? 2 binding) (binding)))
         bindings)
  (reduce (lambda (binding monad)
            (destructuring-bind (v m) binding
              `(mmap (lambda (,v) ,monad) ,m)))
          (if (null bindings)
              bindings
              (butlast bindings))
          :initial-value (if (null bindings)
                             `(let () ,@body)
                             (let ((binding (first (last bindings))))
                               (destructuring-bind (v m) binding
                                 `(mmap (lambda (,v) ,@body) ,m))))
          :from-end t))


;;; Utility for Functor and Applicative
(defmacro define-fmap-by-monad (class)
  `(defmethod fmap (a->b (a* ,class))
     (check-type a->b function)
     (mlet ((a a*))
       (unit ',class (funcall a->b a)))))

(defmacro define-amap-by-monad (class)
  `(defmethod amap (a->*b (a* ,class))
     (check-type a->*b ,class)
     (mlet ((a->b a->*b)
            (a a*))
       (check-type a->b function)
       (unit ',class (funcall a->b a)))))


;;; General Utility
(defun join-m (a**)
  (mmap #'identity a**))

(defun sequence-m (class a*s)
  (check-type class symbol)
  (check-type a*s list)
  (reduce (lambda (a* as*)
            (mlet ((a a*)
                   (as as*))
              (unit class (cons a as))))
          a*s
          :initial-value (unit class '())
          :from-end t))

(defun map-m (class a->b* as)
  (check-type class symbol)
  (check-type a->b* function)
  (check-type as list)
  (reduce (lambda (a bs*)
            (mlet ((bs bs*))
              (unit class (cons (funcall a->b* a) bs))))
          as
          :initial-value '()
          :from-end t))

(defun map-m (class a->b* as)
  (check-type class symbol)
  (check-type a->b* function)
  (check-type as list)
  (reduce (lambda (a bs*)
            (mlet ((bs bs*))
              (unit class (cons (funcall a->b* a) bs))))
          as
          :initial-value (unit class '())
          :from-end t))

(defun map-m_ (class a->b* as)
  (check-type class symbol)
  (check-type a->b* function)
  (check-type as list)
  (reduce (lambda (a _)
            (mprogn
              (unit class (funcall a->b* a))
              _))
          as
          :initial-value (unit class '())
          :from-end t))

(declaim (inline for-m))
(defun for-m (class as a->b*)
  (check-type class symbol)
  (check-type as list)
  (check-type a->b* function)
  (map-m class a->b* as))

(declaim (inline for-m_))
(defun for-m_ (class as a->b*)
  (check-type class symbol)
  (check-type as list)
  (check-type a->b* function)
  (map-m_ class a->b* as))
