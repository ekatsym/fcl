(defpackage fcl.monad
  (:use :common-lisp)
  (:import-from
    :fcl
    #:mmap
    #:monad-do
    #:mprogn
    #:mlet)
  (:import-from
    :fcl.applicative
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
    #:mlet))
(in-package :fcl.monad)


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
          bindings
          :initial-value (if (nlist? 1 body)
                             (first body)
                             `(progn ,@body))
          :from-end t))
