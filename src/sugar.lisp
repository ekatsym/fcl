(defpackage fcl.sugar
  (:use :common-lisp)
  (:import-from
    :fcl
    #:monad-do
    #:mlet
    #:mprogn
    #:genlist)
  (:import-from
    :fcl.util
    #:nlist?)
  (:import-from
    :fcl.monad
    #:unit
    #:mmap)
  (:export
    #:monad-do
    #:mlet
    #:mprogn
    #:genlist))
(in-package :fcl.sugar)


(defmacro monad-do (&body clauses)
  (reduce (lambda (clause body)
            (if (listp clause)
                (case (first clause)
                  (:in
                    (assert (nlist? 3 clause) (clause))
                    `(mlet ((,(second clause) ,(third clause))) ,body))
                  (:is
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

(defmacro genlist (element &rest clauses)
  (reduce (lambda (clause body)
            (if (listp clause)
                (case (first clause)
                  (:in
                    (assert (nlist? 3 clause) (clause))
                    `(mlet ((,(second clause) ,(third clause))) ,body))
                  (:is
                    (assert (nlist? 3 clause) (clause))
                    `(let ((,(second clause) ,(third clause))) ,body))
                  (otherwise
                    `(mprogn (guard 'list ,clause) ,body)))
                `(mprogn (guard 'list ,clause) ,body)))
          clauses
          :initial-value `(unit 'list ,element)
          :from-end t))
