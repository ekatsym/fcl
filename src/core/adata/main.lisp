(defpackage fcl.adata
  (:nicknames :fcl.core.adata :fcl.dt)
  (:use :common-lisp :fcl.adata.util :fcl.adata.parser)
  (:import-from :fcl.util #:rpartial #:proper-list)
  (:import-from :fcl.lazy #:promise #:force)
  (:export
    #:algebraic-datatype
    #:defdata
    #:data=))
(in-package :fcl.adata)

(defstruct (algebraic-datatype (:constructor nil)
                               (:copier nil)
                               (:predicate nil)))

(defmacro defdata (name &body constructors)
  "A macro to define structures as algebraic datatypes."
  (every (lambda (constructor)
           (check-type constructor proper-list))
         constructors)
  `(progn
     (defstruct (,name (:constructor nil)
                       (:copier nil)
                       (:include algebraic-datatype)
                       (:predicate nil)))
     ,@(mapcar (rpartial #'parse-constructor name)
               constructors)
     ,@(mapcar #'parse-printer constructors)
     ',name))

(defun data= (data1 data2)
  (if (eq (class-of data1) (class-of data2))
      (typecase data1
        (promise
          (data= (force data1) (force data2)))
        (algebraic-datatype
          (let ((parameters1 (make-parameters (slot-value data1 'arity)))
                (parameters2 (make-parameters (slot-value data2 'arity))))
            (every (lambda (p1 p2)
                     (data= (slot-value data1 p1)
                            (slot-value data2 p2)))
                   parameters1
                   parameters2)))
        (otherwise
          (equal data1 data2)))
      nil))
