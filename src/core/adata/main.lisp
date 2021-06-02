(defpackage fcl.adata
  (:nicknames :fcl.core.adata :fcl.dt)
  (:use :common-lisp :fcl.adata.util :fcl.adata.parser)
  (:import-from
    :fcl.util
    #:rpartial
    #:proper-list)
  (:export
    #:algebraic-data
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
  (check-type data1 algebraic-datatype)
  (check-type data2 algebraic-datatype)
  (equalp data1 data2))
