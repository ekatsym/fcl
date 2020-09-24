(defpackage fcl.data.core
  (:nicknames :fcl.d.core)
  (:use
    :common-lisp
    :fcl.d.util
    :fcl.d.parser)
  (:import-from
    :fcl.util
    #:rpartial
    #:proper-list)
  (:export
    #:algebraic-datatype
    #:defdata))
(in-package :fcl.d.core)

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
