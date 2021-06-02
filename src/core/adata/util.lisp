(defpackage fcl.adata.util
  (:use :common-lisp)
  (:import-from
    :fcl.util
    #:index
    #:symbolicate)
  (:export
    #:make-parameters
    #:lazy-type-specifier-p))
(in-package :fcl.adata.util)

(defun make-parameters (n)
  "Returns a list of parameters whose size is N like (%0 %1 ... %N)."
  (check-type n index)
  (loop :for i :from 0 :below n
        :collect (symbolicate "%" (write-to-string i))))

(defun lazy-type-specifier-p (specifier)
  "Test whether SPECIFIER is (:LAZY <CL type specifier>) or not."
  (and (consp specifier) (eq (first specifier) :lazy)))
