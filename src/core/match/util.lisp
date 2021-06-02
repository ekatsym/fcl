(defpackage fcl.match.util
  (:use :common-lisp)
  (:import-from
    :fcl.util
    #:index
    #:symbolicate)
  (:export
    #:*standard-atomic-type-specifiers*
    #:make-parameters))
(in-package :fcl.match.util)


(defparameter *standard-atomic-type-specifiers*
  '(arithmetic-error array atom base-char base-string bignum bit bit-vector broadcast-stream
    built-in-class cell-error character class compiled-function complex concatenated-stream
    condition cons control-error division-by-zero double-float echo-stream end-of-file error
    extended-char file-error file-stream fixnum floating-point-inexact
    floating-point-invalid-operation floating-point-overflow floating-point-underflow
    function generic-function hash-table integer keyword list logical-pathname long-float
    method method-combination nil null number package package-error parse-error pathname
    print-not-readable program-error random-state ratio rational reader-error readtable
    real restart sequence serious-condition short-float signed-byte simple-array
    simple-base-string simple-bit-vector simple-condition simple-error simple-string
    simple-type-error simple-vector simple-warning single-float standard-char standard-class
    standard-generic-function standard-method standard-object stream stream-error string
    string-stream structure-class structure-object style-warning t two-way-stream type-error
    unbound-slot unbound-variable undefined-function unsigned-byte vector warning))

(defun make-parameters (n)
  "Returns a list of parameters whose size is N like (%0 %1 ... %N)."
  (check-type n index)
  (loop :for i :from 0 :below n
        :collect (symbolicate "%" (write-to-string i))))
