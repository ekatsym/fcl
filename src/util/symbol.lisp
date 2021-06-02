(defpackage fcl.util.symbol
  (:use :common-lisp)
  (:import-from
    :fcl.util
    #:symbolicate))
(in-package :fcl.util.symbol)


(defun symbolicate (&rest things)
  (identity
    (intern
      (reduce (lambda (acc thing)
                (concatenate 'string acc (string thing)))
              things
              :initial-value ""))))
