(defpackage fcl.util.symbol
  (:nicknames :fcl.u.symbol)
  (:use :common-lisp)
  (:export
    #:symbolicate))
(in-package :fcl.u.symbol)


(defun symbolicate (&rest things)
  (identity
    (intern
      (reduce (lambda (acc thing)
                (concatenate 'string acc (string thing)))
              things
              :initial-value ""))))
