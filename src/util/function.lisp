(defpackage fcl.util.function
  (:nicknames :fcl.u.function :fcl.u.f)
  (:use :common-lisp)
  (:import-from
    :fcl.u.list
    #:insert-at)
  (:export
    #:compose
    #:partial
    #:rpartial
    #:partial-at
    #:curry
    #:rcurry
    #:pipeline))
(in-package :fcl.util.function)


(defun compose (function &rest more-functions)
  (let ((f1 (first (last more-functions)))
        (fs (cons function (butlast more-functions))))
    (lambda (&rest args)
      (reduce (lambda (acc f) (funcall f acc))
              fs
              :initial-value (apply f1 args)))))

(defun partial (function &rest args)
  (lambda (&rest rest-args)
    (apply function (append args rest-args))))

(defun rpartial (function &rest args)
  (lambda (&rest rest-args)
    (apply function (append rest-args args))))

(defun partial-at (n function arg)
  (lambda (&rest rest-args)
    (apply function (insert-at n arg rest-args))))

(defun curry (function)
  (lambda (&rest args)
    (lambda (&rest rest-args)
      (apply function (append args rest-args)))))

(defun rcurry (function)
  (lambda (&rest args)
    (lambda (&rest rest-args)
      (apply function (append rest-args args)))))

(defun pipeline (x &rest functions)
  (let ((acc x))
    (dolist (func functions acc)
      (setq acc (funcall func acc)))))
