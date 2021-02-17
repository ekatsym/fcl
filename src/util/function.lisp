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
    #:rcurry))
(in-package :fcl.util.function)


(defun compose (func &rest more-funcs)
  (let ((f1 (first (last more-funcs)))
        (fs (cons func (butlast more-funcs))))
    (lambda (&rest args)
      (reduce (lambda (acc f) (funcall f acc))
              fs
              :initial-value (apply f1 args)))))

(defun partial (func &rest args)
  (lambda (&rest rest-args)
    (apply func (append args rest-args))))

(defun rpartial (func &rest args)
  (lambda (&rest rest-args)
    (apply func (append rest-args args))))

(defun partial-at (n func arg)
  (lambda (&rest rest-args)
    (apply func (insert-at n arg rest-args))))

(defun curry (func)
  (lambda (&rest args)
    (lambda (&rest rest-args)
      (apply func (append args rest-args)))))

(defun rcurry (func)
  (lambda (&rest args)
    (lambda (&rest rest-args)
      (apply func (append rest-args args)))))
