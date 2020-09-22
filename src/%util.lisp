(defpackage fcl.util
  (:use :common-lisp)
  (:import-from
    :fcl
    #:nlist?
    #:projection
    #:partial
    #:rpartial
    #:curry
    #:rcurry
    #:compose
    #:flip
    #:filter
    #:zip)
  (:export
    #:index
    #:proper-list
    #:proper-list-p
    #:nlist?
    #:symbolicate
    #:projection
    #:partial
    #:rpartial
    #:curry
    #:rcurry
    #:flip
    #:compose
    #:filter
    #:zip))

(in-package :fcl.util)


(deftype index ()
  `(integer 0 ,array-total-size-limit))

(defun proper-list-p (object)
  (declare (optimize (speed 3)))
  (or (null object)
      (and (consp object)
           (proper-list-p (rest object)))))

(deftype proper-list ()
  '(satisfies proper-list-p))

(defun nlist? (n list)
  (check-type n index)
  (cond ((zerop n) (null list))
        ((consp list) (nlist? (1- n) (rest list)))
        (t nil)))

(defun symbolicate (&rest things)
  (identity
    (intern
      (reduce (lambda (acc thing)
                (concatenate 'string acc (string thing)))
              things
              :initial-value ""))))

(defun projection (n)
  (lambda (&rest args)
    (assert (< n (length args)) (args)
            "invalid number of arguments: ~s" (length args))
    (nth n args)))

(defun partial (function &rest args)
  (check-type function function)
  (lambda (&rest rest-args)
    (apply function (append args rest-args))))

(defun rpartial (function &rest args)
  (check-type function function)
  (lambda (&rest rest-args)
    (apply function (append rest-args args))))

(defun curry (function)
  (check-type function function)
  (lambda (&rest args)
    (lambda (&rest rest-args)
      (apply function (append args rest-args)))))

(defun rcurry (function)
  (check-type function function)
  (lambda (&rest args)
    (lambda (&rest rest-args)
      (apply function (append rest-args args)))))

(defun compose (&rest functions)
  (lambda (&rest args)
    (reduce (lambda (f xs) (multiple-value-list (apply f xs)))
            (cons #'values functions)
            :from-end t
            :initial-value args)))

(defun flip (function)
  (lambda (&rest args) (apply function (reverse args))))

(defun filter (function list &rest more-lists)
  (check-type function function)
  (check-type list list)
  (every (lambda (lst) (check-type lst list)) more-lists)
  (do ((lsts (cons list more-lists) (mapcar #'rest lsts))
       (acc '() (let ((x (apply function (mapcar #'first lsts))))
                  (if x
                      (cons x acc)
                      acc))))
      ((some #'endp lsts)
       (nreverse acc))))

(defun zip (&rest lists)
  (apply #'mapcar #'list lists))
