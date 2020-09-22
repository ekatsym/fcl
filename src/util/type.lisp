(defpackage fcl.util.type
  (:nicknames :fcl.u.type)
  (:use :common-lisp)
  (:export
    #:index
    #:proper-list))
(in-package :fcl.u.type)


(deftype index ()
  `(integer 0 ,array-total-size-limit))

(defun proper-list-p (lst)
  (declare (optimize (speed 3)))
  (do ((l lst (cdr l)))
      ((null l) t)
      (unless (consp lst)
        (return nil))))

(deftype proper-list ()
  '(satisfies proper-list-p))
