(defpackage fcl.util.type
  (:use :common-lisp)
  (:import-from
    :fcl.util
    #:index
    #:proper-list))
(in-package :fcl.util.type)


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
