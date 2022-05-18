(defpackage fcl.foldable
  (:nicknames :fcl.generics.foldable :fcl.fd)
  (:use :common-lisp :fcl.lazy)
  (:import-from :fcl.util #:partial)
  (:import-from :fcl.match #:match #:ematch)
  (:export
    #:foldr #:foldr+ #:foldl #:foldl+
    #:lfoldr #:lfoldr+
    #:scanr #:scanr+ #:scanl #:scanl+

    ;;; Lazy
    #:delay #:force
    #:scanr #:scanr+ #:scanl #:scanl+))
(in-package :fcl.foldable)


(defgeneric foldr (a&x->x x0 as))

(defgeneric foldr+ (a&x&as->x x0 as))

(defgeneric foldl (x&a->x x0 as))

(defgeneric foldl+ (x&a&as->x x0 as))


;;; Lazy Folds
(defgeneric lfoldr (a&$x->x x0 as))

(defgeneric lfoldr+ (a&$x&as->x x0 as))


;;; Scans
(defun scanr (a&x->x x0 as)
  (check-type a&x->x function)
  (foldr (lambda (a xs) (cons (funcall a&x->x a (first xs)) xs))
         (list x0)
         as))

(defun scanr+ (a&x&as->x x0 as)
  (check-type a&x&as->x function)
  (foldr+ (lambda (a as xs) (cons (funcall a&x&as->x a (first xs) as) xs))
          (list x0)
          as))

(defun scanl (x&a->x x0 as)
  (check-type x&a->x function)
  (foldl (lambda (xs a) (cons (funcall x&a->x (first xs) a) xs))
         (list x0)
         as))

(defun scanl+ (x&a&as->x x0 as)
  (check-type x&a&as->x function)
  (foldl+ (lambda (xs a as) (cons (funcall x&a&as->x (first xs) a as) xs))
          (list x0)
          as))
