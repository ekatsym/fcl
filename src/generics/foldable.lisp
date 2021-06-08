(defpackage fcl.foldable
  (:nicknames :fcl.generics.foldable :fcl.fd)
  (:use :common-lisp :fcl.lazy)
  (:export
    #:foldr #:foldr+ #:unfoldr #:unfoldr+
    #:foldl #:foldl+ #:unfoldl #:unfoldl+
    #:delay #:force
    #:lfoldr #:lfoldr+
    #:lfoldl #:lfoldl+
    #:scanr #:scanr+ #:scanl #:scanl+))
(in-package :fcl.foldable)


;;; Folds for "List".
(defgeneric foldr (a&x->x x0 as))

(defgeneric foldr+ (a&as&x->x x0 as))

(defgeneric unfoldr (class x->? x->a x->x x))

(defgeneric unfoldr+ (class x->? x->a x->x as0 x))

(defgeneric foldl (x&a->x x0 as))

(defgeneric foldl+ (x&a&as->x x0 as))

(defgeneric unfoldl (class x->? x->x x->a x))

(defgeneric unfoldl+ (class x->? x->x x->a as0 x))


;;; Lazy Folds
(defgeneric lfoldr (a&$x->x x0 as))

(defgeneric lfoldr+ (a&as&$x->x x0 as))

(defgeneric lfoldl ($x&a->x x0 as))

(defgeneric lfoldl+ ($x&a&as->x x0 as))

(defgeneric lfoldt (a&$xs->x x0 at))

(defgeneric lfoldt+ (a&ats$xs->x x0 at))


;;; Scans
(defun scanr (a&x->x x0 as)
  (check-type a&x->x function)
  (foldr (lambda (a xs) (cons (funcall a&x->x a (first xs)) xs))
         (list x0)
         as))

(defun scanr+ (a&as&x->x x0 as)
  (check-type a&as&x->x function)
  (foldr+ (lambda (a as xs) (cons (funcall a&as&x->x a as (first xs)) xs))
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
