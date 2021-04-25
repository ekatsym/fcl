(defpackage fcl.generics.foldable
  (:nicknames :fcl.g.foldable :fcl.foldable)
  (:use
    :common-lisp
    :fcl.generics.recursive)
  (:export
    #:cata
    #:para
    #:ana
    #:apo
    #:foldr
    #:foldr+
    #:unfoldr
    #:unfoldr+
    #:foldl
    #:foldl+
    #:unfoldl
    #:unfoldl+
    #:foldt
    #:foldt+
    #:unfoldt
    #:unfoldt+
    #:lfoldr
    #:lfoldr+
    #:lfoldl
    #:lfoldl+
    #:lfoldt
    #:lfoldt+
    #:scanr
    #:scanr+
    #:scanl
    #:scanl+
    #:scant
    #:scant+))
(in-package :fcl.generics.foldable)


;;; Folds for "List".
(defgeneric foldr (a&x->x x0 as))

(defgeneric foldr+ (a&as&x->x x0 as))

(defgeneric unfoldr (class x->? x->a x->x x))

(defgeneric unfoldr+ (class x->? x->a x->x as0 x))

(defgeneric foldl (x&a->x x0 as))

(defgeneric foldl+ (x&a&as->x x0 as))

(defgeneric unfoldl (class x->? x->x x->a x))

(defgeneric unfoldl+ (class x->? x->x x->a as0 x))


;;; Folds for "Tree".
(defgeneric foldt (a&xs->x x0 at))

(defgeneric foldt+ (a&ats&xs->x x0 at))

(defgeneric unfoldt (class x->? x->a x->xs x))

(defgeneric unfoldt+ (class x->? x->a x->xs at0 x))


;;; Lazy Folds
(defgeneric lfoldr (a&$x->x x0 as))

(defgeneric lfoldr+ (a&as&$x->x x0 as))

(defgeneric lfoldl ($x&a->x x0 as))

(defgeneric lfoldl+ ($x&a->x x0 as))

(defgeneric lfoldt (a&$xs->x x0 at))

(defgeneric lfoldt+ (a&$xs->x x0 at))


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

(defun scant (a&xs->x x0 at)
  (check-type a&xs->x function)
  (foldt (lambda (a xss)
           (cons (funcall a&xs->x a (mapcar #'first xss))
                 (mapcar #'rest xss)))
         (list x0)
         at))

(defun scant+ (a&at&xs->x x0 at)
  (check-type a&at&xs->x function)
  (foldt+ (lambda (a at xss)
            (cons (funcall a&at&xs->x a at (mapcar #'first xss))
                  (mapcar #'rest xss)))
          (list x0)
          at))
