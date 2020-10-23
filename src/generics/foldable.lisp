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
    #:scanr
    #:scanr+
    #:scanl
    #:scanl+
    #:scant
    #:scant+))
(in-package :fcl.generics.foldable)


;;; Fold for "List".
(defgeneric foldr (a&x->x x0 as))

(defgeneric foldr+ (as&x->x x0 as))

(defgeneric unfoldr (class x->? x->a x->x x))

(defgeneric unfoldr+ (class x->? x->a x->x as0 x))

(defgeneric foldl (x&a->x x0 as))

(defgeneric foldl+ (x&as->x x0 as))

(defgeneric unfoldl (class x->? x->x x->a x))

(defgeneric unfoldl+ (class x->? x->x x->a as0 x))


;;; Fold for "Tree".
(defgeneric foldt (a&xs->x x0 at))

(defgeneric foldt+ (at&xs->x x0 at))

(defgeneric unfoldt (class x->? x->a x->xs x))

(defgeneric unfoldt+ (class x->? x->a x->xs at0 x))


;;; Scans
(defun scanr (a&x->x x0 as)
  (check-type a&x->x function)
  (foldr (lambda (a xs) (cons (funcall a&x->x a (first xs)) xs)) (list x0) as))

(defun scanr+ (as&x->x x0 as)
  (check-type as&x->x function)
  (foldr+ (lambda (as xs) (cons (funcall as&x->x as (first xs)) xs)) (list x0) as))

(defun scanl (x&a->x x0 as)
  (check-type x&a->x function)
  (foldl (lambda (xs a) (cons (funcall x&a->x (first xs) a) xs)) (list x0) as))

(defun scanl+ (x&as->x x0 as)
  (check-type x&as->x function)
  (foldl+ (lambda (xs as) (cons (funcall x&as->x (first xs) as) xs)) (list x0) as))

(defun scant (a&xs->x x0 at)
  (check-type a&xs->x function)
  (foldt (lambda (a xss)
           (cons (funcall a&xs->x a (mapcar #'first xss))
                 (mapcar #'rest xss)))
         (list x0)
         at))

(defun scant+ (at&xs->x x0 at)
  (check-type at&xs->x function)
  (foldt+ (lambda (at xss)
            (cons (funcall at&xs->x at (mapcar #'first xss))
                  (mapcar #'rest xss)))
          (list x0)
          at))
