#|
FOLDABLE means F-algebra where F(X) = 1+A*X.
|#
(defpackage fcl.foldable
  (:use :common-lisp)
  (:import-from
    :fcl
    #:foldr
    #:foldl
    #:foldr+
    #:foldl+
    #:unfoldr
    #:unfoldl
    #:unfoldr+
    #:unfoldl+)
  (:export
    #:foldr
    #:foldl
    #:foldr+
    #:foldl+
    #:unfoldr
    #:unfoldl
    #:unfoldr+
    #:unfoldl+))

(in-package :fcl.foldable)


(defgeneric foldr (a&x->x x0 a*))

(defgeneric foldl (a&x->x x0 a*))

(defgeneric foldr+ (a&a*&x->x x0 a*))

(defgeneric foldl+ (a&a*&x->x x0 a*))

(defgeneric unfoldr (class x->? x->a x->x x))

(defgeneric unfoldl (class x->? x->a x->x x))

(defgeneric unfoldr+ (class x->? x->a x->x a* x))

(defgeneric unfoldl+ (class x->? x->a x->x a* x))
