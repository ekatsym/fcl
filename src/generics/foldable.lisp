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
    #:fold2
    #:fold2+
    #:unfold2
    #:unfold2+))
(in-package :fcl.generics.foldable)


;;; Fold for "List".
(defgeneric foldr (a&x->x x0 as))

(defgeneric foldr+ (as&x->x x0 as))

(defgeneric unfoldr (class x->? x->a x->x x))

(defgeneric unfoldr+ (class x->? x->a x->x as x))

(defgeneric foldl (x&a->x x0 as))

(defgeneric foldl+ (x&as->x x0 as))

(defgeneric unfoldl (class x->? x->x x->a x))

(defgeneric unfoldl+ (class x->? x->x x->a as0 x))


;;; Fold for "Binary Tree".
(defgeneric fold2 (a&x&x->x x0 lar))

(defgeneric fold2+ (lar&x&x->x x0 lar))

(defgeneric unfold2 (class x->? x->a x->x1 x->x2 x))

(defgeneric unfold2+ (class x->? x->a x->x1 x->x2 lar0 x))
