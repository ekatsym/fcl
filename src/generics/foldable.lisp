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
    #:fold-tree
    #:fold-tree+
    #:unfold-tree
    #:unfold-tree+))
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
(defgeneric fold-tree (a&xs->x x0 at))

(defgeneric fold-tree+ (at&xs->x x0 at))

(defgeneric unfold-tree (class x->? x->a x->xs x))

(defgeneric unfold-tree+ (class x->? x->a x->xs at0 x))
