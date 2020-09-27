(defpackage fcl.generics.foldable
  (:nicknames :fcl.g.foldable :fcl.foldable)
  (:use :common-lisp)
  (:export
    #:cata
    #:para
    #:ana
    #:apo
    #:iterate
    #:iterate+
    #:accumulate
    #:accumulate+
    #:iteratel
    #:iteratel+
    #:accumulatel
    #:accumulatel+
    #:foldr
    #:foldr+
    #:unfoldr
    #:unfoldr+
    #:foldl
    #:foldl+
    #:unfoldl
    #:unfoldl+))
(in-package :fcl.generics.foldable)


(defgeneric foldr (a&x->x x0 as))

(defgeneric foldr+ (as&a&x->x x0 as))

(defgeneric unfoldr (x->? x->a x->x x))

(defgeneric unfoldr+ (x->? x->a x->x as0 x))

(defgeneric foldl (x&a->x x0 as))

(defgeneric foldl+ (as&x&a->x x0 as))

(defgeneric unfoldl (x->? x->a x->x x))

(defgeneric unfoldl+ (x->? x->a x->x as0 x))
