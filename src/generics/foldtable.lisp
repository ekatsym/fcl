(defpackage fcl.generics.treversable
  (:nicknames :fcl.g.trevarsable :fcl.treversable)
  (:use
    :common-lisp
    :fcl.generics.foldable)
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
    #:unfoldl+
    #:foldt
    #:foldt+
    #:unfoldt
    #:unfoldt+
    #:foldtl
    #:foldtl+
    #:unfoldtl
    #:unfoldtl+))
(in-package :fcl.generics.treversable)


(defgeneric foldt (a&x&x->x x0 lar))

(defgeneric foldt+ (lar&a&x&x->x x0 lar))

(defgeneric unfoldt (x->? x->a x->x1 x->x2 x))

(defgeneric unfoldt+ (x->? x->a x->x1 x->x2 lar0 x))

(defgeneric foldtl (a&x&x->x x0 lar))

(defgeneric foldtl+ (lar&a&x&x->x x0 lar))

(defgeneric unfoldtl (x->? x->a x->x1 x->x2 x))

(defgeneric unfoldtl+ (x->? x->a x->x1 x->x2 lar0 x))
