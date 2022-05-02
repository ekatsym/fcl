(defpackage fcl.unfoldable
  (:nicknames :fcl.generics.unfoldable :fcl.uf)
  (:use :common-lisp)
  (:export
    #:unfoldr #:unfoldr+ #:unfoldl #:unfoldl+))


(defgeneric unfoldr (class x->? x->a x->x x))

(defgeneric unfoldr+ (class x->? x->a x->x as0 x))

(defgeneric unfoldl (class x->? x->x x->a x))

(defgeneric unfoldl+ (class x->? x->x x->a as0 x))
