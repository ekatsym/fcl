(defpackage fcl.datatypes.vector
  (:nicknames :fcl.dt.vector :fcl.vector :fcl.vec)
  (:use
    :common-lisp
    :fcl.generics.foldable
    :fcl.generics.monad-plus)
  (:import-from
    :fcl.util
    #:index)
  (:import-from
    :fcl.datatypes.maybe
    #:maybe
    #:just #:just%0
    #:nothing)
  (:import-from
    :fcl.datatypes.either
    #:either
    #:left #:left%0
    #:right #:right%0)
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
    #:scant+

    #:fmap
    #:unit
    #:amap
    #:mmap
    #:mlet
    #:mprogn
    #:mdo
    #:mzero
    #:mplus
    #:msum
    #:guard))
(in-package :fcl.datatypes.vector)


;;; Methods
;; FOLDABLE
(defmethod cata (x*->x (i vector))
  "X* == (NOTHING) | (JUST (LIST A X))"
  (check-type x*->x function)
  (do ((x (funcall x*->x (nothing)) (funcall x*->x (just (list (aref i idx) x))))
       (idx 0 (1+ 0))
       (len (length i)))
      ((>= idx len) x)))

(defmethod para (i&*x->x (i vector))
  "I&*X == (NOTHING) | (JUST (LIST A I X))"
  (check-type i&*x->x function)
  (do ((x (funcall i&*x->x (nothing)) (funcall i&*x->x (just (list (aref i idx) (subseq i idx) x))))
       (idx 0 (1+ 0))
       (len (length i)))
      ((>= idx len) x)))

(macrolet ((defana (classname)
             `(defmethod ana ((class (eql ',classname)) x->x* x)
                "X* == (NOTHING) | (JUST (LIST A X))"
                (check-type x->x* function)
                (coerce (ana 'list x->x* x) class))))
  (defana vector)
  (defana simple-vector)
  (defana bit-vector)
  (defana simple-bit-vector)
  (defana string)
  (defana simple-string)
  (defana base-string)
  (defana simple-base-string))

(macrolet ((defapo (classname)
             `(defmethod apo ((class (eql ',classname)) x->f+*x x)
                "F+*X == (NOTHING) | (JUST (LIST A (LEFT F))) | (JUST (LIST A (RIGHT X)))"
                (check-type x->f+*x function)
                (coerce (apo 'list x->f+*x x) class))))
  (defapo vector)
  (defapo simple-vector)
  (defapo bit-vector)
  (defapo simple-bit-vector)
  (defapo string)
  (defapo simple-string)
  (defapo base-string)
  (defapo simple-base-string))
