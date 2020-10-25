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

(defmethod foldr (a&x->x x0 (as vector))
  (check-type a&x->x function)
  (do ((x x0 (funcall a&x->x (aref as i) x))
       (i (1- (length as)) (1- i)))
      ((<= i -1) x)))

(defmethod foldr+ (as&x->x x0 (as vector))
  (check-type as&x->x function)
  (do ((x x0 (funcall as&x->x (subseq as i) x))
       (i (1- (length as)) (1- i)))
      ((<= i -1) x)))

(macrolet ((define-unfoldr (classname)
             `(defmethod unfoldr ((class (eql ',classname)) x->? x->a x->x x)
                (check-type x->? function)
                (check-type x->a function)
                (check-type x->x function)
                (coerce (unfoldr 'list x->? x->a x->x x) class))))
  (define-unfoldr vector)
  (define-unfoldr simple-vector)
  (define-unfoldr bit-vector)
  (define-unfoldr simple-bit-vector)
  (define-unfoldr string)
  (define-unfoldr simple-string)
  (define-unfoldr base-string)
  (define-unfoldr simple-base-string))

(macrolet ((define-unfoldr+ (classname)
             `(defmethod unfoldr+ ((class (eql ',classname)) x->? x->a x->x as0 x)
                (check-type x->? function)
                (check-type x->a function)
                (check-type x->x function)
                (check-type as0 ,classname)
                (coerce (unfoldr+ 'list x->? x->a x->x (coerce as0 'list) x) class))))
  (define-unfoldr+ vector)
  (define-unfoldr+ simple-vector)
  (define-unfoldr+ bit-vector)
  (define-unfoldr+ simple-bit-vector)
  (define-unfoldr+ string)
  (define-unfoldr+ simple-string)
  (define-unfoldr+ base-string)
  (define-unfoldr+ simple-base-string))

(defmethod foldl (x&a->x x0 (as vector))
  (check-type x&a->x function)
  (do ((x x0 (funcall x&a->x x (aref as i)))
       (i 0 (1+ i))
       (len (length as)))
      ((>= i len) x)))

(defmethod foldl+ (x&as->x x0 (as vector))
  (check-type x&as->x function)
  (do ((x x0 (funcall x&as->x x (subseq as i)))
       (i 0 (1+ i))
       (len (length as)))
      ((>= i len) x)))

(macrolet ((define-unfoldl (classname)
             `(defmethod unfoldl ((class (eql ',classname)) x->? x->x x->a x)
                (check-type x->? function)
                (check-type x->a function)
                (check-type x->x function)
                (coerce (unfoldl 'list x->? x->x x->a x) class))))
  (define-unfoldl vector)
  (define-unfoldl simple-vector)
  (define-unfoldl bit-vector)
  (define-unfoldl simple-bit-vector)
  (define-unfoldl string)
  (define-unfoldl simple-string)
  (define-unfoldl base-string)
  (define-unfoldl simple-base-string))

(macrolet ((define-unfoldl+ (classname)
             `(defmethod unfoldl+ ((class (eql ',classname)) x->? x->x x->a as0 x)
                (check-type x->? function)
                (check-type x->a function)
                (check-type x->x function)
                (check-type as0 ,classname)
                (coerce (unfoldl+ 'list x->? x->x x->a (coerce as0 'list) x) class))))
  (define-unfoldl+ vector)
  (define-unfoldl+ simple-vector)
  (define-unfoldl+ bit-vector)
  (define-unfoldl+ simple-bit-vector)
  (define-unfoldl+ string)
  (define-unfoldl+ simple-string)
  (define-unfoldl+ base-string)
  (define-unfoldl+ simple-base-string))

(defmethod foldt (a&xs->x x0 (at vector))
  (check-type a&xs->x function)
  (labels ((rec (at)
             (if (null at)
                 x0
                 (funcall a&xs->x (aref at 0) (map 'list #'rec (subseq at 1))))))
    (rec at)))

(defmethod foldt+ (at&xs->x x0 (at vector))
  (check-type at&xs->x function)
  (labels ((rec (at)
             (if (null at)
                 x0
                 (funcall at&xs->x at (map 'list #'rec (subseq at 1))))))
    (rec at)))

(macrolet ((define-unfoldt (classname)
             `(defmethod unfoldt ((class (eql ',classname)) x->? x->a x->xs x)
                (check-type x->? function)
                (check-type x->a function)
                (check-type x->xs function)
                (coerce (unfoldt 'list x->? x->a x->xs x) class))))
  (define-unfoldt vector)
  (define-unfoldt simple-vector)
  (define-unfoldt bit-vector)
  (define-unfoldt simple-bit-vector)
  (define-unfoldt string)
  (define-unfoldt simple-string)
  (define-unfoldt base-string)
  (define-unfoldt simple-base-string))

(macrolet ((define-unfoldt+ (classname)
             `(defmethod unfoldt+ ((class (eql ',classname)) x->? x->a x->xs at0 x)
                (check-type x->? function)
                (check-type x->a function)
                (check-type x->xs function)
                (check-type at0 ,classname)
                (coerce (unfoldt+ 'list x->? x->a x->xs (coerce at0 'list) x) class))))
  (define-unfoldt+ vector)
  (define-unfoldt+ simple-vector)
  (define-unfoldt+ bit-vector)
  (define-unfoldt+ simple-bit-vector)
  (define-unfoldt+ string)
  (define-unfoldt+ simple-string)
  (define-unfoldt+ base-string)
  (define-unfoldt+ simple-base-string))

;;; MONAD-PLUS
(define-fmap-by-monad vector)

(macrolet ((define-unit (classname)
             `(defmethod unit ((class (eql ',classname)) a)
                (coerce (vector a) ',classname))))
  (define-unit vector)
  (define-unit simple-vector)
  (define-unit bit-vector)
  (define-unit simple-bit-vector)
  (define-unit string)
  (define-unit simple-string)
  (define-unit base-string)
  (define-unit simple-base-string))

(define-amap-by-monad vector)

(defmethod mmap (a->b* (a* vector))
  (check-type a->b* function)
  (let ((type (typecase a*
                (string 'string)
                (vector 'vector))))
    (foldr (lambda (a b*) (concatenate type (funcall a->b* a) b*))
           '()
           a*)))

(macrolet ((define-mzero (classname)
             `(defmethod mzero ((class (eql ',classname)))
                (coerce #() class))))
  (define-mzero vector)
  (define-mzero simple-vector)
  (define-mzero bit-vector)
  (define-mzero simple-bit-vector)
  (define-mzero string)
  (define-mzero simple-string)
  (define-mzero base-string)
  (define-mzero simple-base-string))

(defmethod mplus ((monoid1 vector) monoid2)
  (check-type monoid2 vector)
  (typecase monoid1
    (string (concatenate 'string monoid1 monoid2))
    (vector (concatenate 'vector monoid1 monoid2))))
