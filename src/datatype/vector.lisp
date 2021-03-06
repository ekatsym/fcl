(defpackage fcl.vector
  (:nicknames :fcl.data.vector :fcl.vc)
  (:use :common-lisp :fcl.monad-plus :fcl.foldable :fcl.unfoldable)
  (:import-from
    :fcl.util
    #:index)
  (:export
    ;; Monad Plus
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:mzero #:mplus #:msum
    #:guard
    #:vc

    ;; Foldable
    #:foldr #:foldr+
    #:foldl #:foldl+
    #:lfoldr #:lfoldr+
    #:scanr #:scanr+ #:scanl #:scanl+

    ;; Unfoldable
     #:unfoldr #:unfoldr+
     #:unfoldl #:unfoldl+

     ;; Lazy Evaluation
     #:delay #:force))
(in-package :fcl.vector)


;;; FOLDABLE
(defmethod foldr (a&x->x x0 (as vector))
  (check-type a&x->x function)
  (do ((x x0 (funcall a&x->x (elt as i) x))
       (i (1- (length as)) (1- i)))
      ((<= i -1) x)))

(defmethod foldr+ (a&x&as->x x0 (as vector))
  (check-type a&x&as->x function)
  (do ((x x0 (funcall a&x&as->x (elt as i) x (subseq as (1+ i))))
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
  (do ((x x0 (funcall x&a->x x (elt as i)))
       (i 0 (1+ i))
       (len (length as)))
      ((>= i len) x)))

(defmethod foldl+ (x&a&as->x x0 (as vector))
  (check-type x&a&as->x function)
  (do ((x x0 (funcall x&a&as->x x (elt as i) (subseq as (1+ i))))
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

(defmethod lfoldr (a&$x->x x0 (as vector))
  (check-type a&$x->x function)
  (let ((len (length as)))
    (labels ((rec (i)
               (if (>= i len)
                   x0
                   (funcall a&$x->x (elt as i) (delay (rec (1+ i)))))))
      (rec 0))))

(defmethod lfoldr+ (a&$x&as->x x0 (as vector))
  (check-type a&$x&as->x function)
  (let ((len (length as)))
    (labels ((rec (i)
               (if (>= i len)
                   x0
                   (funcall a&$x&as->x
                            (elt as i)
                            (delay (rec (1+ i)))
                            (subseq as (1+ i))))))
      (rec 0))))

(defmethod lfoldl ($x&a->x x0 (as vector))
  (check-type $x&a->x function)
  (let ((len (length as)))
    (labels ((rec (i $x)
               (declare (optimize (space 3)))
               (if (>= i len)
                   (force $x)
                   (rec (1- i) (delay (funcall $x&a->x (force $x) (elt as i)))))))
      (rec 0 (delay x0)))))

(defmethod lfoldl+ ($x&a&as->x x0 (as vector))
  (check-type $x&a&as->x function)
  (let ((len (length as)))
    (labels ((rec (i $x)
               (declare (optimize (space 3)))
               (if (>= i len)
                   (force $x)
                   (rec (1- i)
                        (delay (funcall $x&a&as->x (force $x) (elt as i) (subseq as (1+ i))))))))
      (rec 0 (delay x0)))))


;;; MONAD-PLUS
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

(define-fmap-by-monad vector)

(define-amap-by-monad vector)

(defmethod mmap (a->b* (a* vector))
  (check-type a->b* function)
  (let ((type (typecase a*
                (string 'string)
                (vector 'vector))))
    (foldr (lambda (a b*) (concatenate type (funcall a->b* a) b*))
           #()
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

(defmethod mplus ((monoid1 vector) (monoid2 vector))
  (typecase monoid1
    (string (concatenate 'string monoid1 monoid2))
    (vector (concatenate 'vector monoid1 monoid2))))


;;; Vector Comprehension
(defmacro vc (element &body clauses)
  `(mdo ,@(mapcar (lambda (clause)
                    (if (listp clause)
                        (case (first clause)
                          ((:in :let) clause)
                          (otherwise `(guard 'vector ,clause)))
                        `(guard 'vector ,clause)))
                  clauses)
        (unit 'vector ,element)))

