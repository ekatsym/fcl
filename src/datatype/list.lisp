(defpackage fcl.list
  (:nicknames :fcl.data.list :fcl.ls)
  (:use :common-lisp :fcl.monad-plus :fcl.foldable :fcl.unfoldable)
  (:import-from
    :fcl.util
    #:index)
  (:import-from
    :fcl.lazy
    #:delay
    #:force)
  (:import-from
    :fcl.match
    #:ematch)
  (:export
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:mzero #:mplus #:msum
    #:guard
    #:lc
    #:mzero #:mplus #:msum
    #:foldr #:foldr+ #:unfoldr #:unfoldr+
    #:foldl #:foldl+ #:unfoldl #:unfoldl+
    #:lfoldr #:lfoldr+
    #:lfoldl #:lfoldl+
    #:scanr #:scanr+ #:scanl #:scanl+))
(in-package :fcl.list)


;;; FOLDABLE
(defmethod foldr (a&x->x x0 (as list))
  (check-type a&x->x function)
  (do ((as (reverse as) (rest as))
       (x x0 (funcall a&x->x (first as) x)))
      ((endp as) x)))

(defmethod foldr+ (a&x&as->x x0 (as list))
  (check-type a&x&as->x function)
  (flet ((rev+ (lst)
           (do ((lst lst (rest lst))
                (acc '() (cons lst acc)))
               ((endp lst) acc))))
    (do ((as-s (rev+ as) (rest as-s))
         (x x0 (ematch as-s
                 ((cons (cons a as) _)
                  (funcall a&x&as->x a x as)))))
        ((endp as-s) x))))

(defmethod unfoldr ((class (eql 'list)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (do ((x x (funcall x->x x))
       (as '() (cons (funcall x->a x) as)))
      ((funcall x->? x) (nreverse as))))

(defmethod unfoldr+ ((class (eql 'list)) x->? x->a x->x as0 x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (check-type as0 list)
  (do ((x x (funcall x->x x))
       (as '() (cons (funcall x->a x) as)))
      ((funcall x->? x) (revappend as as0))))

(defmethod foldl (x&a->x x0 (as list))
  (check-type x&a->x function)
  (do ((as as (rest as))
       (x x0 (funcall x&a->x x (first as))))
      ((endp as) x)))

(defmethod foldl+ (x&a&as->x x0 (as list))
  (check-type x&a&as->x function)
  (do ((as as as2)
       (a (first as) (first as2))
       (as2 (rest as) (rest as2))
       (x x0 (funcall x&a&as->x x a as2)))
      ((endp as) x)))

(defmethod unfoldl ((class (eql 'list)) x->? x->x x->a x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (do ((x x (funcall x->x x))
       (as '() (cons (funcall x->a x) as)))
      ((funcall x->? x) as)))

(defmethod unfoldl+ ((class (eql 'list)) x->? x->x x->a as0 x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (check-type as0 list)
  (do ((x x (funcall x->x x))
       (as as0 (cons (funcall x->a x) as)))
      ((funcall x->? x) as)))

(defmethod lfoldr (a&$x->x x0 (as list))
  (check-type a&$x->x function)
  (labels ((rec (as)
             (if (endp as)
                 x0
                 (funcall a&$x->x (first as) (delay (rec (rest as)))))))
    (rec as)))

(defmethod lfoldr+ (a&$x&as->x x0 (as list))
  (check-type a&$x&as->x function)
  (labels ((rec (as)
             (if (endp as)
                 x0
                 (let ((a (first as))
                       (as (rest as)))
                   (funcall a&$x&as->x a (delay (rec as)) as)))))
    (rec as)))

(defmethod lfoldl ($x&a->x x0 (as list))
  (check-type $x&a->x function)
  (labels ((rec (as $x)
             (declare (optimize (space 3)))
             (if (endp as)
                 (force $x)
                 (rec (rest as) (delay (funcall $x&a->x (force $x) (first as)))))))
    (rec as (delay x0))))

(defmethod lfoldl+ ($x&a&as->x x0 (as list))
  (check-type $x&a&as->x function)
  (labels ((rec (as $x)
             (declare (optimize (space 3)))
             (if (endp as)
                 (force $x)
                 (let ((a (first as))
                       (as (rest as)))
                   (rec as (delay (funcall $x&a&as->x (force $x) a as)))))))
    (rec as (delay x0))))


;;; MONAD-PLUS
(defmethod unit ((class (eql 'list)) a)
  (list a))

(define-fmap-by-monad list)

(define-amap-by-monad list)

(defmethod mmap (a->b* (a* list))
  (check-type a->b* function)
  (foldr (lambda (a b*) (append (funcall a->b* a) b*))
         '()
         a*))

(defmethod mzero ((class (eql 'list)))
  '())

(defmethod mplus ((monoid1 list) (monoid2 list))
  (append monoid1 monoid2))


;;; List Comprehension
(defmacro lc (element &body clauses)
  `(mdo ,@(mapcar (lambda (clause)
                    (if (listp clause)
                        (case (first clause)
                          ((:in :let) clause)
                          (otherwise  `(guard 'list ,clause)))
                        `(guard 'list ,clause)))
                  clauses)
        (unit 'list ,element)))

