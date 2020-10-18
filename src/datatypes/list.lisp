(defpackage fcl.datatypes.list
  (:nicknames :fcl.dt.list :fcl.list)
  (:use
    :common-lisp
    :fcl.util.list
    :fcl.generics.foldable
    :fcl.generics.monad-plus)
  (:import-from
    :fcl.match
    #:ematch)
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
    #:nlist?
    #:take
    #:drop
    #:enum
    #:insert-at
    #:filter
    #:mappend
    #:zip
    #:group

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
    #:unfold-tree+

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
    #:guard

    #:lc
    ))
(in-package :fcl.datatypes.list)


;;; Utility
;;; FOLDABLE
(defmethod cata (x*->x (i list))
  "X* == (NOTHING) | (JUST (LIST A X))"
  (check-type x*->x function)
  (do ((as (reverse i) (rest as))
       (x (funcall x*->x (nothing))
          (funcall x*->x (just (list (first as) x)))))
      ((endp as) x)))

(defmethod para (i&x*->x (i list))
  "I&X* == I (NOTHING) | I (JUST (LIST A X))"
  (check-type i&x*->x function)
  (do ((as-s (reverse+ i) (rest as-s))
       (x (funcall i&x*->x '() (nothing))
          (funcall i&x*->x (first as-s) (just (list (first (first as-s)) x)))))
      ((endp as-s) x)))

(defmethod ana ((class (eql 'list)) x->x* x)
  "X* == (NOTHING) | (JUST (LIST A X))"
  (check-type x->x* function)
  (do ((x* (funcall x->x* x))
       (ras '()))
      ((typep x* 'nothing) (nreverse ras))
      (ematch x*
        ((just (list a x))
         (setq x* (funcall x->x* x))
         (setq ras (cons a ras))))))

(defmethod apo ((class (eql'list)) x->f+*x x)
  "F+*X == (NOTHING) | (JUST (LIST A (LEFT F))) | (JUST (LIST A (RIGHT X)))"
  (check-type x->f+*x function)
  (do ((f+*x (funcall x->f+*x x))
       (ras '()))
      ((typep f+*x 'nothing) (nreverse ras))
      (ematch f+*x
        ((just (list a f+x))
         (ematch f+x
           ((left f)
            (return (revappend ras f)))
           ((right x)
            (setq f+*x (funcall x->f+*x x))
            (setq ras (cons a ras))))))))

(defmethod foldr (a&x->x x0 (as list))
  (check-type a&x->x function)
  (do ((as (reverse as) (rest as))
       (x x0 (funcall a&x->x (first as) x)))
      ((endp as) x)))

(defmethod foldr+ (as&x->x x0 (as list))
  (check-type as&x->x function)
  (do ((as-s (reverse+ as) (rest as-s))
       (x x0 (funcall as&x->x (first as-s) x)))
      ((endp as-s) x)))

(defmethod unfoldr ((class (eql 'list)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (do ((x x (funcall x->x x))
       (ras '() (cons (funcall x->a x) ras)))
      ((funcall x->? x) (nreverse ras))))

(defmethod unfoldr+ ((class (eql 'list)) x->? x->a x->x as0 x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (check-type as0 list)
  (do ((x x (funcall x->x x))
       (ras '() (cons (funcall x->a x) ras)))
      ((funcall x->? x) (revappend ras as0))))

(defmethod foldl (x&a->x x0 (as list))
  (check-type x&a->x function)
  (do ((as as (rest as))
       (x x0 (funcall x&a->x x (first as))))
      ((endp as) x)))

(defmethod foldl+ (x&as->x x0 (as list))
  (check-type x&as->x function)
  (do ((as as (rest as))
       (x x0 (funcall x&as->x x as)))
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


;;; MONAD-PLUS
(defmethod fmap (a->b (a* list))
  (check-type a->b function)
  (mapcar a->b a*))

(defmethod unit ((class (eql 'list)) a)
  (list a))

(defmethod amap (a->*b (a* list))
  (check-type a->*b list)
  (foldr (lambda (a->b b*)
           (check-type a->b function)
           (foldr (lambda (a bs) (cons (funcall a->b a) bs)) b* a*))
         '()
         a->*b))

(defmethod mmap (a->b* (a* list))
  (check-type a->b* function)
  (foldr (lambda (a b*) (append (funcall a->b* a) b*))
         '()
         a*))

(defmethod mzero ((class (eql 'list)))
  '())

(defmethod mplus ((monoid1 list) monoid2)
  (check-type monoid2 list)
  (append monoid1 monoid2))


;;; Utility
;; List Comprehension
(defmacro lc (element &body clauses)
  `(mdo ,@(mapcar (lambda (clause)
                    (if (listp clause)
                        (case (first clause)
                          ((:in :let) clause)
                          (otherwise  `(guard 'list ,clause)))
                        `(guard 'list ,clause)))
                  clauses)
        (unit 'list ,element)))
