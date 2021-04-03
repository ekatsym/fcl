(defpackage fcl.datatypes.list
  (:nicknames :fcl.dt.list :fcl.list)
  (:use
    :common-lisp
    :fcl.util.list
    :fcl.generics.foldable
    :fcl.generics.monad-plus)
  (:import-from
    :fcl.util
    #:index)
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
    #:guard

    #:lc
    #:take-while
    #:drop-while
    #:span
    #:split-at
    #:partition
    #:filter
    #:mappend
    #:transpose
    #:sublists
    #:permutations
    #:flatten
    #:argmax
    #:argmin))
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

(defmethod para (i&*x->x (i list))
  "I&*X == (NOTHING) | (JUST (LIST A I X))"
  (check-type i&*x->x function)
  (do ((as-s (reverse+ i) (rest as-s))
       (x (funcall i&*x->x (nothing))
          (funcall i&*x->x (just (list (first (first as-s)) (first as-s) x)))))
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

(defmethod foldt (a&xs->x x0 (at list))
  (declare (optimize (speed 3)))
  (check-type a&xs->x function)
  (labels ((rec (at)
             (declare (type function a&xs->x))
             (if (endp at)
                 x0
                 (funcall a&xs->x (first at) (mapcar #'rec (rest at))))))
    (rec at)))

(defmethod foldt+ (at&xs->x x0 (at list))
  (declare (optimize (speed 3)))
  (check-type at&xs->x function)
  (labels ((rec (at)
             (declare (type function at&xs->x))
             (if (endp at)
                 x0
                 (funcall at&xs->x at (mapcar #'rec (rest at))))))
    (rec at)))

(defmethod unfoldt ((class (eql 'list)) x->? x->a x->xs x)
  (declare (optimize (speed 3)))
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->xs function)
  (labels ((rec (x)
             (declare (type function x->? x->a x->xs))
             (if (funcall x->? x)
                 nil
                 (cons (funcall x->a x) (mapcar #'rec (funcall x->xs x))))))
    (rec x)))

(defmethod unfoldt+ ((class (eql 'list)) x->? x->a x->xs at0 x)
  (declare (optimize (speed 3)))
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->xs function)
  (check-type at0 list)
  (labels ((rec (x)
             (declare (type function x->? x->a x->xs))
             (if (funcall x->? x)
                 at0
                 (cons (funcall x->a x) (mapcar #'rec (funcall x->xs x))))))
    (rec x)))


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

;;; Utility
(defun take-while (predicate list)
  (check-type predicate function)
  (check-type list list)
  (do ((tail list (rest tail))
       (rhead '() (cons (first tail) rhead)))
      ((not (funcall predicate (first tail))) (nreverse rhead))))

(defun drop-while (predicate list)
  (check-type predicate function)
  (check-type list list)
  (do ((tail list (rest tail)))
      ((not (funcall predicate (first tail))) tail)))

(defun span (predicate list)
  (check-type predicate function)
  (check-type list list)
  (do ((tail list (rest tail))
       (rhead '() (cons (first tail) rhead)))
      ((not (funcall predicate (first tail))) (list (nreverse rhead) tail))))

(defun split-at (n list)
  (check-type list list)
  (check-type n index)
  (do ((tail list (rest tail))
       (rhead '() (cons (first tail) rhead))
       (i 0 (1+ i)))
      ((>= i n) (list (reverse rhead) tail))))

(defun partition (predicate list)
  (check-type predicate function)
  (check-type list list)
  (do ((lst list (rest lst))
       (acc1 '())
       (acc2 '()))
      ((endp lst) (list (nreverse acc1) (nreverse acc2)))
      (if (funcall predicate (first lst))
          (push (first lst) acc1)
          (push (first lst) acc2))))

(defun filter (function list &rest more-lists)
  (check-type function function)
  (check-type list list)
  (mapc (lambda (l) (check-type l list)) more-lists)
  (foldr (lambda (as x)
           (let ((b (apply function as)))
             (if (null b)
                 x
                 (cons b x))))
         '()
         (apply #'zip list more-lists)))

(defun mappend (function list &rest more-lists)
  (check-type function function)
  (check-type list list)
  (mapc (lambda (l) (check-type l list)) more-lists)
  (mmap (lambda (as) (apply function as)) (apply #'zip list more-lists)))

(defun transpose (lists)
  (check-type lists list)
  (mapc (lambda (lst) (check-type lst list)) lists)
  (do ((lsts (filter #'identity lists) (filter #'rest lsts))
       (acc '() (cons (mapcar #'first lsts) acc)))
      ((every #'endp lsts) (nreverse acc))))

(defun sublists (list)
  (check-type list list)
  (cons '()
        (foldr (lambda (x yss)
                 (cons (list x)
                       (foldr (lambda (ys zss) (list* ys (cons x ys) zss))
                              '()
                              yss)))
               '()
               list)))

(defun permutations (list)
  (check-type list list)
  (let ((n 0))
    (foldr (lambda (x acc)
             (mmap (lambda (l)
                     (loop :repeat (incf n)
                           :for i :from 0
                           :collect (insert-at i x l)))
                   acc))
           '(())
           list)))

(defun flatten (list)
  (check-type list list)
  (foldr (lambda (x acc)
           (if (listp x)
               (append (flatten x) acc)
               (cons x acc)))
         '()
         list))

(defun argmax (function list)
  (check-type function function)
  (check-type list list)
  (ematch list
    ((cons hd tl)
     (apply
       #'values
       (foldl (lambda (acc new)
                (ematch acc
                  ((list _ v1)
                   (let ((v2 (funcall function new)))
                     (if (> v2 v1) (list new v2) acc)))))
              (list hd (funcall function hd))
              tl)))))

(defun argmin (function list)
  (check-type function function)
  (check-type list list)
  (ematch list
    ((cons hd tl)
     (apply
       #'values
       (foldl (lambda (acc new)
                (ematch acc
                  ((list _ v1)
                   (let ((v2 (funcall function new)))
                     (if (< v2 v1) (list new v2) acc)))))
              (list hd (funcall function hd))
              tl)))))
