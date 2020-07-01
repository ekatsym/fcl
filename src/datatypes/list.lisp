(defpackage fcl.datatypes.list
  (:nicknames :fcl.dt.list)
  (:use :common-lisp)
  (:import-from
    :fcl
    #:genlist)
  (:import-from
    :fcl.foldable
    #:foldr
    #:foldl
    #:foldr+
    #:foldl+
    #:unfoldr
    #:unfoldl
    #:unfoldr+
    #:unfoldl+)
  (:import-from
    :fcl.monad
    #:unit
    #:fmap
    #:amap
    #:mmap)
  (:import-from
    :fcl.monoid
    #:mzero
    #:mplus)
  (:import-from
    :fcl.monad+
    #:guard))
(in-package :fcl.dt.list)


;;; General Utility
(defun enum (start end)
  (check-type start integer)
  (check-type end integer)
  (do ((n (1- end) (1- n))
       (acc '() (cons n acc)))
      ((< n start) acc)))


;;; Foldable
(defmethod foldr (a&x->x x0 (a* list))
  (check-type a&x->x function)
  (foldl a&x->x x0 (reverse a*)))

(defmethod foldl (a&x->x x0 (a* list))
  (check-type a&x->x function)
  (do ((lst a* (rest lst))
       (acc x0 (funcall a&x->x (first lst) acc)))
      ((null lst) acc)))

(defmethod foldr+ (a&x&a*->x x0 (a* list))
  (check-type a&x&a*->x function)
  (do ((stack (reverse a*) (rest stack))
       (lst '() (cons (first stack) lst))
       (acc x0 (funcall a&x&a*->x (first stack) acc lst)))
      ((null lst) acc)))

(defmethod foldl+ (a&x&a*->x x0 (a* list))
  (check-type a&x&a*->x function)
  (do ((lst a* (rest lst))
       (acc x0 (funcall (first lst) acc lst)))
      ((null lst) acc)))

(defmethod unfoldr ((class (eql 'list)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (reverse (unfoldl class x->? x->a x->x x)))

(defmethod unfoldl ((class (eql 'list)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (do ((lzt x (funcall x->x lzt))
       (acc '() (cons (funcall x->a lzt) acc)))
      ((funcall x->? lzt) acc)))

(defmethod unfoldr+ ((class (eql 'list)) x->? x->a x->x a* x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (revappend (unfoldl class x->? x->a x->x x) a*))

(defmethod unfoldl+ ((class (eql 'list)) x->? x->a x->x a* x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (do ((lzt x (funcall x->x lzt))
       (acc a* (cons (funcall x->a lzt) acc)))
      ((funcall x->? lzt) acc)))


;;; Monad
(defmethod unit ((class (eql 'list)) a)
  (list a))

(defmethod fmap (a->b (a* list))
  (check-type a->b function)
  (mapcar a->b a*))

(defmethod amap (a->*b (a* list))
  (check-type a->*b list)
  (every (lambda (a->b) (check-type a->b function)) a->*b)
  (foldr (lambda (a->b acc)
           (foldr (lambda (a acc)
                    (cons (funcall a->b a) acc))
                  acc
                  a*))
         '()
         a->*b))

(defmethod mmap (a->b* (a* list))
  (check-type a->b* function)
  (foldr (lambda (a acc) (append (the list (funcall a->b* a)) acc))
         '()
         (reverse a*)))


;;; Monoid
(defmethod mzero ((class (eql 'list)))
  '())

(defmethod mplus ((monoid1 list) monoid2)
  (check-type monoid2 list)
  (append monoid1 monoid2))

(defmacro genlist (element &rest clauses)
  `(monad-do ,@(mapcar (lambda (clause)
                         (if (listp clause)
                             (case (first clause)
                               (:in clause)
                               (:let clause)
                               (otherwise `(guard 'list ,clause)))
                             `(guard 'list ,clause)))
                       clauses)
             (unit 'list ,element)))
