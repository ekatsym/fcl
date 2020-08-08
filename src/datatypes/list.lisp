(defpackage fcl.datatypes.list
  (:nicknames :fcl.dt.list)
  (:use :common-lisp)
  (:import-from
    :fcl
    #:enum
    #:take
    #:drop
    #:genlist)
  (:import-from
    :fcl.util
    #:index
    #:filter
    #:partial)
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
    #:mmap
    #:monad-do)
  (:import-from
    :fcl.monoid
    #:mzero
    #:mplus)
  (:import-from
    :fcl.monad+
    #:guard)
  (:export
    #:enum
    #:take
    #:drop
    #:sublist
    #:filter
    #:foldr
    #:foldl
    #:foldr+
    #:foldl+
    #:unfoldr
    #:unfoldl
    #:unfoldr+
    #:unfoldl+
    #:unit
    #:fmap
    #:amap
    #:mmap
    #:monad-do
    #:mzero
    #:mplus
    #:guard
    #:genlist))

(in-package :fcl.dt.list)


;;; Foldable
(defmethod foldr (a&x->x x0 (a* list))
  (declare (optimize (speed 3)))
  (check-type a&x->x function)
  (do ((lst (reverse a*) (rest lst))
       (acc x0 (funcall a&x->x (first lst) acc)))
      ((endp lst) acc)))

(defmethod foldl (a&x->x x0 (a* list))
  (declare (optimize (speed 3)))
  (check-type a&x->x function)
  (do ((lst a* (rest lst))
       (acc x0 (funcall a&x->x (first lst) acc)))
      ((endp lst) acc)))

(defmethod foldr+ (a&a*&x->x x0 a*->? a*->x (a* list))
  (check-type a&a*&x->x function)
  (check-type a*->? function)
  (check-type a*->x function)
  (do ((lst (reverse a*) (rest lst))
       (acc x0 (funcall a&a*&x->x (first lst) lst acc)))
      ((endp lst) acc)
      (when (funcall a*->? lst)
        (return (funcall a*->x lst)))))

(defmethod foldl+ (a&a*&x->x x0 a*->? a*->x (a* list))
  (declare (optimize (speed 3)))
  (check-type a&a*&x->x function)
  (check-type a*->? function)
  (check-type a*->x function)
  (do ((lst a* (rest lst))
       (acc x0 (funcall a&a*&x->x (first lst) lst acc)))
      ((endp lst) acc)
      (when (funcall a*->? lst)
        (return (funcall a*->x lst)))))

(defmethod unfoldr ((class (eql 'list)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (do ((lzt x (funcall x->x lzt))
       (acc '() (cons (funcall x->a lzt) acc)))
      ((funcall x->? lzt) (nreverse acc))))

(defmethod unfoldl ((class (eql 'list)) x->? x->a x->x x)
  (declare (optimize (speed 3)))
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (do ((lzt x (funcall x->x lzt))
       (acc '() (cons (funcall x->a lzt) acc)))
      ((funcall x->? lzt) acc)))

(defmethod unfoldr+ ((class (eql 'list)) x->? x->a x->x a* x)
  (declare (optimize (speed 3)))
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (do ((lzt x (funcall x->x lzt))
       (acc a* (cons (funcall x->a lzt) acc)))
      ((funcall x->? lzt) (nreverse acc))))

(defmethod unfoldl+ ((class (eql 'list)) x->? x->a x->x a* x)
  (declare (optimize (speed 3)))
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (do ((lzt x (funcall x->x lzt))
       (acc a* (cons (funcall x->a lzt) acc)))
      ((funcall x->? lzt) acc)))


;;; Functor, Applicative and Monad
(defmethod unit ((class (eql 'list)) a)
  (list a))

(defmethod fmap (a->b (a* list))
  (check-type a->b function)
  (mapcar a->b a*))

(defmethod amap (a->*b (a* list))
  (check-type a->*b list)
  (every (lambda (a->b) (check-type a->b function)) a->*b)
  (foldr (lambda (a->b acc1)
           (foldr (lambda (a acc2)
                    (cons (funcall a->b a) acc2))
                  acc1
                  a*))
         '()
         a->*b))

(defmethod mmap (a->b* (a* list))
  (check-type a->b* function)
  (foldr (lambda (a acc) (append (funcall a->b* a) acc))
         '()
         a*))


;;; Monoid
(defmethod mzero ((class (eql 'list)))
  '())

(defmethod mplus ((monoid1 list) monoid2)
  (check-type monoid2 list)
  (append monoid1 monoid2))


;;; List Comprehension
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


;;; General Utility
(defun enum (start end)
  (check-type start integer)
  (check-type end integer)
  (do ((n (1- end) (1- n))
       (acc '() (cons n acc)))
      ((< n start) acc)))

(defun take (n list)
  (check-type n index)
  (check-type list list)
  (do ((n n (1- n))
       (lst list (rest lst))
       (acc '() (cons (first lst) acc)))
      ((<= n 0) (nreverse acc))
      (check-type lst cons)))

(declaim (inline drop))
(defun drop (n list)
  (check-type n index)
  (check-type list list)
  (nthcdr n list))

(declaim (inline sublist))
(defun sublist (start end list)
  (check-type start index)
  (check-type end index)
  (check-type list list)
  (take (- end start) (drop start list)))
