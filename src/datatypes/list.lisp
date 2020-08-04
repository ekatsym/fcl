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
    #:mmap
    #:monad-do)
  (:import-from
    :fcl.monoid
    #:mzero
    #:mplus)
  (:import-from
    :fcl.monad+
    #:guard)
  (:import-from
    :fcl.util
    #:index))
(in-package :fcl.dt.list)


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


;;; Foldable
(defmethod foldr (a&x->x x0 (a* list))
  (declare (optimize (speed 3)))
  (check-type a&x->x function)
  (labels ((rec (lst)
             (declare (optimize (speed 3)) (type function a&x->x))
             (if (endp lst)
                 x0
                 (funcall a&x->x (first lst) (rec (rest lst))))))
    (rec a*)))

(defmethod foldl (a&x->x x0 (a* list))
  (declare (optimize (speed 3)))
  (check-type a&x->x function)
  (do ((lst a* (rest lst))
       (acc x0 (funcall a&x->x (first lst) acc)))
      ((null lst) acc)))

(defmethod foldr+ (a&x&a*->x x0 (a* list))
  (check-type a&x&a*->x function)
  (labels ((rec (lst)
             (declare (optimize (speed 3)) (type function a&x&a*->x))
             (if (endp lst)
                 x0
                 (funcall a&x&a*->x (first lst) (rec (rest lst)) lst))))
    (rec a*)))

(defmethod foldl+ (a&x&a*->x x0 (a* list))
  (declare (optimize (speed 3)))
  (check-type a&x&a*->x function)
  (do ((lst a* (rest lst))
       (acc x0 (funcall a&x&a*->x (first lst) acc lst)))
      ((null lst) acc)))

(defmethod unfoldr ((class (eql 'list)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (labels ((rec (lzt)
             (declare (optimize (speed 3)) (type function x->? x->a x->x))
             (if (funcall x->? lzt)
                 '()
                 (cons (funcall x->a lzt) (rec (funcall x->x lzt))))))
    (rec x)))

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
  (labels ((rec (lzt)
             (declare (optimize (speed 3)) (type function x->? x->a x->x))
             (if (funcall x->? lzt)
                 a*
                 (cons (funcall x->a lzt) (rec (funcall x->x lzt))))))
    (rec x)))

(defmethod unfoldl+ ((class (eql 'list)) x->? x->a x->x a* x)
  (declare (optimize (speed 3)))
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
  (let ((a* (reverse a*)))
    (foldl (lambda (a->b acc)
             (foldl (lambda (a acc)
                      (cons (funcall a->b a) acc))
                    acc
                    a*))
           '()
           (reverse a->*b))))

(defmethod mmap (a->b* (a* list))
  (check-type a->b* function)
  (foldl (lambda (a acc) (revappend (funcall a->b* a) acc))
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
