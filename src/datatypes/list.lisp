(defpackage fcl.datatypes.list
  (:nicknames :fcl.dt.list)
  (:use :common-lisp)
  (:import-from
    :fcl
    #:enum
    #:take
    #:drop
    #:sublist
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
    #:mplus
    #:msum)
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
    #:msum
    #:guard
    #:genlist))

(in-package :fcl.dt.list)


;;; Foldable
(defmethod foldr (a&x->x x0 (a* list))
  (declare (optimize (speed 3)))
  (check-type a&x->x function)
  (let ((acc x0))
    (dolist (a (reverse a*))
      (setq acc (funcall a&x->x a acc)))
    acc))

(defmethod foldl (a&x->x x0 (a* list))
  (declare (optimize (speed 3)))
  (check-type a&x->x function)
  (let ((acc x0))
    (dolist (a a*)
      (setq acc (funcall a&x->x a acc)))
    acc))

(defmethod foldr+ (a&a*&x->x x0 (a* list))
  (declare (optimize (speed 3)))
  (check-type a&a*&x->x function)
  (do ((lst (reverse a*) (rest lst))
       (acc x0 (funcall a&a*&x->x (first lst) lst acc)))
      ((endp lst) acc)))

(defmethod foldl+ (a&a*&x->x x0 (a* list))
  (declare (optimize (speed 3)))
  (check-type a&a*&x->x function)
  (do ((lst a* (rest lst))
       (acc x0 (funcall a&a*&x->x (first lst) lst acc)))
      ((endp lst) acc)))

(defmethod unfoldr ((class (eql 'list)) x->? x->a x->x x)
  (declare (optimize (speed 3)))
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
  (declare (optimize (speed 3)))
  (check-type a->b function)
  (mapcar a->b a*))

(defmethod amap (a->*b (a* list))
  (declare (optimize (speed 3)))
  (check-type a->*b list)
  (let ((acc '()))
    (dolist (a->b a->*b)
      (check-type a->b function)
      (dolist (a a*)
        (push (funcall a->b a) acc)))
    (nreverse acc)))

(defmethod mmap (a->b* (a* list))
  (declare (optimize (speed 3)))
  (check-type a->b* function)
  (let ((acc '()))
    (dolist (a a*)
      (setq acc (revappend (the list (funcall a->b* a)) acc)))
    (nreverse acc)))


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
  (check-type start number)
  (check-type end number)
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
      (when (endp lst)
        (error "Out of index: ~S" n))))

(defun drop (n list)
  (check-type n index)
  (check-type list list)
  (let ((lst list))
    (dotimes (i n)
      (when (endp lst)
        (error "Out of index: ~S" n))
      (setq lst (rest lst)))
    lst))

(declaim (inline sublist))
(defun sublist (start end list)
  (check-type start index)
  (check-type end index)
  (check-type list list)
  (take (- end start) (drop start list)))

#|
breakable folding functions

(defmethod foldr (a&x->x x0 (a* list) &optional a->? a->x)
  (declare (optimize (speed 3)))
  (check-type a&x->x function)
  (check-type a->? (or null function))
  (check-type a->x (or null function))
  (assert (or (and a->? a->x) (and (not a->?) (not a->x)))
          (a->? a->x)
          "One of A->? and A->X is true and the other is false.")
  (if a->?
      (let ((stack '())
            (acc x0))
        ;; set STACK
        (dolist (a a*)
          (declare (type function a->? a->x))
          (if (funcall a->? a)
              (return (setq acc (funcall a->x a))) ; break
              (push (partial a&x->x a) stack)))
        ;; use STACK
        (dolist (func stack acc)
          (declare (type function func))
          (setq acc (funcall func acc))))
      (foldl a&x->x x0 (reverse a*))))

(defmethod foldl (a&x->x x0 (a* list) &optional a->? a->x)
  (declare (optimize (speed 3)))
  (check-type a&x->x function)
  (check-type a->? (or null function))
  (check-type a->x (or null function))
  (assert (or (and a->? a->x) (and (not a->?) (not a->x)))
          (a->? a->x)
          "One of A->? and A->X is true and the other is false.")
  (if a->?
      (foldr a&x->x x0 (reverse a*) a->? a->x)
      (let ((acc x0))
        (dolist (a a* acc)
          (setq acc (funcall a&x->x a acc))))))

(defmethod foldr+ (a&a*&x->x x0 (a* list) &optional a&a*->? a&a*->x)
  (declare (optimize (speed 3)))
  (check-type a&a*&x->x function)
  (check-type a&a*->? (or null function))
  (check-type a&a*->x (or null function))
  (assert (or (and a&a*->? a&a*->x) (and (not a&a*->?) (not a&a*->x)))
          (a&a*->? a&a*->x)
          "One of A&A*->? and A&A*->X is true and the other is false.")
  (if a&a*->?
      (let ((stack '())
            (acc x0))
        ;; set STACK
        (do ((lst a* (rest lst)))
            ((endp lst))
            (let ((a (first lst)))
              (declare (type function a&a*->? a&a*->x))
              (if (funcall a&a*->? a lst)
                  (return (setq acc (funcall a&a*->x a lst)))
                  (push (partial a&a*&x->x a lst) acc))))
        ;; use STACK
        (dolist (func stack acc)
          (declare (type function func))
          (setq acc (funcall func acc))))
      (foldl+ a&a*&x->x x0 (reverse a*))))

(defmethod foldl+ (a&a*&x->x x0 (a* list) &optional a&a*->? a&a*->x)
  (declare (optimize (speed 3)))
  (check-type a&a*&x->x function)
  (check-type a&a*->? (or null function))
  (check-type a&a*->x (or null function))
  (assert (or (and a&a*->? a&a*->x) (and (not a&a*->?) (not a&a*->x)))
          (a&a*->? a&a*->x)
          "One of A&A*->? and A&A*->X is true and the other is false.")
  (if a&a*->?
      (foldr+ a&a*&x->x x0 (reverse a*) a&a*->? a&a*->x)
      (do ((lst a* (rest lst))
           (acc x0 (funcall a&a*&x->x (first lst) lst acc)))
          ((endp lst) acc))))
|#

