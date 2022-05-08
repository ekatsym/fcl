(defpackage fcl.queue
  (:nicknames :fcl.data.queue :fcl.qu)
  (:use :common-lisp :fcl.monad-plus :fcl.foldable :fcl.unfoldable)
  (:import-from :fcl.adata #:defdata)
  (:import-from :fcl.match #:match #:ematch)
  (:export
    #:queue
    #:empty #:empty?
    #:add #:head #:tail

    ;; LIST convertions
    #:queue->list
    #:list->queue

    ;; foldable
    #:foldr #:foldr+
    #:foldl #:foldl+
    #:lfoldr #:lfoldr+
    #:lfoldl #:lfoldl+
    #:scanr #:scanr+ #:scanl #:scanl+

    ;; unfoldable
    #:unfoldr #:unfoldr+
    #:unfoldl #:unfoldl+

    ;; monad+
    #:unit #:fmap #:amap #:mmap
    #:lift1 #:lift2 #:liftn
    #:mlet #:mprogn #:mdo
    #:mzero #:mplus #:msum
    #:guard

    #:qc))
(in-package :fcl.queue)


;;; Core
(defdata queue
  (%queue list list))

(defun empty ()
  (%queue '() '()))

(defun empty? (queue)
  (check-type queue queue)
  (match queue
    ((%queue '() _) t)
    (_              nil)))

(defun check (queue)
  (match queue
    ((%queue '() '())      queue)
    ((%queue (cons _ _) _) queue)
    ((%queue '() l2)       (%queue (reverse l2) '()))))

(defun add (x queue)
  (check-type queue queue)
  (ematch queue
    ((%queue l1 l2) (check (%queue l1 (cons x l2))))))

(defun head (queue)
  (check-type queue queue)
  (ematch queue
    ((%queue (cons x _) _) x)
    ((%queue '() _)        (error 'queue-empty))))

(defun tail (queue)
  (check-type queue queue)
  (ematch queue
    ((%queue '() _)           (error 'queue-empty))
    ((%queue (cons _ l1-) l2) (check (%queue l1- l2)))))


;;; LIST Convertions
(defun queue->list (queue)
  (check-type queue queue)
  (ematch queue
    ((%queue l1 l2) (append l1 (reverse l2)))))

(defun list->queue (list)
  (check-type list list)
  (%queue list '()))


;;; Condition
(define-condition queue-empty (error)
  ()
  (:report
    (lambda (o s)
      (declare (ignore o))
      (format s "The given QUEUE is empty but expected non-empty."))))


;;; Printer
(defmethod print-object ((object %queue) stream)
  (check-type stream stream)
  (format stream "#<QUEUE ~S>" (queue->list object)))


;;; Foldable
(defmethod foldr (a&x->x x0 (as queue))
  (check-type a&x->x function)
  (ematch as
    ((%queue l1 l2)
     (foldr a&x->x (foldl (lambda (x a) (funcall a&x->x a x)) x0 l2) l1))))

(defmethod foldr+ (a&x&as->x x0 (as queue))
  (check-type a&x&as->x function)
  (ematch as
    ((%queue l1 l2)
     (foldr+ a&x&as->x (foldl+ (lambda (x a as) (funcall a&x&as->x a x as)) x0 l2) l1))))

(defmethod foldl (x&a->x x0 (as queue))
  (check-type x&a->x function)
  (ematch as
    ((%queue l1 l2)
     (foldr (lambda (a x) (funcall x&a->x x a)) (foldl x&a->x x0 l1) l2))))

(defmethod foldl+ (x&a&as->x x0 (as queue))
  (check-type x&a&as->x function)
  (ematch as
    ((%queue l1 l2)
     (foldr+ (lambda (a x as) (funcall x&a&as->x x a as)) (foldl+ x&a&as->x x0 l1) l2))))

(defmethod lfoldr (a&$x->x x0 (as queue))
  (check-type a&$x->x function)
  (ematch as
    ((%queue l1 l2)
     (lfoldr a&$x->x (lfoldl (lambda ($x a) (funcall a&$x->x a $x)) x0 l2) l1))))

(defmethod lfoldr+ (a&$x&as->x x0 (as queue))
  (check-type a&$x&as->x function)
  (ematch as
    ((%queue l1 l2)
     (lfoldr+ a&$x&as->x (lfoldl+ (lambda ($x a as) (funcall a&$x&as->x a $x as)) x0 l2) l1))))

(defmethod lfoldl ($x&a->x x0 (as queue))
  (check-type $x&a->x function)
  (ematch as
    ((%queue l1 l2)
     (lfoldr (lambda (a $x) (funcall $x&a->x $x a)) (lfoldl $x&a->x x0 l1) l2))))

(defmethod lfoldl+ ($x&a&as->x x0 (as queue))
  (check-type $x&a&as->x function)
  (ematch as
    ((%queue l1 l2)
     (lfoldr+ (lambda (a $x as) (funcall $x&a&as->x $x a as)) (lfoldl+ $x&a&as->x x0 l1) l2))))


;;; Unfoldable
(defmethod unfoldr ((class (eql 'queue)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (check (%queue (unfoldr 'list x->? x->a x->x x) '())))

(defmethod unfoldr+ ((class (eql 'queue)) x->? x->a x->x as0 x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (check-type as0 queue)
  (ematch as0
    ((%queue '() _) (unfoldr 'queue x->? x->a x->x x))
    ((%queue l1 l2) (check (%queue l1 (unfoldl+ 'list x->? x->x x->a l2 x))))))

(defmethod unfoldl ((class (eql 'queue)) x->? x->x x->a x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (check (%queue (unfoldl 'list x->? x->x x->a x) '())))

(defmethod unfoldl+ ((class (eql 'queue)) x->? x->x x->a as0 x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (check-type as0 queue)
  (ematch as0
    ((%queue '() _) (unfoldl 'queue x->? x->x x->a x))
    ((%queue l1 l2) (check (%queue (unfoldl+ 'list x->? x->x x->a l1 x) l2)))))

;;; Monad Plus
(defmethod unit ((class (eql 'queue)) a)
  (check (%queue (list a) '())))

(define-fmap-by-monad queue)

(define-amap-by-monad queue)

(defmethod mmap (a->b* (a* queue))
  (check-type a->b* function)
  (do ((a* a* (tail a*))
       (b* (empty) (mplus b* (funcall a->b* (head a*)))))
      ((empty? a*) b*)))

(defmethod mzero ((class (eql 'queue)))
  (empty))

(defmethod mplus ((monoid1 queue) (monoid2 queue))
  (ematch (cons monoid1 monoid2)
    ((cons (%queue l11 l12) (%queue l21 l22))
     (check (%queue l11 (append l22 (revappend l21 l12)))))))

(defmacro qc (element &body clauses)
  `(mdo ,@(mapcar (lambda (clause)
                    (if (listp clause)
                        (case (first clause)
                          ((:in :let) clause)
                          (otherwise `(guard 'queue ,clause)))
                        `(guard 'queue ,clause)))
                  clauses)
        (unit 'queue ,element)))
