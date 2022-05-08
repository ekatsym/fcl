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

    ;; monad+
    #:unit #:fmap #:amap #:mmap
    #:lift1 #:lift2 #:liftn
    #:mlet #:mprogn #:mdo
    #:define-fmap-by-applicative
    #:define-fmap-by-monad
    #:define-amap-by-monad
    #:mzero #:mplus #:msum
    #:guard

    ;; foldable / unfoldable
    #:foldr #:foldr+ #:unfoldr #:unfoldr+
    #:foldl #:foldl+ #:unfoldl #:unfoldl+
    #:lfoldr #:lfoldr+
    #:lfoldl #:lfoldl+
    #:scanr #:scanr+ #:scanl #:scanl+))
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

(defun add (x queue)
  (check-type queue queue)
  (ematch queue
    ((%queue '() _) (%queue (list x) '()))
    ((%queue l1 l2) (%queue l1 (cons x l2)))))

(defun head (queue)
  (check-type queue queue)
  (ematch queue
    ((%queue (cons x _) _) x)
    ((%queue '() _)        (error 'queue-empty))))

(defun tail (queue)
  (check-type queue queue)
  (ematch queue
    ((%queue (cons _ '()) l2) (%queue (reverse l2) '()))
    ((%queue (cons _ l1-) l2) (%queue l1- l2))
    ((%queue '() _)           (error 'queue-empty))))


;;; LIST Convertions
(defun queue->list (queue &optional (fifo? t))
  (check-type queue queue)
  (ematch queue
    ((%queue l1 l2) (if fifo?
                        (append l1 (reverse l2))
                        (append l2 (reverse l1))))))

(defun list->queue (list &optional (fifo? nil))
  (check-type list list)
  (if fifo?
      (%queue (reverse list) '())
      (%queue list '())))

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
