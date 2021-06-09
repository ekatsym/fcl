(defpackage fcl.writer
  (:nicknames :fcl.data.writer :fcl.wt)
  (:use :common-lisp :fcl.monad)
  (:import-from
    :fcl.adata
    #:defdata)
  (:import-from
    :fcl.match
    #:match
    #:ematch)
  (:export
    #:writer #:get-message #:get-value
    #:setwt #:getwt #:modwt
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo))
(in-package :fcl.writer)


;;; LList
(defdata llist
  (lnil)
  (lcons (:lazy t) (:lazy llist)))

(defun lfirst (llist)
  (check-type llist llist)
  (lcons%0 llist))

(defun lrest (llist)
  (check-type llist llist)
  (lcons%1 llist))

(defun lendp (llist)
  (check-type llist llist)
  (typep llist 'lnil))

(defun llist->list (llist)
  (check-type llist llist)
  (fcl.foldable::unfoldr 'list #'lendp #'lfirst #'lrest llist))

(defmethod print-object ((object lnil) stream)
  (format stream "#.(LNIL)"))

(defmethod print-object ((object lcons) stream)
  (format stream "#.(LLIST ~{~S~^ ~})"
          (llist->list object)))

(defun lappend (llist &rest more-llists)
  (check-type llist llist)
  (mapc (lambda (llst) (check-type llst llist)) more-llists)
  (labels ((lapp2 (llst1 llst2)
             (ematch llst1
               ((lnil) llst2)
               ((lcons x1 rest1) (lcons x1 (lapp2 rest1 llst2))))))
    (let ((llists (cons llist more-llists)))
      (reduce #'lapp2 llists :from-end t))))

(defun lreverse (llist)
  (check-type llist llist)
  (labels ((rev (llst acc)
             (ematch llst
               ((lnil) acc)
               ((lcons x llst) (rev llst (lcons x acc))))))
    (rev llist (lnil))))


;;; Queue
(defdata queue
  (%queue integer llist integer llist))

(defmethod print-object ((object %queue) stream)
  (format stream "~S" (queue->list object)))

(defun empty ()
  (%queue 0 (lnil) 0 (lnil)))

(defun empty? (queue)
  (check-type queue queue)
  (match queue
    ((%queue 0 _ _ _) t)
    (_                nil)))

(defun check-queue (queue)
  (ematch queue
    ((%queue len1 head len2 tail)
     (if (>= len1 len2)
         queue
         (%queue (+ len1 len2) (lappend head (lreverse tail)) 0 (lnil))))))

(defun enqueue (x queue)
  (ematch queue
    ((%queue len1 head len2 tail)
     (check-queue (%queue len1 head (1+ len2) (lcons x tail))))))

(defun qfirst (queue)
  (ematch queue
    ((%queue _ (lcons x _) _ _) x)))

(defun qrest (queue)
  (ematch queue
    ((%queue len1 (lcons _ head) len2 tail)
     (check-queue (%queue (1- len1) head len2 tail)))))

(defun qappend (queue1 queue2)
  (declare (optimize (space 3)))
  (if (empty? queue2)
      queue1
      (qappend (enqueue (qfirst queue2) queue1)
               (qrest queue2))))

(defun queue->list (queue)
  (check-type queue queue)
  (ematch queue
    ((%queue head tail)
     (append (llist->list head) (reverse (llist->list tail))))))

(defun list->queue (list)
  (check-type list list)
  (%queue (length list) (fcl.foldable:foldr (lambda (x acc) (lcons x acc)) (lnil) list)
          0             (lnil)))


;;; Definition
(defdata writer
  (%writer queue t))

(defun get-message (writer)
  (ematch writer
    ((%queue message _)
     message)))

(defun get-value (writer)
  (ematch writer
    ((%queue _ value)
     value)))

(defun setwt (w)
  (check-type w sequence)
  (%writer (list->queue (coerce w 'list)) nil))

(defun getwt (writer)
  (ematch writer
    ((%writer w a) (%writer w (list a (queue->list w))))))

(defun modwt (writer)
  (ematch writer
    ((%writer w (list f a)) (%writer (funcall f w) a))))



;;; MONAD
(defmethod unit ((class (eql 'writer)) a)
  (%writer (empty) a))

(define-fmap-by-monad writer)

(define-amap-by-monad writer)

(defmethod mmap (a->b* (a* writer))
  (check-type a->b* function)
  (ematch a*
    ((%writer w0 a)
     (ematch (funcall a->b* a)
       ((%writer w1 b)
        (%writer (qappend w0 w1) b))))))
