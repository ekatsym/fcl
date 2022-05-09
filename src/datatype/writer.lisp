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
    #:writer #:run-writer
    #:setwt #:getwt #:dowt
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
  (fcl.unfoldable::unfoldr 'list #'lendp #'lfirst #'lrest llist))

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
    ((%queue _ head _ tail)
     (append (llist->list head) (reverse (llist->list tail))))))

(defun list->queue (list)
  (check-type list list)
  (%queue (length list) (fcl.foldable:foldr (lambda (x acc) (lcons x acc)) (lnil) list)
          0             (lnil)))


;;; Definition
(defdata writer
  (%writer t queue))

(defun run-writer (writer w)
  (ematch (mdo (setwt w) writer)
    ((%writer value message)
     (values value (queue->list message)))))

(defun setwt (w)
  (check-type w sequence)
  (%writer nil (list->queue (coerce w 'list))))

(defun getwt (writer)
  (ematch writer
    ((%writer a w)
     (%writer (list a (queue->list w)) w))))

(defun dowt (writer expression)
  (declare (ignore expression))
  writer)


;;; MONAD
(defmethod unit ((class (eql 'writer)) a)
  (%writer a (empty)))

(define-fmap-by-monad writer)

(define-amap-by-monad writer)

(defmethod mmap (a->b* (a* writer))
  (check-type a->b* function)
  (ematch a*
    ((%writer a w0)
     (ematch (funcall a->b* a)
       ((%writer b w1)
        (%writer b (qappend w0 w1)))))))
