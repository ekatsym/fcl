(defpackage fcl.datatypes.llist
  (:nicknames :fcl.dt.llist)
  (:use :common-lisp)
  (:import-from
    :fcl
    #:lazy-list
    #:lnil
    #:lcons
    #:lfirst
    #:lrest
    #:lendp
    #:genllist
    #:llist
    #:lenum
    #:ladjoin
    #:ltake
    #:ldrop
    #:llast
    #:lbutlast
    #:lsublist
    #:lreverse
    #:lappend
    #:lrevappend
    #:llength
    #:lcount
    #:lcount-if
    #:lcount-if-not
    #:lremove
    #:lremove-if
    #:lremove-if-not
    #:lsubstitute
    #:lsubstitute-if
    #:lsubstitute-if-not
    #:lfind
    #:lfind-if
    #:lfind-if-not
    #:lmember
    #:lmember-if
    #:lmember-if-not
    #:lposition
    #:lposition-if
    #:lposition-if-not
    #:lreplace
    #:lmapc
    #:lmapcar
    #:lmapcan
    #:lmapl
    #:lmaplist
    #:lmapcon
    #:lsearch
    #:lmismatch
    #:lsort)
  (:import-from
    :fcl.defdata
    #:defdata)
  (:import-from
    :fcl.util
    #:index
    #:constant
    #:compose
    #:partial)
  (:import-from
    :fcl.lazy
    #:delay
    #:force)
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
    #:mprogn
    #:mlet
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
    #:force
    #:delay
    #:lazy-list
    #:lnil
    #:lcons
    #:lfirst
    #:lrest
    #:lendp

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
    #:mprogn
    #:mlet
    #:monad-do

    #:mzero
    #:mplus
    #:msum

    #:guard

    #:llist
    #:lenum
    #:ladjoin
    #:ltake
    #:ldrop
    #:llast
    #:lbutlast
    #:lsublist
    #:lreverse
    #:lappend
    #:llength
    #:lrevappend
    #:lcount
    #:lcount-if
    #:lcount-if-not
    #:lremove
    #:lremove-if
    #:lremove-if-not
    #:lsubstitute
    #:lsubstitute-if
    #:lsubstitute-if-not
    #:lfind
    #:lfind-if
    #:lfind-if-not
    #:lmember
    #:lmember-if
    #:lmember-if-not
    #:lposition
    #:lposition-if
    #:lposition-if-not
    #:lreplace
    #:lmapc
    #:lmapcar
    #:lmapcan
    #:lmapl
    #:lmaplist
    #:lmapcon
    #:lsearch
    #:lmismatch
    #:lsort))

(in-package :fcl.dt.llist)


;;; Core
(defdata lazy-list
  (lnil)
  (lcons (:lazy t) (:lazy lazy-list)))

(defmethod print-object ((object lnil) stream)
  (format stream "(LNIL)"))

(defmethod print-object ((object lcons) stream)
  (format stream "(LCONS ~S ~S)"
          (lcons%0 object)
          (lcons%1 object)))

(declaim (inline lfirst))
(defun lfirst (llist)
  (check-type llist lazy-list)
  (lcons%0 llist))

(declaim (inline lrest))
(defun lrest (llist)
  (check-type llist lazy-list)
  (lcons%1 llist))

(declaim (inline lendp))
(defun lendp (llist)
  (check-type llist lazy-list)
  (typep llist 'lnil))


;;; Foldable
(defmethod foldr (a&x->x x0 (a* lazy-list))
  (declare (optimize (speed 3)))
  (check-type a&x->x function)
  (labels ((rec (llst)
             (declare (type function a&x->x))
             (if (lendp llst)
                 x0
                 (funcall a&x->x (lfirst llst) (delay (rec (lrest llst)))))))
    (rec a*)))

(defmethod foldl (a&x->x x0 (a* lazy-list))
  (declare (optimize (speed 3)))
  (check-type a&x->x function)
  (labels ((rec (llst acc)
             (declare (optimize (speed 3)) (type function a&x->x))
             (if (lendp llst)
                 acc
                 (rec (lrest llst) (funcall a&x->x (lfirst llst) (delay acc))))))
    (rec a* x0)))

(defmethod foldr+ (a&a*&x->x x0 (a* lazy-list))
  (declare (optimize (speed 3)))
  (check-type a&a*&x->x function)
  (labels ((rec (llst)
             (declare (type function a&a*&x->x))
             (if (lendp llst)
                 x0
                 (funcall a&a*&x->x (lfirst llst) llst (delay (rec (lrest llst)))))))
    (rec a*)))

(defmethod foldl+ (a&a*&x->x x0 (a* lazy-list))
  (declare (optimize (speed 3)))
  (check-type a&a*&x->x function)
  (labels ((rec (llst acc)
             (declare (optimize (speed 3)) (type function a&a*&x->x))
             (if (lendp llst)
                 acc
                 (rec (lrest llst) (delay (funcall a&a*&x->x (lfirst llst) llst acc))))))
    (rec a* x0)))

(defmethod unfoldr ((class (eql 'lazy-list)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (labels ((rec (lzt)
             (if (funcall x->? lzt)
                 (lnil)
                 (lcons (funcall x->a lzt) (rec (funcall x->x lzt))))))
    (rec x)))

(defmethod unfoldl ((class (eql 'lazy-list)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (labels ((rec (lzt acc)
             (if (funcall x->? lzt)
                 acc
                 (rec (funcall x->x lzt) (lcons (funcall x->a lzt) acc)))))
    (rec x (lnil))))

(defmethod unfoldr+ ((class (eql 'lazy-list)) x->? x->a x->x a* x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (labels ((rec (lzt)
             (if (funcall x->? lzt)
                 a*
                 (lcons (funcall x->a lzt) (rec (funcall x->x (lrest lzt)))))))
    (rec x)))

(defmethod unfoldl+ ((class (eql 'lazy-list)) x->? x->a x->x a* x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (do ((lzt x (funcall x->x lzt))
       (acc a* (lcons (funcall x->a lzt) acc)))
      ((funcall x->? lzt) acc)))


;;; Functor, Applicative and Monad
(defmethod unit ((class (eql 'lazy-list)) a)
  (lcons a (lnil)))

(defmethod fmap (a->b (a* lazy-list))
  (declare (optimize (speed 3)))
  (check-type a->b function)
  (foldr (lambda (a $acc)
           (declare (type function a->b))
           (lcons (funcall a->b a) (force $acc)))
         (lnil)
         a*))

(defmethod amap (a->*b (a* lazy-list))
  (declare (optimize (speed 3)))
  (check-type a->*b lazy-list)
  (foldr (lambda (a->b $acc1)
           (check-type a->b function)
           (foldr (lambda (a $acc2)
                    (declare (type function a->b))
                    (lcons (funcall a->b a) (force $acc2)))
                  (force $acc1)
                  a*))
         (lnil)
         a->*b))

(defmethod mmap (a->b* (a* lazy-list))
  (declare (optimize (speed 3)))
  (check-type a->b* function)
  (foldr (lambda (a $acc1)
           (declare (type function a->b*))
           (foldr (lambda (b $acc2) (lcons b (force $acc2)))
                  (force $acc1)
                  (the lazy-list (funcall a->b* a))))
         (lnil)
         a*))


;;; Monoid
(defmethod mzero ((class (eql 'lazy-list)))
  (lnil))

(defmethod mplus ((monoid1 lazy-list) monoid2)
  (check-type monoid2 lazy-list)
  (lappend monoid1 monoid2))


;;; General Utility
(defmacro genllist (element &rest clauses)
  `(monad-do ,@(mapcar (lambda (clause)
                         (if (listp clause)
                             (case (first clause)
                               (:in clause)
                               (:let clause)
                               (otherwise `(guard 'lazy-list ,clause)))
                             `(guard 'lazy-list ,clause)))
                       clauses)
             (unit 'lazy-list ,element)))

(defmacro llist (&rest args)
  (foldr (lambda (arg acc) `(lcons ,arg ,acc))
         '(lnil)
         args))

(defun lenum (start &optional end)
  (check-type start number)
  (check-type end (or number null))
  (labels ((rec (n)
             (if (and end (>= n end))
                 (lnil)
                 (lcons n (rec (1+ n))))))
    (rec start)))

(declaim (inline %make-key))
(defun %make-key (key)
  (or key #'identity))

(declaim (inline %make-test))
(defun %make-test (test test-not default)
  (if test-not (complement test-not) (or test default)))

(defun %make-predicate (item key test test-not default)
  (let ((key (%make-key key))
        (test (%make-test test test-not default)))
    (lambda (x) (funcall test item (funcall key x)))))

(defmacro %test-assert (test test-not datum &rest args)
  `(assert (and ,test ,test-not) (,test ,test-not)
           ,datum ,@args))

(defun ladjoin (item llist &key key test test-not)
  (check-type llist lazy-list)
  (check-type key (or function null))
  (check-type test (or function null))
  (check-type test-not (or function null))
  (%test-assert test test-not "can't specify both :TEST and :TEST-NOT")
  (if (lfind item llist :key key :test test :test-not test-not)
      llist
      (lcons item llist)))

(defun ltake (n llist)
  (check-type n index)
  (check-type llist lazy-list)
  (labels ((rec (n llst)
             (cond ((zerop n)    (lnil))
                   ((lendp llst) (error "Out of index: ~S" n))
                   (t            (lcons (lfirst llst) (rec (1- n) (lrest llst)))))))
    (rec n llist)))

(defun ldrop (n llist)
  (check-type n index)
  (check-type llist lazy-list)
  (labels ((rec (n llst)
             (declare (optimize (speed 3)) (type index n))
             (check-type llst lcons)
             (if (zerop n)
                 llst
                 (rec (1- n) (lrest llst)))))
    (rec n llist)))

(defun lsublist (start end llist)
  (check-type start index)
  (check-type end (or index null))
  (check-type llist lazy-list)
  (if end
      (ltake (- end start) (ldrop start llist))
      (ldrop start llist)))

(defun lreverse (llist)
  (check-type llist lazy-list)
  (foldl (lambda (x $acc) (lcons x (force $acc))) (lnil) llist))

(defun lappend (&rest llists)
  (if (null llists)
      (lnil)
      (foldr (lambda (llst acc1)
               (check-type llst lazy-list)
               (foldr (lambda (x $acc2) (lcons x (force $acc2)))
                      acc1
                      llst))
             (first (last llists))
             (butlast llists))))

(defun lrevappend (llist1 llist2)
  (check-type llist1 lazy-list)
  (check-type llist2 lazy-list)
  (foldl (lambda (x $acc) (lcons x (force $acc)))
         llist2
         llist1))

(defun llength (llist)
  (check-type llist lazy-list)
  (labels ((rec (llst acc)
             (declare (optimize (speed 3)) (type index acc))
             (if (lendp llst)
                 acc
                 (rec (lrest llst) (1+ acc)))))
    (rec llist 0)))

(defun %lcount-if (pred llist)
  (declare (optimize (speed 3)) (type function pred))
  (foldr (lambda (x $acc)
           (if (funcall pred x)
               (1+ (the index (force $acc)))
               (force $acc)))
         0
         llist))

(defun %lcount-if-from-end (pred llist)
  (declare (optimize (speed 3)) (type function pred))
  (foldl (lambda (x $acc)
           (if (funcall pred x)
               (1+ (the index (force $acc)))
               (force $acc)))
         0
         llist))

(defun lcount (item llist &key from-end (start 0) end key test test-not)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type key (or function null))
  (check-type test (or function null))
  (check-type test-not (or function null))
  (let ((pred (%make-predicate item key test test-not #'eql)))
    (if from-end
        (%lcount-if-from-end pred (lsublist start end llist))
        (%lcount-if pred (lsublist start end llist)))))

(defun lcount-if (predicate llist &key from-end (start 0) end key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type key (or function null))
  (let ((pred (if key
                  (compose predicate key)
                  predicate)))
    (if from-end
        (%lcount-if-from-end pred (lsublist start end llist))
        (%lcount-if pred (lsublist start end llist)))))

(defun lcount-if-not (predicate llist &key from-end (start 0) end key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type key (or function null))
  (let ((pred (if key
                  (complement (compose predicate key))
                  (complement predicate))))
    (if from-end
        (%lcount-if-from-end pred (lsublist start end llist))
        (%lcount-if pred (lsublist start end llist)))))

(defun %lremove-if (pred llist start end)
  (labels ((rec (i llst)
             (declare (optimize (speed 3))
                      (type function pred)
                      (type index i start)
                      (type (or index null) end))
             (cond ((lendp llst)
                    (lnil))
                   ((and end (>= i end))
                    llst)
                   ((< i start)
                    (lcons (lfirst llst) (rec (1+ i) (lrest llst))))
                   ((funcall pred (lfirst llst))
                    (rec (1+ i) (lrest llst)))
                   (t
                    (lcons (lfirst llst) (rec (1+ i) (lrest llst)))))))
    (rec 0 llist)))

(defun %lremove-if-count (pred n llist start end)
  (labels ((rec (i n llst)
             (declare (optimize (speed 3))
                      (type function pred)
                      (type index i n start)
                      (type (or index null) end))
             (cond ((lendp llst)
                    (lnil))
                   ((zerop n)
                    llst)
                   ((and end (>= i end))
                    llst)
                   ((< i start)
                    (lcons (lfirst llst) (rec (1+ i) n (lrest llst))))
                   ((funcall pred (lfirst llst))
                    (rec (1+ i) (1- n) (lrest llst)))
                   (t
                    (lcons (lfirst llst) (rec (1+ i) n (lrest llst)))))))
    (rec 0 n llist)))

(defun %lremove-if-from-end (pred llist start end)
  (labels ((rec (i rllst acc)
             (declare (optimize (speed 3))
                      (type function pred)
                      (type index i start)
                      (type (or index null) end))
             (cond ((lendp rllst)
                    acc)
                   ((and end (>= i end))
                    (lrevappend rllst acc))
                   ((< i start)
                    (rec (1+ i) (lrest rllst) (lcons (lfirst rllst) acc)))
                   ((funcall pred (lfirst rllst))
                    (rec (1+ i) (lrest rllst) acc))
                   (t
                    (rec (1+ i) (lrest rllst) (lcons (lfirst rllst) acc))))))
    (rec 0 (lreverse llist) (lnil))))

(defun %lremove-if-count-from-end (pred n llist start end)
  (labels ((rec (i n rllst acc)
             (declare (optimize (speed 3))
                      (type function pred)
                      (type index i n start)
                      (type (or index null) end))
             (cond ((lendp rllst)
                    acc)
                   ((zerop n)
                    (lrevappend rllst acc))
                   ((and end (>= i end))
                    (lrevappend rllst acc))
                   ((< i start)
                    (rec (1+ i) n (lrest rllst) (lcons (lfirst rllst) acc)))
                   ((funcall pred (lfirst rllst))
                    (rec (1+ i) (1- n) (lrest rllst) acc))
                   (t
                    (rec (1+ i) n (lrest rllst) (lcons (lfirst rllst) acc))))))
    (rec 0 n (lreverse llist) (lnil))))

(defun %%lremove-if (pred llist from-end start end count)
  (cond ((and from-end count)
         (%lremove-if-count-from-end pred count llist start end))
        ((and from-end (not count))
         (%lremove-if-from-end pred llist start end))
        ((and (not from-end) count)
         (%lremove-if-count pred count llist start end))
        ((and (not from-end) (not count))
         (%lremove-if pred llist start end))))

(defun lremove (item llist &key from-end test test-not (start 0) end count key)
  (check-type llist lazy-list)
  (check-type test (or function null))
  (check-type test-not (or function null))
  (check-type start index)
  (check-type end (or index null))
  (check-type count (or index null))
  (check-type key (or function null))
  (let ((pred (%make-predicate item key test test-not #'eql)))
    (%%lremove-if pred llist from-end start end count)))

(defun lremove-if (predicate llist &key from-end (start 0) end count key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type count (or index null))
  (check-type key (or function null))
  (let ((pred (if key (compose predicate key) predicate)))
    (%%lremove-if pred llist from-end start end count)))

(defun lremove-if-not (predicate llist &key from-end (start 0) end count key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type count (or index null))
  (check-type key (or function null))
  (let ((pred (complement (if key (compose predicate key) predicate))))
    (%%lremove-if pred llist from-end start end count)))

(defun %lsubstitute-if (new pred llist start end)
  (labels ((rec (i llst)
             (declare (optimize (speed 3))
                      (type function pred)
                      (type index i start)
                      (type (or index null) end))
             (cond ((lendp llst)
                    (lnil))
                   ((and end (>= i end))
                    llst)
                   ((< i start)
                    (lcons (lfirst llst) (rec (1+ i) (lrest llst))))
                   ((funcall pred (lfirst llst))
                    (lcons new (rec (1+ i) (lrest llst))))
                   (t
                    (lcons (lfirst llst) (rec (1+ i) (lrest llst)))))))
    (rec 0 llist)))

(defun %lsubstitute-if-count (new pred n llist start end)
  (labels ((rec (i n llst)
             (declare (optimize (speed 3))
                      (type function pred)
                      (type index i n start)
                      (type (or index null) end))
             (cond ((lendp llst)
                    (lnil))
                   ((zerop n)
                    llst)
                   ((and end (>= i end))
                    llst)
                   ((< i start)
                    (lcons (lfirst llst) (rec (1+ i) n (lrest llst))))
                   ((funcall pred (lfirst llst))
                    (lcons new (rec (1+ i) (1- n) (lrest llst))))
                   (t
                    (lcons (lfirst llst) (rec (1+ i) n (lrest llst)))))))
    (rec 0 n llist)))

(defun %lsubstitute-if-from-end (new pred llist start end)
  (labels ((rec (i rllst acc)
             (declare (optimize (speed 3))
                      (type function pred)
                      (type index i start)
                      (type (or index null) end))
             (cond ((lendp rllst)
                    acc)
                   ((and end (>= i end))
                    (lrevappend rllst acc))
                   ((< i start)
                    (rec (1+ i) (lrest rllst) (lcons (lfirst rllst) acc)))
                   ((funcall pred (lfirst rllst))
                    (rec (1+ i) (lrest rllst) (lcons new acc)))
                   (t
                    (rec (1+ i) (lrest rllst) (lcons (lfirst rllst) acc))))))
    (rec 0 (lreverse llist) (lnil))))

(defun %lsubstitute-if-count-from-end (new pred n llist start end)
  (labels ((rec (i n rllst acc)
             (declare (optimize (speed 3))
                      (type function pred)
                      (type index i n start)
                      (type (or index null) end))
             (cond ((lendp rllst)
                    acc)
                   ((zerop n)
                    (lrevappend rllst acc))
                   ((and end (>= i end))
                    (lrevappend rllst acc))
                   ((< i start)
                    (rec (1+ i) n (lrest rllst) (lcons (lfirst rllst) acc)))
                   ((funcall pred (lfirst rllst))
                    (rec (1+ i) (1- n) (lrest rllst) (lcons new acc)))
                   (t
                    (rec (1+ i) n (lrest rllst) (lcons (lfirst rllst) acc))))))
    (rec 0 n (lreverse llist) (lnil))))

(defun %%lsubstitute-if (new pred llist from-end start end count)
  (cond ((and from-end count)
         (%lsubstitute-if-count-from-end new pred count llist start end))
        ((and from-end (not count))
         (%lsubstitute-if-from-end new pred llist start end))
        ((and (not from-end) count)
         (%lsubstitute-if-count new pred count llist start end))
        ((and (not from-end) (not count))
         (%lsubstitute-if new pred llist start end))))

(defun lsubstitute (new old llist &key from-end test test-not (start 0) count end key)
  (check-type llist lazy-list)
  (check-type test (or function null))
  (check-type test-not (or function null))
  (check-type start index)
  (check-type count (or index null))
  (check-type end (or index null))
  (check-type key (or function null))
  (let ((pred (%make-predicate old key test test-not #'eql)))
    (%%lsubstitute-if new pred llist from-end start end count)))

(defun lsubstitute-if (new predicate llist &key from-end (start 0) end count key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type count (or index null))
  (check-type key (or function null))
  (let ((pred (if key
                  (compose predicate key)
                  predicate)))
    (%%lsubstitute-if new pred llist from-end start end count)))

(defun lsubstitute-if-not (new predicate llist &key from-end (start 0) end count key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type count (or index null))
  (check-type key (or function null))
  (let ((pred (if key
                  (compose predicate key)
                  predicate)))
    (%%lsubstitute-if new (complement pred) llist from-end start end count)))

(defun lfind (item llist &key from-end (start 0) end key test test-not)
  (%test-assert test test-not "can't specify both :TEST and :TEST-NOT")
  (let ((pred (%make-predicate item key test test-not #'eql)))
    (lfind-if pred llist
              :from-end from-end
              :start start
              :end end
              :key key)))

(defun lfind-if (predicate llist &key from-end (start 0) end key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type key (or function null))
  (let ((pred (if key
                  (compose predicate key)
                  predicate)))
    (if from-end
        (foldl (lambda (x $acc)
                 (if (funcall pred x)
                     x
                     (force $acc)))
               nil
               (lsublist start end llist))
        (foldr (lambda (x $acc)
                 (if (funcall pred x)
                     x
                     (force $acc)))
               nil
               (lsublist start end llist)))))

(defun lfind-if-not (predicate llist &key from-end (start 0) end key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type key (or function null))
  (lfind-if-not (complement predicate) llist
                :from-end from-end
                :start start
                :end end
                :key key))

(defun lmember (item llist &key key test test-not)
  (check-type llist lazy-list)
  (check-type key (or function null))
  (check-type test (or function null))
  (check-type test-not (or function null))
  (%test-assert test test-not "can't specify both :TEST and :TEST-NOT")
  (let ((pred (%make-predicate item key test test-not #'eql)))
    (lmember-if pred llist)))

(defun lmember-if (predicate llist &key key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type key (or function null))
  (let ((pred (if key
                  (compose predicate key)
                  predicate)))
    (foldr+ (lambda (x xs $acc)
              (declare (type function pred))
              (if (funcall pred x)
                  xs
                  (force $acc)))
            nil
            llist)))

(defun lmember-if-not (predicate llist &key key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type key (or function null))
  (lmember-if (complement predicate) llist :key key))

(defun lposition (item llist &key from-end (start 0) end key test test-not)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type key (or function null))
  (check-type test (or function null))
  (check-type test-not (or function null))
  (%test-assert test test-not "can't specify both :TEST and :TEST-NOT")
  (let ((pred (%make-predicate item key test test-not #'eql)))
    (lposition-if pred llist
                  :from-end from-end
                  :start start
                  :end end)))

(defun lposition-if (predicate llist &key from-end (start 0) end key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type key (or function null))
  (let ((pred (if key
                  (compose predicate key)
                  predicate)))
    (if from-end
        (foldr (lambda (x $acc)
                 (if (funcall pred x)
                     (1- (or end (llength llist)))
                     (and (force $acc) (1- (force $acc)))))
               nil
               (lreverse (lsublist start end llist)))
        (foldr (lambda (x $acc)
                 (if (funcall pred x)
                     start
                     (and (force $acc) (1+ (force $acc)))))
               nil
               (lsublist start end llist)))))

(defun lposition-if-not (predicate llist &key from-end (start 0) end key)
  (check-type predicate function)
  (check-type llist lazy-list)
  (check-type start index)
  (check-type end (or index null))
  (check-type key (or function null))
  (lposition-if (complement predicate) llist
                :from-end from-end
                :start start
                :end end
                :key key))

(defun lreplace (target-llist1 source-llist2 &key (start1 0) end1 (start2 0) end2)
  (check-type target-llist1 lazy-list)
  (check-type source-llist2 lazy-list)
  (check-type start1 index)
  (check-type end1 (or index null))
  (check-type start2 index)
  (check-type end2 (or index null))
  (labels ((rec (i tgt src)
             (declare (optimize (speed 3))
                      (type index i start1)
                      (type (or index null) end1))
             (cond ((lendp tgt)
                    (lnil))
                   ((lendp src)
                    tgt)
                   ((< i start1)
                    (lcons (lfirst tgt) (rec (1+ i) (lrest tgt) src)))
                   ((and end1 (>= i end1))
                    tgt)
                   (t
                    (lcons (lfirst src) (rec (1+ i) (lrest tgt) (lrest src)))))))
    (rec 0 target-llist1 (lsublist start2 end2 source-llist2))))

(defun lmapc (function llist &rest more-llists)
  (check-type function function)
  (check-type llist lazy-list)
  (every (lambda (llst) (check-type llst lazy-list)) more-llists)
  (labels ((rec (llsts)
             (declare (optimize (speed 3))
                      (type function function))
             (unless (some #'lendp llsts)
               (apply function (mapcar #'first llsts))
               (rec (mapcar #'rest llsts)))))
    (rec (cons llist more-llists))))

(defun lmapcar (function llist &rest more-llists)
  (check-type function function)
  (check-type llist lazy-list)
  (every (lambda (llst) (check-type llst lazy-list)) more-llists)
  (labels ((rec (llsts)
             (declare (optimize (speed 3))
                      (type function function))
             (if (some #'lendp llsts)
                 (lnil)
                 (lcons (apply function (mapcar #'first llsts))
                        (rec (mapcar #'rest llsts))))))
    (rec (cons llist more-llists))))

(defun lmapcan (function llist &rest more-llists)
  (check-type function function)
  (check-type llist lazy-list)
  (every (lambda (llst) (check-type llst lazy-list)) more-llists)
  (labels ((rec (llsts)
             (declare (optimize (speed 3))
                      (type function function))
             (if (some #'lendp llsts)
                 (lnil)
                 (lappend (the lazy-list (apply function (mapcar #'first llsts)))
                          (rec (mapcar #'rest llsts))))))
    (rec (cons llist more-llists))))

(defun lmapl (function llist &rest more-llists)
  (check-type function function)
  (check-type llist lazy-list)
  (every (lambda (llst) (check-type llst lazy-list)) more-llists)
  (labels ((rec (llsts)
             (declare (optimize (speed 3))
                      (type function function))
             (unless (some #'lendp llsts)
               (apply function llsts)
               (rec (mapcar #'lrest llsts)))))
    (rec (cons llist more-llists))))

(defun lmaplist (function llist &rest more-llists)
  (check-type function function)
  (check-type llist lazy-list)
  (every (lambda (llst) (check-type llst lazy-list)) more-llists)
  (labels ((rec (llsts)
             (declare (optimize (speed 3))
                      (type function function))
             (if (some #'lendp llsts)
                 (lnil)
                 (lcons (apply function llsts)
                        (rec (mapcar #'rest llsts))))))
    (rec (cons llist more-llists))))

(defun lmapcon (function llist &rest more-llists)
  (check-type function function)
  (check-type llist lazy-list)
  (every (lambda (llst) (check-type llst lazy-list)) more-llists)
  (labels ((rec (llsts)
             (declare (optimize (speed 3))
                      (type function function))
             (if (some #'lendp llsts)
                 (lnil)
                 (lappend (apply function llsts)
                          (rec (mapcar #'rest llsts))))))
    (rec (cons llist more-llists))))

(defun lsearch (sub-llist1 main-llist2 &key from-end test test-not (start1 0) end1 (start2 0) end2 key)
  (check-type sub-llist1 lazy-list)
  (check-type main-llist2 lazy-list)
  (check-type test (or function null))
  (check-type test-not (or function null))
  (check-type start1 index)
  (check-type end1 (or index null))
  (check-type start2 index)
  (check-type end2 (or index null))
  (check-type key (or function null))
  (%test-assert test test-not "can't specify both :TEST and :TEST-NOT")
  (let ((test (%make-test test
                          test-not
                          (if key
                              (lambda (x y) (eql (funcall key x) (funcall key y)))
                              #'eql))))
    (declare (type function test))
    (labels ((rec- (pos sub main)
               (declare (optimize (speed 3))
                        (type index pos)
                        (type (or index null) end2))
               (cond ((lendp main)
                      nil)
                     ((and end2 (>= pos end2))
                      nil)
                     ((match? sub main)
                      pos)
                     (t
                      (rec- (1- pos) sub (lrest main)))))
             (rec+ (pos sub main)
               (declare (optimize (speed 3))
                        (type index pos))
               (cond ((lendp main)
                      nil)
                     ((match? sub main)
                      pos)
                     (t
                      (rec+ (1+ pos) sub (lrest main)))))
             (match? (sub main)
               (declare (optimize (speed 3)))
               (cond ((lendp sub)
                      t)
                     ((lendp main)
                      nil)
                     ((funcall test (lfirst sub) (lfirst main))
                      (match? (lrest sub) (lrest main)))
                     (t
                      nil))))
      (if from-end
          (rec- (or end2 (llength main-llist2))
                (lreverse (lsublist start1 end1 sub-llist1))
                (lreverse (lsublist start2 end2 main-llist2)))
          (rec+ start2
                (lsublist start1 end1 sub-llist1)
                (ldrop start2 main-llist2))))))

(defun lmismatch (llist1 llist2 &key from-end test test-not (start1 0) end1 (start2 0) end2 key)
  (check-type llist1 lazy-list)
  (check-type llist2 lazy-list)
  (check-type test (or function null))
  (check-type test-not (or function null))
  (%test-assert test test-not "can't specify both :TEST and :TEST-NOT")
  (check-type start1 index)
  (check-type end1 (or index null))
  (check-type start2 index)
  (check-type end2 (or index null))
  (check-type key (or function null))
  (let ((test (%make-test test
                          test-not
                          (if key
                              (lambda (x y) (eql (funcall key x) (funcall key y)))
                              #'eql))))
    (labels ((rec+ (pos llst1 llst2)
               (declare (optimize (speed 3))
                        (type function test)
                        (type index pos))
               (cond ((and (lendp llst1) (lendp llst2))
                      nil)
                     ((or (lendp llst1) (lendp llst2))
                      pos)
                     ((funcall test (lfirst llst1) (lfirst llst2))
                      (rec+ (1+ pos) (lrest llst1) (lrest llst2)))
                     (t
                      pos)))
             (rec- (pos llst1 llst2)
               (declare (optimize (speed 3))
                        (type function test)
                        (type index pos))
               (cond ((and (lendp llst1) (lendp llst2))
                      nil)
                     ((or (lendp llst1) (lendp llst2))
                      pos)
                     ((funcall test (lfirst llst1) (lfirst llst2))
                      (rec- (1- pos) (lrest llst1) (lrest llst2)))
                     (t
                      pos))))
      (if from-end
          (rec- (or end1 (llength llist1))
                (lreverse (lsublist start1 end1 llist1))
                (lreverse (lsublist start2 end2 llist2)))
          (rec+ start1 (lsublist start1 end1 llist1) (lsublist start2 end2 llist2))))))

(defun lsort (llist test &key key)
  (check-type llist lazy-list)
  (check-type test function)
  (check-type key (or function null))
  (let ((test (if key
                  (lambda (x y) (funcall test (funcall key x) (funcall key y)))
                  test))
        (n (llength llist)))
    (labels ((lmsort (n llst)
               (declare (optimize (speed 3))
                        (type index n))
               (if (or (lendp llst) (lendp (lrest llst)))
                   llst
                   (let ((m (ceiling n 2)))
                     (lmerge (lmsort m (lsublist 0 m llst))
                             (lmsort (- n m) (lsublist m nil llst))))))
             (lmerge (sorted-llst1 sorted-llst2)
               (declare (optimize (speed 3))
                        (type function test))
               (cond ((lendp sorted-llst1)
                      sorted-llst2)
                     ((lendp sorted-llst2)
                      sorted-llst1)
                     ((funcall test (lfirst sorted-llst1) (lfirst sorted-llst2))
                      (lcons (lfirst sorted-llst1) (lmerge (lrest sorted-llst1) sorted-llst2)))
                     (t
                      (lcons (lfirst sorted-llst2) (lmerge sorted-llst1 (lrest sorted-llst2)))))))
      (lmsort n llist))))
