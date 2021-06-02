(defpackage fcl.datatypes.llist
  (:nicknames :fcl.dt.llist :fcl.llist)
  (:use
    :common-lisp
    :fcl.generics.foldable
    :fcl.generics.monad-plus)
  (:import-from
    :fcl.lazy
    #:delay
    #:force)
  (:import-from
    :fcl.data
    #:defdata)
  (:import-from
    :fcl.match
    #:match
    #:ematch)
  (:import-from
    :fcl.datatypes.maybe
    #:maybe
    #:just
    #:nothing)
  (:import-from
    :fcl.datatypes.either
    #:either
    #:left
    #:right)
  (:import-from
    :fcl.util
    #:index
    #:compose
    #:partial)
  (:import-from
    :fcl.list
    #:take
    #:drop)
  (:export
    ;; Core
    #:lnil
    #:lcons

    ;; Lazy
    #:delay
    #:force

    ;; Utilitiy
    #:lapply
    #:lenum
    #:ltake
    #:ldrop
    #:subllist
    #:lzip
    #:lgroup
    #:llist-length=

    ;; Foldable
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
    #:lfoldr
    #:lfoldr+
    #:lfoldl
    #:lfoldl+
    #:scanr
    #:scanr+
    #:scanl
    #:scanl+
    #:scant
    #:scant+

    ;; Monad Plus
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
    #:llc

    ;; CL-like Utility
    #:lconsp
    #:lnull
    #:lendp
    #:llist
    #:lcar
    #:lcdr
    #:lfirst
    #:lrest
    #:ladjoin
    #:lnth
    #:lnthcdr
    #:llast
    #:lbutlast
    #:lappend
    #:lreverse
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
    #:lmapc
    #:lmapcar
    #:lmapcan
    #:lmapl
    #:lmaplist
    #:lmapcon
    #:lsearch
    #:lmismatch
    #:lsort
    #:lstable-sort))
(in-package :fcl.datatypes.llist)


;;; Definition
(defdata llist
  (lnil)
  (lcons (:lazy t) (:lazy llist)))


;;; Printer
(defmethod print-object ((object lnil) stream)
  (format stream "#.(LNIL)"))

(defmethod print-object ((object lcons) stream)
  (format stream "#.(LLIST")
  (lmapc (lambda (x) (format stream " ~S" x)) object)
  (format stream ")"))


;;; Foldable
(defmethod cata (x*->x (i llist))
  "X* == (NOTHING) | (JUST (LIST A X))"
  (check-type x*->x function)
  (ematch i
    ((lnil)       (funcall x*->x (nothing)))
    ((lcons a as) (funcall x*->x (just (list a (cata x*->x as)))))))

(defmethod para (i&*x->x (i llist))
  "I&*X == (NOTHING) | (JUST (LIST A I X))"
  (check-type i&*x->x function)
  (ematch i
    ((lnil)       (funcall i&*x->x (nothing)))
    ((lcons a as) (funcall i&*x->x (just (list a i (para i&*x->x as)))))))

(defmethod ana ((class (eql 'llist)) x->x* x)
  "X* == (NOTHING) | (JUST (LIST A X))"
  (check-type x->x* function)
  (ematch (funcall x->x* x)
    ((nothing)         (lnil))
    ((just (list a x)) (lcons a (ana 'llist x->x* x)))))

(defmethod apo ((class (eql 'llist)) x->f+*x x)
  "F+*X == (NOTHING) | (JUST (LIST A (LEFT F))) | (JUST (LIST A (RIGHT X)))"
  (check-type x->f+*x function)
  (ematch (funcall x->f+*x x)
    ((nothing)           (lnil))
    ((just (list a f+x)) (ematch f+x
                           ((left f)  (lcons a f))
                           ((right x) (lcons a (apo 'llist x->f+*x x)))))))

(defmethod foldr (a&x->x x0 (as llist))
  (check-type a&x->x function)
  (do ((as (lreverse as) (lrest as))
       (x x0 (funcall a&x->x (lfirst as) x)))
      ((lendp as) x)))

(defmethod foldr+ (a&as&x->x x0 (as llist))
  (check-type a&as&x->x function)
  (flet ((lrev+ (llst)
           (do ((llst llst (lrest llst))
                (acc '() (cons llst acc)))
               ((lendp llst) acc))))
    (do ((as-s (lrev+ as) (rest as-s))
         (x x0 (ematch as-s
                 ((cons (lcons a as) _)
                  (funcall a&as&x->x a as x)))))
        ((endp as-s) x))))

(defmethod unfoldr ((class (eql 'llist)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (if (funcall x->? x)
      (lnil)
      (lcons (funcall x->a x) (unfoldr 'llist x->? x->a x->x (funcall x->x x)))))

(defmethod unfoldr+ ((class (eql 'llist)) x->? x->a x->x as0 x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (if (funcall x->? x)
      as0
      (lcons (funcall x->a x) (unfoldr 'llist x->? x->a x->x (funcall x->x x)))))

(defmethod foldl (x&a->x x0 (as llist))
  (check-type x&a->x function)
  (do ((as as (lrest as))
       (x x0 (funcall x&a->x x (lfirst as))))
      ((lendp as) x)))

(defmethod foldl+ (x&a&as->x x0 (as llist))
  (check-type x&a&as->x function)
  (do ((as as as2)
       (a (lfirst as) (lfirst as2))
       (as2 (lrest as) (lrest as2))
       (x x0 (funcall x&a&as->x x a as2)))
      ((lendp as) x)))

(defmethod unfoldl ((class (eql 'llist)) x->? x->x x->a x)
  (check-type x->? function)
  (check-type x->x function)
  (check-type x->a function)
  (labels ((rec (x as)
             (declare (optimize (speed 3))
                      (type function x->? x->x x->a))
             (if (funcall x->? x)
                 as
                 (rec (funcall x->x x) (lcons (funcall x->a x) as)))))
    (rec x (lnil))))

(defmethod unfoldl+ ((class (eql 'llist)) x->? x->x x->a as0 x)
  (check-type x->? function)
  (check-type x->x function)
  (check-type x->a function)
  (check-type as0 llist)
  (labels ((rec (x as)
             (declare (optimize (speed 3))
                      (type function x->? x->x x->a))
             (if (funcall x->? x)
                 as
                 (rec (funcall x->x x) (lcons (funcall x->a x) as)))))
    (rec x as0)))

(defmethod lfoldr (a&$x->x x0 (as llist))
  (check-type a&$x->x function)
  (labels ((rec (as)
             (declare (optimize (speed 3))
                      (type function a&$x->x))
             (ematch as
               ((lnil) x0)
               ((lcons a as) (funcall a&$x->x a (delay (rec as)))))))
    (rec as)))

(defmethod lfoldr+ (a&as&$x->x x0 (as llist))
  (check-type a&as&$x->x function)
  (labels ((rec (as)
             (declare (optimize (speed 3))
                      (type function a&as&$x->x))
             (ematch as
               ((lnil) x0)
               ((lcons a as) (funcall a&as&$x->x a as (delay (rec as)))))))
    (rec as)))

(defmethod lfoldl ($x&a->x x0 (as llist))
  (check-type $x&a->x function)
  (labels ((rec (as $x)
             (declare (optimize (speed 3))
                      (type function $x&a->x))
             (ematch as
               ((lnil) (force $x))
               ((lcons a as) (rec as (delay (funcall $x&a->x $x a)))))))
    (rec as (delay x0))))

(defmethod lfoldl+ ($x&a&as->x x0 (as llist))
  (check-type $x&a&as->x function)
  (labels ((rec (as $x)
             (declare (optimize (speed 3))
                      (type function $x&a&as->x))
             (ematch as
               ((lnil) (force $x))
               ((lcons a as) (rec as (delay (funcall $x&a&as->x $x a as)))))))
    (rec as (delay x0))))

;;; Monad Plus
(defmethod fmap (a->b (a* llist))
  (check-type a->b function)
  (foldr (lambda (a b*) (lcons (funcall a->b a) b*))
         (lnil)
         a*))

(defmethod unit ((class (eql 'llist)) a)
  (lcons a (lnil)))

(defmethod amap (a->*b (a* llist))
  (check-type a->*b llist)
  (foldr (lambda (a->b b*)
           (check-type a->b function)
           (foldr (lambda (a b*) (lcons (funcall a->b a) b*)) b* a*))
         (lnil)
         a->*b))

(defmethod mmap (a->b* (a* llist))
  (check-type a->b* function)
  (foldr (lambda (a b*) (lappend (funcall a->b* a) b*))
         (lnil)
         a*))

(defmethod mzero ((class (eql 'llist)))
  (lnil))

(defmethod mplus ((monoid1 llist) monoid2)
  (check-type monoid2 llist)
  (lappend monoid1 monoid2))

(defmacro llc (element &body clauses)
  `(mdo ,@(mapcar (lambda (clause)
                    (if (listp clause)
                        (case (first clause)
                          ((:in :let) clause)
                          (otherwise `(guard 'llist ,clause)))
                        `(guard 'llist ,clause)))
                  clauses)
        (unit 'llist ,element)))


;;; Utility
(defun lconsf (x llist)
  (check-type llist llist)
  (lcons x llist))

(defun lapply (function arg &rest args)
  (check-type function function)
  (labels ((rec (f arg args)
             (declare (optimize (speed 3))
                      (type function f))
             (if (endp args)
                 (funcall (the function (foldl (lambda (f a) (partial f a)) f arg)))
                 (apply #'rec (partial f arg) args))))
    (rec function arg args)))

(defun lenum (start &optional end)
  (check-type start integer)
  (check-type end (or null integer))
  (unfoldr 'llist
           (if end (partial #'= end) (constantly nil))
           #'identity
           #'1+
           start))

(defun ltake (n llist)
  (check-type n index)
  (check-type llist llist)
  (labels ((rec (n llst)
             (ematch (list n llst)
               ((list 0 _)              (lnil))
               ((list _ (lcons x llst)) (lcons x (rec (1- n) llst)))
               ((list _ (lnil))         (lcons (lnil) (rec (1- n) (lnil)))))))
    (rec n llist)))

(defun ldrop (n llist)
  (check-type n index)
  (check-type llist llist)
  (labels ((rec (n llst)
             (declare (optimize (speed 3))
                      (type index n))
             (ematch (list n llst)
               ((list 0 _)              llst)
               ((list _ (lcons _ llst)) (rec (1- n) llst))
               ((list _ (lnil))         (lnil)))))
    (rec n llist)))

(defun subllist (llist start &optional end)
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (if end
      (ltake (- end start) (ldrop start llist))
      (ldrop start llist)))

(defun lzip (&rest llists)
  (mapc (lambda (llst) (check-type llst llist)) llists)
  (labels ((rec (llsts)
             (if (some #'lendp llsts)
                 (lnil)
                 (lcons (lfoldr (lambda (llst $acc) (lcons (lfirst llst) (force $acc)))
                                (lnil)
                                llsts)
                        (rec (mapcar #'lrest llsts))))))
    (rec llists)))

(defun lgroup (n llist)
  (check-type n index)
  (check-type llist llist)
  (lfoldr+ (lambda (ix ixs $acc)
             (ematch ix
               ((lcons i (lcons x (lnil)))
                (if (zerop (mod i n))
                    (lcons (lcons x (fmap (partial #'lnth 1) (ltake (1- n) ixs)))
                           (force $acc))
                    (force $acc)))))
           (lnil)
           (lzip (lenum 0) llist)))

(defun llist-length= (llist1 llist2)
  (check-type llist1 llist)
  (check-type llist2 llist)
  (do ((ll1 llist1 (lrest ll1))
       (ll2 llist2 (lrest ll2)))
      ((or (lendp ll1) (lendp ll2))
       (and (lendp ll1) (lendp ll2)))))


;;; CL-like Utility
(defun lconsp (object)
  (typep object 'lcons))

(defun lnull (object)
  (typep object 'lnil))

(defun lendp (object)
  (check-type object llist)
  (lnull object))

(defmacro llist (&rest args)
  (foldr (lambda (arg acc) `(lcons ,arg ,acc))
         '(lnil)
         args))

(defun lcar (llist)
  (ematch llist
    ((lnil)      nil)
    ((lcons x _) x)))

(defun lcdr (llist)
  (ematch llist
    ((lnil)         (lnil))
    ((lcons _ llst) llst)))

(defun lfirst (llist)
  (declare (inline))
  (lcar llist))

(defun lrest (llist)
  (declare (inline))
  (lcdr llist))

(defun ladjoin (item llist &key key (test #'eql))
  (check-type llist llist)
  (let ((item (if key item (funcall key item)))
        (pred (if key
                  (lambda (x) (funcall test item (funcall key x)))
                  (lambda (x) (funcall test item x)))))
    (labels ((rec (llst)
               (declare (optimize (speed 3)))
               (ematch llst
                 ((lnil)         (lcons item llist))
                 ((lcons x llst) (if (funcall pred x)
                                     llist
                                     (rec llst))))))
      (rec llist))))


(defun lnth (n llist)
  (check-type n index)
  (check-type llist llist)
  (labels ((rec (n llst)
             (declare (optimize (speed 3))
                      (type index n))
             (ematch (list n llst)
               ((list 0 (lcons x _))    x)
               ((list _ (lcons _ llst)) (rec (1- n) llst))
               ((list _ (lnil))         (lnil)))))
    (rec n llist)))

(defun lnthcdr (n llist)
  (check-type n index)
  (check-type llist llist)
  (labels ((rec (n llst)
             (declare (optimize (speed 3))
                      (type index n))
             (ematch (list n llst)
               ((list 0 _)              llst)
               ((list _ (lcons _ llst)) (rec (1- n) llst))
               ((list _ (lnil)          (lnil))))))
    (rec n llist)))

(defun llast (llist &optional (n 1))
  (check-type llist llist)
  (check-type n index)
  (if (zerop n)
      (lnil)
      (foldl (lambda (acc x) (lcons x acc))
             (lnil)
             (take n (foldl (lambda (acc x) (cons x acc))
                            '()
                            llist)))))

(defun lbutlast (llist &optional (n 1))
  (check-type llist llist)
  (check-type n index)
  (if (zerop n)
      llist
      (foldl (lambda (acc x) (lcons x acc))
             (lnil)
             (drop n (foldl (lambda (acc x) (cons x acc))
                            '()
                            llist)))))

(defun lreverse (llist)
  (check-type llist llist)
  (foldl (lambda (acc x) (lcons x acc)) (lnil) llist))

(defun lappend (&rest llists)
  (flet ((lapp2 (llst1 llst2)
           (lfoldr (lambda (x $acc) (lcons x (force $acc)))
                   llst2
                   llst1)))
    (foldr #'lapp2 (first (last llists)) (butlast llists))))

(defun lrevappend (x y)
  (check-type x llist)
  (check-type y llist)
  (foldl (lambda (acc x) (lcons x acc))
         y
         x))

(defun llength (llist)
  (foldl (lambda (acc _) (declare (ignore _)) (1+ acc)) 0 llist))

(defun lcount (item llist &key from-end (start 0) end key (test #'eql))
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type key (or null function))
  (check-type test function)
  (lcount-if (lambda (x) (funcall test x item)) llist
             :from-end from-end
             :start start
             :end end
             :key key))

(defun lcount-if (predicate llist &key from-end (start 0) end key)
  (check-type predicate function)
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type key (or null function))
  (let ((predicate (if key (compose predicate key) predicate)))
    (flet ((func (ix $acc)
             (ematch ix
               ((lcons i (lcons x (lnil)))
                (cond ((< i start)           (force $acc))
                      ((and end (>= i end))  0)
                      ((funcall predicate x) (1+ (force $acc)))
                      (t                     (force $acc)))))))
      (if from-end
          (lfoldl (lambda ($acc ix) (func ix $acc)) 0 (lzip (lenum 0) llist))
          (lfoldr #'func 0 (lzip (lenum 0) llist))))))

(defun lcount-if-not (predicate llist &key from-end (start 0) end key)
  (check-type predicate function)
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type key (or null function))
  (lcount-if (complement predicate) llist
             :from-end from-end
             :start start
             :end end
             :key key))

(defun lremove (item llist &key from-end (test #'eql) (start 0) end count key)
  (check-type llist llist)
  (check-type test function)
  (check-type start index)
  (check-type end (or null index))
  (check-type count (or null index))
  (check-type key (or null function))
  (lremove-if (lambda (x) (funcall test x item)) llist
              :from-end from-end
              :start start
              :end end
              :count count
              :key key))

(defun lremove-if (predicate llist &key from-end (start 0) end count key)
  (check-type predicate function)
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type count (or null index))
  (check-type key (or null function))
  (let ((predicate (if key (compose predicate key) predicate)))
    (flet ((func (ix ixs $acc)
             (ematch ix
               ((lcons i (lcons x (lnil)))
                (cond ((< i start)               (lcons x (force $acc)))
                      ((and end (>= i end))      (lcons x (fmap (partial #'lnth 1) ixs)))
                      ((and count (zerop count)) (lcons x (fmap (partial #'lnth 1) ixs)))
                      ((funcall predicate x)     (when count (decf count))
                                                 (force $acc))
                      (t                         (lcons x (force $acc))))))))
      (if from-end
          (lreverse (lfoldr+ #'func (lnil) (lreverse (lzip (lenum 0) llist))))
          (lfoldr+ #'func (lnil) (lzip (lenum 0) llist))))))

(defun lremove-if-not (predicate llist &key from-end (start 0) end count key)
  (check-type predicate function)
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type count (or null index))
  (check-type key (or null function))
  (lremove-if (complement predicate) llist
              :from-end from-end
              :start start
              :end end
              :count count
              :key key))

(defun lsubstitute (new old llist &key from-end (test #'eql) (start 0) end count key)
  (check-type llist llist)
  (check-type test function)
  (check-type start index)
  (check-type end (or null index))
  (check-type count (or null index))
  (check-type key (or null function))
  (lsubstitute-if new (lambda (x) (funcall test x old)) llist
                  :from-end from-end
                  :start start
                  :end end
                  :count count
                  :key key))

(defun lsubstitute-if (new predicate llist &key from-end (start 0) end count key)
  (check-type predicate function)
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type count (or null index))
  (check-type key (or null function))
  (let ((predicate (if key (compose predicate key) predicate)))
    (flet ((func (ix ixs $acc)
             (ematch ix
               ((lcons i (lcons x (lnil)))
                (cond ((< i start)               (lcons x (force $acc)))
                      ((and end (>= i end))      (lcons x (fmap (partial #'lnth 1) ixs)))
                      ((and count (zerop count)) (lcons x (fmap (partial #'lnth 1) ixs)))
                      ((funcall predicate x)     (when count (decf count))
                                                 (lcons new (force $acc)))
                      (t                         (lcons x (force $acc))))))))
      (if from-end
          (lreverse (lfoldr+ #'func (lnil) (lreverse (lzip (lenum 0) llist))))
          (lfoldr+ #'func (lnil) (lzip (lenum 0) llist))))))

(defun lsubstitute-if-not (new predicate llist &key from-end (start 0) end count key)
  (check-type predicate function)
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type count (or null index))
  (check-type key (or null function))
  (lsubstitute-if new (complement predicate) llist
                  :from-end from-end
                  :start start
                  :end end
                  :count count
                  :key key))

(defun lfind (item llist &key from-end (start 0) end key (test #'eql))
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type key (or null function))
  (check-type test function)
  (lfind-if (lambda (x) (funcall test x item)) llist
            :from-end from-end
            :start start
            :end end
            :key key))

(defun lfind-if (predicate llist &key from-end (start 0) end key)
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type key (or null function))
  (let ((predicate (if key (compose predicate) predicate)))
    (flet ((func (ix $acc)
             (ematch ix
               ((lcons i (lcons x (lnil)))
                (cond ((< i start)           (force $acc))
                      ((and end (>= i end))  nil)
                      ((funcall predicate x) x)
                      (t                     (force $acc)))))))
      (if from-end
          (lfoldl (lambda ($acc x) (func x $acc)) nil llist)
          (lfoldr #'func nil llist)))))

(defun lfind-if-not (predicate llist &key from-end (start 0) end key)
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type key (or null function))
  (lfind-if (complement predicate) llist
            :from-end from-end
            :start start
            :end end
            :key key))

(defun lmember (item llist &key key (test #'eql))
  (check-type llist llist)
  (check-type key (or null function))
  (check-type test function)
  (lmember-if (lambda (x) (funcall test x item)) llist
              :key key))

(defun lmember-if (predicate llist &key key)
  (check-type predicate function)
  (check-type llist llist)
  (check-type key (or null function))
  (let ((predicate (if key (compose predicate key) predicate)))
    (foldr+ (lambda (x xs acc)
              (if (funcall predicate x)
                  (lcons x xs)
                  acc))
            nil
            llist)))

(defun lmember-if-not (predicate llist &key key)
  (check-type predicate function)
  (check-type llist llist)
  (check-type key (or null function))
  (lmember-if (complement predicate) llist
              :key key))

(defun lposition (item llist &key from-end (start 0) end key (test #'eql))
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type key (or null function))
  (check-type test function)
  (lposition-if (lambda (x) (funcall test x item)) llist
                :from-end from-end
                :start start
                :end end
                :key key))

(defun lposition-if (predicate llist &key from-end (start 0) end key)
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type key (or null function))
  (let ((predicate (if key (compose predicate key) predicate)))
    (flet ((func (ix $acc)
             (ematch ix
               ((lcons i (lcons x (lnil)))
                (cond ((< i start)           (force $acc))
                      ((and end (>= i end))  nil)
                      ((funcall predicate x) i)
                      (t                     (force $acc)))))))
      (if from-end
          (lfoldl (lambda ($acc x) (func x $acc)) nil llist)
          (lfoldr #'func nil llist)))))

(defun lposition-if-not (predicate llist &key from-end (start 0) end key)
  (check-type llist llist)
  (check-type start index)
  (check-type end (or null index))
  (check-type key (or null function))
  (lposition-if (complement predicate) llist
                :from-end from-end
                :start start
                :end end
                :key key))

(defun lmapc (function llist &rest more-llists)
  (check-type function function)
  (check-type llist llist)
  (mapc (lambda (llst) (check-type llst llist)) more-llists)
  (labels ((rec (llsts)
             (declare (optimize (speed 3)))
             (unless (some #'lendp llsts)
               (apply function (mapcar #'lfirst llsts))
               (rec (mapcar #'lrest llsts)))))
    (rec (cons llist more-llists))))

(defun lmapcar (function llist &rest more-llists)
  (check-type function function)
  (check-type llist llist)
  (mapc (lambda (llst) (check-type llst llist)) more-llists)
  (fmap (partial #'lapply function)
        (apply #'lzip llist more-llists)))

(defun lmapcan (function llist &rest more-llists)
  (check-type function function)
  (check-type llist llist)
  (mapc (lambda (llst) (check-type llst llist)) more-llists)
  (mmap (partial #'lapply function) (apply #'lzip llist more-llists)))

(defun lmapl (function llist &rest more-llists)
  (check-type function function)
  (check-type llist llist)
  (mapc (lambda (llst) (check-type llst llist)) more-llists)
  (labels ((rec (llsts)
             (declare (optimize (speed 3)))
             (unless (some #'lendp llsts)
               (lapply function llsts)
               (rec (mapcar #'lrest llsts)))))
    (rec (cons llist more-llists))))

(defun lmaplist (function llist &rest more-llists)
  (check-type function function)
  (check-type llist llist)
  (mapc (lambda (llst) (check-type llst llist)) more-llists)
  (labels ((rec (llsts)
             (declare (optimize (speed 3)))
             (if (some #'lendp llsts)
                 (lnil)
                 (lcons (lapply function llsts)
                        (rec (mapcar #'lrest llsts))))))
    (rec (cons llist more-llists))))

(defun lmapcon (function llist &rest more-llists)
  (check-type function function)
  (check-type llist llist)
  (mapc (lambda (llst) (check-type llst llist)) more-llists)
  (labels ((rec (llsts)
             (declare (optimize (speed 3)))
             (if (some #'lendp llsts)
                 (lnil)
                 (lappend (lapply function llsts)
                          (rec (mapcar #'lrest llsts))))))
    (rec (cons llist more-llists))))

(defun lsearch (sub-llist1 main-llist2
                &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 key)
  (check-type sub-llist1 llist)
  (check-type main-llist2 llist)
  (check-type test function)
  (check-type start1 index)
  (check-type end1 (or null index))
  (check-type start2 index)
  (check-type end2 (or null index))
  (check-type key (or null function))
  (let ((test (if key
                  (lambda (x y) (funcall test (funcall key x) (funcall key y)))
                  test))
        (sub-x1 (lnth start1 sub-llist1))
        (sub-rest1 (subllist sub-llist1 (1+ start1) end1)))
    (flet ((func (ix ixs $acc1)
             (ematch ix
               ((lcons i (lcons x (lnil)))
                (cond ((< i start2)
                       (force $acc1))
                      ((and end2 (>= i end2))
                       nil)
                      ((funcall test sub-x1 x)
                       (lfoldr (lambda (xy $acc2)
                                 (ematch xy
                                   ((lcons x (lcons y (lnil)))
                                    (if (funcall test x y)
                                        (force $acc2)
                                        (force $acc1)))))
                               i
                               (lzip sub-rest1
                                     (lappend (fmap (partial #'lnth 1) ixs)
                                              (unfoldr 'llist
                                                       (constantly nil)
                                                       #'identity
                                                       #'identity
                                                       (gensym))))))
                      (t
                       (force $acc1)))))))
      (if from-end
          (lfoldl+ (lambda ($acc ix ixs) (func ix ixs $acc)) nil (lzip (lenum 0) main-llist2))
          (lfoldr+ #'func nil (lzip (lenum 0) main-llist2))))))

(defun lmismatch (llist1 llist2 &key from-end (test #'eql) (start1 0) end1 (start2 0) end2 key)
  (check-type llist1 llist)
  (check-type llist2 llist)
  (check-type test function)
  (check-type start1 index)
  (check-type end1 (or null index))
  (check-type start2 index)
  (check-type end2 (or null index))
  (check-type key (or null function))
  (let ((llist1 (subllist llist1 start1 end1))
        (llist2 (subllist llist2 start2 end2))
        (test (if key
                  (lambda (x y) (funcall test (funcall key x) (funcall key y)))
                  test)))
    (flet ((func (ixy $acc)
             (ematch ixy
               ((lcons i (lcons x (lcons y (lnil))))
                (if (funcall test x y)
                    i
                    (force $acc))))))
      (cond ((not (llist-length= llist1 llist2))
             (llength llist1))
            (from-end
             (lfoldl (lambda ($acc ixy) (func ixy $acc))
                     nil
                     (lzip (lenum start1 end1) llist1 llist2)))
            (t
            (lfoldr #'func
                    nil
                    (lzip (lenum start1 end1) llist1 llist2)))))))

(defun lsort (llist predicate &key key)
  (check-type llist llist)
  (check-type predicate function)
  (lfoldr (lambda (x $acc) (lcons x (force $acc)))
          (lnil)
          (sort (foldr #'cons '() llist) predicate :key key)))

(defun lstable-sort (llist predicate &key key)
  (check-type llist llist)
  (check-type predicate function)
  (lfoldr (lambda (x $acc) (lcons x (force $acc)))
          (lnil)
          (stable-sort (foldr #'cons '() llist) predicate :key key)))
