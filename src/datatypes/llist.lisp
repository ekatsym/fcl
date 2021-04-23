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
    #:compose)
  (:import-from
    :fcl.list
    #:take
    #:drop)
  (:export
    ;; Core
    #:lnil
    #:lcons

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
    #:lsort

    ;; Lazy
    #:delay
    #:force

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
    #:foldt
    #:foldt+
    #:unfoldt
    #:unfoldt+
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
    #:llc))
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
  (ematch as
    ((lnil)       x0)
    ((lcons a as) (funcall a&x->x a (foldr a&x->x x0 as)))))

(defmethod foldr+ (as&x->x x0 (as llist))
  (check-type as&x->x function)
  (ematch as
    ((lnil) x0)
    ((lcons _ as2) (funcall as&x->x as (foldr+ as&x->x x0 as2)))))

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
  (labels ((rec (as x)
             (declare (optimize (speed 3)))
             (ematch as
               ((lnil)       x)
               ((lcons a as) (rec as (funcall x&a->x x a))))))
    (rec as x0)))

(defmethod foldl+ (x&as->x x0 (as llist))
  (check-type x&as->x function)
  (labels ((rec (as x)
             (declare (optimize (speed 3)))
             (ematch as
               ((lnil) x)
               ((lcons _ as2) (rec as2 (funcall x&as->x x as))))))
    (rec as x0)))

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

(defmethod foldt (a&xs->x x0 (at llist))
  (check-type a&xs->x function)
  (labels ((rec (at)
             (ematch at
               ((lnil)        x0)
               ((lcons a ats) (funcall a&xs->x a (mapcar #'rec ats))))))
    (rec at))) 
(defmethod foldt+ (at&xs->x x0 (at llist))
  (check-type at&xs->x function)
  (labels ((rec (at)
             (ematch at
               ((lnil)         x0)
               ((lcons _ ats) (funcall at&xs->x at (mapcar #'rec ats))))))
    (rec at)))

(defmethod unfoldt ((class (eql 'llist)) x->? x->a x->xs x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->xs function)
  (labels ((rec (x)
             (if (funcall x->? x)
                 (lnil)
                 (lcons (funcall x->a x) (funcall x->xs x)))))
    (rec x)))

(defmethod unfoldt+ ((class (eql 'llist)) x->? x->a x->xs at0 x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->xs function)
  (check-type at0 llist)
  (labels ((rec (x)
             (if (funcall x->? x)
                 at0
                 (lcons (funcall x->a x) (funcall x->xs x)))))
    (rec x)))


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


;;; CL-like Utility
(defun lconsp (object)
  (typep object 'lcons))

(defun lnull (object)
  (typep object 'lnil))

(defun lendp (object)
  (check-type object llist)
  (lnull object))

(defun llist (&rest args)
  (if (null args)
      (lnil)
      (lcons (first args) (apply #'llist (rest args)))))

(defun lcar (llist)
  (ematch llist
    ((lnil)         (lnil))
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
  (labels ((lapp2 (llst1 llst2)
             (ematch llst1
               ((lnil)          llst2)
               ((lcons x rest1) (lcons x (lapp2 rest1 llst2))))))
    (foldr #'lapp2 (lnil) llists)))

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
  (labels ((rec (i llst acc)
             (declare (optimize (speed 3))
                      (type (or null index) end)
                      (type index i)
                      (type integer acc)
                      #+sbcl(sb-ext:muffle-conditions sb-int:simple-compiler-note))
             (ematch llst
               ((lnil)
                (assert (or (null end) (= i end)) (start end))
                acc)
               ((lcons x llst)
                (cond ((< i start)           (rec (1+ i) llst acc))
                      ((and end (>= i end))  acc)
                      ((funcall predicate x) (rec (1+ i) llst (1+ acc)))
                      (t                     (rec (1+ i) llst acc)))))))
    (if (or from-end key)
        (lcount-if
          (if key (compose predicate key) predicate)
          (if from-end (subllist llist start end) llist)
          :start (if from-end 0 start)
          :end (if from-end nil end))
        (rec 0 llist 0))))

(defun lremove (item llist &key from-end (test #'eql) (start 0) end count key)
  (check-type llist llist)
  (check-type test function)
  (check-type start index)
  (check-type end (or null index))
  (check-type count (or null index))
  (check-type key (or null function))
  (lremove-if (lambda (x) (funcall test x item)) llist
              :from-end from-end
              :start 0
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
  (labels ((rec (i c llst)
             (declare (optimize (speed 3))
                      (type function predicate)
                      (type index start i)
                      (type (or null index) end)
                      (type fixnum c))
             (ematch llst
               ((lnil)
                (assert (or (null end) (= i end)) (start end))
                (lnil))
               ((lcons x xs)
                (cond ((< i start)           (rec (1+ i) c xs))
                      ((and end (>= i end))  llst)
                      ((zerop c)             llst)
                      ((funcall predicate x) (rec (1+ i) (1- c) xs))
                      (t                     (lcons x (rec (1+ i) c xs))))))))
    (cond (key      (lremove-if (compose predicate key) llist
                                :from-end from-end
                                :start start
                                :end end
                                :count count))
          (from-end (lappend (subllist llist 0 start)
                             (lrevappend (lremove-if predicate
                                                     (lreverse (subllist llist start end))
                                                     :count count)
                                         (if end
                                             (ldrop end llist)
                                             (lnil)))))
          (t        (rec 0 (or count -1) llist)))))

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
  (labels ((rec (i c llst)
             (declare (optimize (speed 3))
                      (type function predicate)
                      (type index start i)
                      (type (or null index) end)
                      (type fixnum c))
             (ematch llst
               ((lnil)
                (lnil))
               ((lcons x xs)
                (cond ((< i start)           (rec (1+ i) c xs))
                      ((and end (>= i end))  llst)
                      ((zerop c)             llst)
                      ((funcall predicate x) (lcons new (rec (1+ i) (1- c) xs)))
                      (t                     (lcons x (rec (1+ i) c xs))))))))
    (cond (key      (lsubstitute-if new (compose predicate key) llist
                                    :from-end from-end
                                    :start start
                                    :end end
                                    :count count))
          (from-end (lappend (subllist llist 0 start)
                             (lrevappend (lsubstitute-if new
                                                         predicate
                                                         (lreverse (subllist llist start end))
                                                         :count count)
                                         (if end
                                             (ldrop end llist)
                                             (lnil)))))
          (t        (rec 0 (or count -1) llist)))))

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
  (labels ((rec (i llst)
             (declare (optimize (speed 3))
                      (type function predicate)
                      (type index start i)
                      (type (or null index) end))
             (ematch llst
               ((lnil)
                nil)
               ((lcons x xs)
                (cond ((< i start)           (rec (1+ i) xs))
                      ((and end (>= i end))  nil)
                      ((funcall predicate x) x)
                      (t                     (rec (1+ i) xs)))))))
    (cond (key      (lfind-if (compose predicate key) llist
                              :from-end from-end
                              :start start
                              :end end))
          (from-end (lfind-if predicate (lreverse (subllist llist start end))))
          (t        (rec 0 llist)))))

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
  (labels ((rec (i llst)
             (declare (optimize (speed 3))
                      (type function predicate)
                      (type index start i)
                      (type (or null index) end))
             (ematch llst
               ((lnil)
                nil)
               ((lcons x xs)
                (cond ((< i start)           (rec (1+ i) xs))
                      ((and end (>= i end))  nil)
                      ((funcall predicate x) i)
                      (t                     (rec (1+ i) xs)))))))
    (cond (key      (lposition-if (compose predicate key) llist
                                  :from-end from-end
                                  :start start
                                  :end end))
          (from-end (let ((revpos (lposition-if predicate (lreverse (subllist llist start end)))))
                      (if revpos
                          (- (1- (or end (llength llist))) revpos)
                          nil)))
          (t        (rec 0 llist)))))


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
