(defpackage fcl.monad
  (:nicknames :fcl.generics.monad :fcl.ma)
  (:use :common-lisp :fcl.applicative)
  (:import-from
    :fcl.util
    #:nlist?)
  (:export
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:define-fmap-by-applicative
    #:define-fmap-by-monad
    #:define-amap-by-monad))
(in-package :fcl.monad)


;;; Core
(defgeneric mmap (a->b* a*)
  (:documentation
"Returns a value of class B* \"appended\" (FMAP A->B* A)
where A is a value included A*.
MMAP must satisfy the rules:
  Left Identity:  (mmap #'a->b* (unit class a))
               == (a->b* a)
  Right Identity: (mmap (partial #'unit class) a*)
               == a*
  Associativity:  (mmap (lambda (a) (mmap #'b->c* (a->b* a))) a*)
               == (mmap #'b->c* (mmap #'a->b* a*))"))

(defmacro mlet ((&rest bindings) &body body)
  (every (lambda (binding) (assert (nlist? 2 binding) (binding))) bindings)
  (reduce (lambda (binding body)
            (destructuring-bind (var monad) binding
              (if (string= "_" var)
                  `(mmap (lambda (,var) (declare (ignore ,var)) ,body) ,monad)
                  `(mmap (lambda (,var) ,body) ,monad))))
          bindings
          :initial-value `(progn ,@body)
          :from-end t))

(defmacro mprogn (&body forms)
  (reduce (lambda (form body)
            `(mlet ((_ ,form)) ,body))
          forms
          :from-end t))

(defmacro mdo (&body clauses)
  (reduce (lambda (clause body)
            (if (consp clause)
                (case (first clause)
                  (:in
                    (assert (nlist? 3 clause) (clause))
                    `(mlet ((,(second clause) ,@(nthcdr 2 clause))) ,body))
                  (:let
                    (assert (nlist? 3 clause) (clause))
                    `(let ((,(second clause) ,@(nthcdr 2 clause))) ,body))
                  (otherwise
                    `(mprogn ,clause ,body)))
                `(mprogn ,clause ,body)))
          clauses
          :from-end t))


;;; Shorthand for Functor or Applicative
(defmacro define-fmap-by-monad (class)
  `(defmethod fmap (a->b (a* ,class))
     (check-type a->b function)
     (mlet ((a a*))
       (unit ',class (funcall a->b a)))))

(defmacro define-amap-by-monad (class)
  `(defmethod amap (a->*b (a* ,class))
     (check-type a->*b ,class)
     (mlet ((a->b a->*b)
            (a a*))
       (check-type a->b function)
       (unit ',class (funcall a->b a)))))
