(defpackage fcl.monad
  (:use :common-lisp)
  (:import-from
    :fcl
    #:mmap
    #:monad-do
    #:mprogn
    #:mlet
    #:define-fmap-by-monad
    #:define-amap-by-monad)
  (:import-from
    :fcl.applicative
    #:unit
    #:fmap
    #:amap)
  (:import-from
    :fcl.util
    #:nlist?)
  (:export
    #:unit
    #:fmap
    #:amap
    #:mmap
    #:monad-do
    #:mprogn
    #:mlet
    #:define-fmap-by-monad
    #:define-amap-by-monad))

(in-package :fcl.monad)


(defgeneric mmap (a->b* a*)
  (:documentation
"Returns a value of class of B*, \"appended\" (FMAP A->B* A)
where A is a value included A*.
MMAP must satisfy the rules:
  Left identity:  (mmap #'a->b* (unit class a))
               == (a->b* a)
  Right identity: (mmap (partial #'unit class) a*)
               == a*
  Associativity:  (mmap (lambda (a) (mmap #'b->c* (a->b* a))) a*)
               == (mmap #'b->c* (mmap #'a->b* a*))"))

(defmacro monad-do (&body clauses)
  (reduce (lambda (clause body)
            (if (listp clause)
                (case (first clause)
                  (:in
                    (assert (nlist? 3 clause) (clause))
                    `(mlet ((,(second clause) ,(third clause))) ,body))
                  (:let
                    (assert (nlist? 3 clause) (clause))
                    `(let ((,(second clause) ,(third clause))) ,body))
                  (otherwise
                    `(mprogn ,clause ,body)))
                `(mprogn ,clause ,body)))
          clauses
          :from-end t))

(defmacro mprogn (&rest monads)
  (let ((g!_ (gensym "G!_")))
    (reduce (lambda (m body)
              `(mmap (lambda (,g!_)
                       (declare (ignore ,g!_))
                       ,body)
                     ,m))
            monads
            :from-end t)))

(defmacro mlet ((&rest bindings) &body body)
  (every (lambda (binding)
           (check-type binding list)
           (assert (nlist? 2 binding) (binding)))
         bindings)
  (reduce (lambda (binding monad)
            (destructuring-bind (v m) binding
              `(mmap (lambda (,v) ,monad) ,m)))
          (if (null bindings)
              bindings
              (butlast bindings))
          :initial-value (if (null bindings)
                             `(let () ,@body)
                             (let ((binding (first (last bindings))))
                               (destructuring-bind (v m) binding
                                 `(mmap (lambda (,v) ,@body) ,m))))
          :from-end t))

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
