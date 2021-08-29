(defpackage fcl/tests.monad
  (:nicknames :fcl/tests.generics.monad :fcl/t.ma)
  (:use :common-lisp :rove :fcl.monad)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:partial)
  (:export #:left-identity-test
           #:right-identity-test
           #:associativity-test))
(in-package :fcl/tests.monad)


(defmacro left-identity-test (class a->b* a)
  `(ok (data= (mmap ,a->b* (unit ,class ,a)) (funcall ,a->b* ,a))))

(defmacro right-identity-test (class a*)
  `(ok (data= (mmap (partial #'unit ,class) ,a*) ,a*)))

(defmacro associativity-test (a->b* b->c* a*)
  (let ((g!a (gensym "A")))
    `(ok (data= (mmap (lambda (,g!a) (mmap ,b->c* (funcall ,a->b* ,g!a))) ,a*)
                (mmap ,b->c* (mmap ,a->b* ,a*))))))
