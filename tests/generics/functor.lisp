(defpackage fcl/tests.functor
  (:nicknames :fcl/tests.generics.functor :fcl/t.ft)
  (:use :common-lisp :rove :fcl.functor)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:compose)
  (:export #:identity-test #:composition-test))
(in-package :fcl/tests.functor)


(defmacro identity-test (a*)
  `(progn
     (ok (data= (fmap #'identity ,a*) ,a*))
     nil))

(defmacro composition-test (b->c a->b a*)
  `(progn
     (ok (data= (fmap (compose ,b->c ,a->b) ,a*)
                (fmap ,b->c (fmap ,a->b ,a*))))
     nil))
