(defpackage fcl/tests.functor
  (:nicknames :fcl/tests.generics.functor :fcl/t.ft)
  (:use :common-lisp :fiveam :fcl.functor)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:compose)
  (:export #:identity-test #:composition-test))
(in-package :fcl/tests.functor)


(defmacro identity-test (a*)
  `(is (data= (fmap #'identity ,a*) ,a*)))

(defmacro composition-test (b->c a->b a*)
  `(is (data= (fmap (compose ,b->c ,a->b) ,a*)
              (fmap ,b->c (fmap ,a->b ,a*)))))
