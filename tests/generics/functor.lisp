(defpackage fcl/tests.functor
  (:nicknames :fcl/tests.generics.functor :fcl/t.ft)
  (:use :common-lisp :rove :fcl/tests.util :fcl.functor)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:compose)
  (:export #:functor-testing))
(in-package :fcl/tests.functor)


(defmacro identity-test (a*)
  `(ok (data= (fmap #'identity ,a*) ,a*)))

(defmacro composition-test (b->c a->b a*)
  `(ok (data= (fmap (compose ,b->c ,a->b) ,a*)
              (fmap ,b->c (fmap ,a->b ,a*)))))

