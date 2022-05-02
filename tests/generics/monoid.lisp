(defpackage fcl/tests.monoid
  (:nicknames :fcl/tests.generics.monoid :fcl/t.mo)
  (:use :common-lisp :rove :fcl.monoid)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:partial)
  (:export #:left-identity-test
           #:right-identity-test
           #:associativity-test))
(in-package :fcl/tests.monoid)


(defmacro left-identity-test (class a*)
  `(progn
     (ok (data= (mplus (mzero ,class) ,a*) ,a*))
     nil))

(defmacro right-identity-test (class a*)
  `(progn
     (ok (data= (mplus ,a* (mzero ,class)) ,a*))
     nil))

(defmacro associativity-test (a* b* c*)
  `(progn
     (ok (data= (mplus (mplus ,a* ,b*) ,c*)
                (mplus ,a* (mplus ,b* ,c*))))
     nil))
