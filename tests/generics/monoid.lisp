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
  `(ok (data= (mplus (mzero ,class) ,a*) ,a*)))

(defmacro right-identity-test (class a*)
  `(ok (data= (mplus ,a* (mzero ,class)) ,a*)))

(defmacro associativity-test (a* b* c*)
  `(ok (data= (mplus (mplus ,a* ,b*) ,c*)
              (mplus ,a* (mplus ,b* ,c*)))))
