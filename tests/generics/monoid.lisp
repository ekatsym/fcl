(defpackage fcl/tests.monoid
  (:nicknames :fcl/tests.generics.monoid :fcl/t.mo)
  (:use :common-lisp :fiveam :fcl.monoid)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:partial)
  (:export #:left-identity-test
           #:right-identity-test
           #:associativity-test))
(in-package :fcl/tests.monoid)


(defmacro left-identity-test (class a*)
  `(is (data= (mplus (mzero ,class) ,a*) ,a*)))

(defmacro right-identity-test (class a*)
  `(is (data= (mplus ,a* (mzero ,class)) ,a*)))

(defmacro associativity-test (a* b* c*)
  `(is (data= (mplus (mplus ,a* ,b*) ,c*)
              (mplus ,a* (mplus ,b* ,c*)))))
