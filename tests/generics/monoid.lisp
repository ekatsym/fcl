(defpackage fcl/tests.monoid
  (:nicknames :fcl/tests.generics.monoid :fcl/t.mo)
  (:use :common-lisp :fiveam :fcl.monoid)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:partial #:symbolicate)
  (:export #:monoid-test))
(in-package :fcl/tests.monoid)


(defmacro monoid-test (name class gen-a*)
  `(progn
     (test ,(symbolicate 'left-identity-of-monoid/ name)
       "Left Identity of Monoid"
       (for-all ((a* ,gen-a*))
         (is (data= (mplus (mzero ',class) a*) a*))))
     (test ,(symbolicate 'right-identity-of-monoid/ name)
       "Right Identity of Monoid"
       (for-all ((a* ,gen-a*))
         (is (data= (mplus a* (mzero ',class)) a*))))
     (test ,(symbolicate 'associativity-of-monoid/ name)
       (for-all ((a* ,gen-a*)
                 (b* ,gen-a*)
                 (c* ,gen-a*))
         (is (data= (mplus a* (mplus b* c*))
                    (mplus (mplus a* b*) c*)))))))
