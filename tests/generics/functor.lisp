(defpackage fcl/tests.functor
  (:nicknames :fcl/tests.generics.functor :fcl/t.ft)
  (:use :common-lisp :fiveam :fcl.functor :fcl/tests.util)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:compose #:symbolicate)
  (:export #:functor-test))
(in-package :fcl/tests.functor)


(defmacro functor-test (name gen-a* gen-a->b gen-b->c)
  `(progn
     (test ,(symbolicate 'identity-of-functor/ name)
       "Identity of Functor"
       (for-all ((a* ,gen-a*))
         (is (data= (fmap #'identity a*) a*))))
     (test ,(symbolicate 'composition-of-functor/ name)
       "Composition of Functor"
       (for-all ((a* ,gen-a*)
                 (a->b ,gen-a->b)
                 (b->c ,gen-b->c))
         (is (data= (fmap (compose b->c a->b) a*)
                    (fmap b->c (fmap a->b a*))))))))
