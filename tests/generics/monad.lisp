(defpackage fcl/tests.monad
  (:nicknames :fcl/tests.generics.monad :fcl/t.ma)
  (:use :common-lisp :fiveam :fcl.monad :fcl/tests.util)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:partial #:symbolicate)
  (:export #:monad-test))
(in-package :fcl/tests.monad)


(defmacro monad-test (name class gen-a gen-a* gen-a->b* gen-b->c*)
  `(progn
     (test ,(symbolicate 'left-identity-of-monad/ name)
       "Left Identity of Monad"
       (for-all ((a ,gen-a)
                 (a->b* ,gen-a->b*))
         (is (data= (mmap a->b* (unit ',class a))
                    (funcall a->b* a)))))
     (test ,(symbolicate 'right-identity-of-monad/ name)
       "Right Identity of Monad"
       (for-all ((a* ,gen-a*))
         (is (data= (mmap (partial #'unit ',class) a*) a*))))
     (test ,(symbolicate 'associativity-of-monad/ name)
       "Associativity of Monad"
       (for-all ((a* ,gen-a*)
                 (a->b* ,gen-a->b*)
                 (b->c* ,gen-b->c*))
         (is (data= (mmap (lambda (a) (mmap b->c* (funcall a->b* a))) a*)
                    (mmap b->c* (mmap a->b* a*))))))))
