(defpackage fcl/tests.applicative
  (:nicknames :fcl/tests.generics.applicative :fcl/t.ap)
  (:use :common-lisp :fiveam :fcl.applicative :fcl/tests.util)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:compose #:curry #:symbolicate)
  (:export #:applicative-test))
(in-package :fcl/tests.applicative)


(defmacro applicative-test (name class gen-a gen-a* gen-a->b gen-b->c)
  `(progn
     (test ,(symbolicate 'identity-of-applicative/ name)
       "Identity of Applicative"
       (for-all ((a* ,gen-a*))
         (is (data= (amap (unit ',class #'identity) a*) a*))))
     (test ,(symbolicate 'composition-of-applicative/ name)
       "Composition of Applicative"
       (for-all ((a* ,gen-a*)
                 (a->b ,gen-a->b)
                 (b->c ,gen-b->c))
         (let ((a->*b (unit ',class a->b))
               (b->*c (unit ',class b->c)))
           (is (data= (amap (amap (amap (unit ',class (curry #'compose)) b->*c) a->*b) a*)
                      (amap b->*c (amap a->*b a*)))))))
     (test ,(symbolicate 'homomorphism-of-applicative/ name)
       "Homomorphism of Aplicative"
       (for-all ((a ,gen-a)
                 (a->b ,gen-a->b))
         (is (data= (amap (unit ',class a->b) (unit ',class a))
                    (unit ',class (funcall a->b a))))))
     (test ,(symbolicate 'interchange-of-applicative/ name)
       (for-all ((a ,gen-a)
                 (a->b ,gen-a->b))
         (is (data= (amap (unit ',class a->b) (unit ',class a))
                    (amap (unit ',class (lambda (a->b) (funcall a->b a))) (unit ',class a->b))))))))
