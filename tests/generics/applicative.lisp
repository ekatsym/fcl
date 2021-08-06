(defpackage fcl/tests.applicative
  (:nicknames :fcl/tests.generics.applicative :fcl/t.ap)
  (:use :common-lisp :rove :fcl/tests.util :fcl.applicative)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:compose #:curry)
  (:export #:identity-test
           #:composition-test
           #:homomorphism-test
           #:interchange-test))
(in-package :fcl/tests.applicative)


(defmacro identity-test (a*)
  `(ok (data= (amap #'identity ,a*) ,a*)))

(defmacro composition-test (class b->*c a->*b a*)
  `(ok (data= (amap (amap (amap (unit ',class (curry #'compose)) ,b->*c) ,a->*b) ,a*)
              (amap ,b->*c (amap ,a->*b ,a*)))))

(defmacro homomorphism-test (class a->b a)
  `(ok (data= (amap (unit ',class ,a->b) (unit ',class ,a))
              (unit ',class (funcall ,a->b ,a)))))

(defmacro interchange-test (class a->*b a)
  (let ((g!a->b (gensym "A->B")))
    `(ok (data= (amap ,a->*b (unit ',class a))
                (amap (unit ',class (lambda (,g!a->b) (funcall ,g!a->b ,a))) ,a->*b)))))
