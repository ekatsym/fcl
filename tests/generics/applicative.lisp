(defpackage fcl/tests.applicative
  (:nicknames :fcl/tests.generics.applicative :fcl/t.ap)
  (:use :common-lisp :fiveam :fcl.applicative)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.util #:compose #:curry)
  (:export #:identity-test
           #:composition-test
           #:homomorphism-test
           #:interchange-test))
(in-package :fcl/tests.applicative)


(defmacro identity-test (class a*)
  `(is (data= (amap (unit ,class #'identity) ,a*) ,a*)))

(defmacro composition-test (class b->*c a->*b a*)
  `(is (data= (amap (amap (amap (unit ,class (curry #'compose)) ,b->*c) ,a->*b) ,a*)
              (amap ,b->*c (amap ,a->*b ,a*)))))

(defmacro homomorphism-test (class a->b a)
  `(is (data= (amap (unit ,class ,a->b) (unit ,class ,a))
              (unit ,class (funcall ,a->b ,a)))))

(defmacro interchange-test (class a->*b a)
  (let ((g!a->b (gensym "A->B")))
    `(is (data= (amap ,a->*b (unit ,class ,a))
                (amap (unit ,class (lambda (,g!a->b) (funcall ,g!a->b ,a))) ,a->*b)))))
