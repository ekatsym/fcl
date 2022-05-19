(defpackage fcl/tests.list
  (:nicknames :fcl/tests.data.list :fcl/t.ls)
  (:use :common-lisp :rove :fcl/tests.util :fcl.list)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose #:partial #:curry))
(in-package :fcl/tests.list)


(deftest matching
  (testing "NIL"
    (ok (match '()
          ('() t)
          (_   nil)))
    (ok (match '()
          (nil t)
          (_   nil))))
  (testing "CONS"
    (dotimes (i 10)
      (let ((a (random-object))
            (b (random-object)))
        (ok (match (cons a b)
              ('()        nil)
              ((cons x y) (and (data= a x) (data= b y)))))
        (ok (match (cons a '())
              ('()          nil)
              ((cons x '()) (data= a x))))
        (ok (match (cons a (cons b '()))
              ('()                   nil)
              ((cons x (cons y '())) (and (data= a x) (data= b y)))))
        (ok (match (cons a '())
              ('()        nil)
              ((cons x _) (data= a x))))
        (ok (match (cons a (cons b '()))
              ('()        nil)
              ((cons x _) (data= a x)))))))
  (testing "LIST"
    (dotimes (i 10)
      (let ((a (random-object))
            (b (random-object))
            (c (random-object)))
        (ok (match (list)
              ((list)       t)
              ((list _)     nil)
              ((list _ _)   nil)
              ((list _ _ _) nil)))
        (ok (match (list a)
              ((list)       nil)
              ((list x)     (data= a x))
              ((list _ _)   nil)
              ((list _ _ _) nil)))
        (ok (match (list a b)
              ((list)       nil)
              ((list _)     nil)
              ((list x y)   (and (data= a x) (data= b y)))
              ((list _ _ _) nil)))
        (ok (match (list a b c)
              ((list)       nil)
              ((list _)     nil)
              ((list _ _)   nil)
              ((list x y z) (and (data= a x) (data= b y) (data= c z)))))))))

(deftest nil=mzero
  (testing "Equality of NIL and MZERO"
    (ok (data= '() (mzero 'list)))))

(deftest list=unit
  (testing "Equality of LIST and UNIT"
    (dotimes (i 10)
      (let ((a (random-object)))
        (ok (data= (list a) (unit 'list a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 10)
      (mlet ((a* (list '() (random-list 1 500 :random-fn #'random-object))))
        (fcl/tests.functor:identity-test a*))))
  (testing "Composition"
    (dotimes (i 10)
      (let ((a->b (random-function))
            (b->c (random-function)))
        (mlet ((a* (list '() (random-list 1 500))))
          (fcl/tests.functor:composition-test b->c a->b a*))))))

(deftest applicative
  (testing "Identity"
    (dotimes (i 10)
      (mlet ((a* (list '() (random-list 1 500 :random-fn #'random-object))))
        (fcl/tests.applicative:identity-test 'list a*))))
  (testing "Composition"
    (dotimes (i 10)
      (mlet ((a*    (list '() (random-list 1 500)))
             (a->*b (list '()
                          (random-list 1 5 :random-fn #'random-function)
                          (functions)))
             (b->*c (list '()
                          (random-list 1 5 :random-fn #'random-function)
                          (functions))))
        (fcl/tests.applicative:composition-test 'list b->*c a->*b a*))))
  (testing "Homomorphism"
    (dotimes (i 10)
      (let ((a    (random-number -1.0d6 1.0d6))
            (a->b (random-function)))
        (fcl/tests.applicative:homomorphism-test 'list a->b a))))
  (testing "Interchange"
    (dotimes (i 10)
      (let ((a (random-number -1.0d6 1.0d6)))
        (mlet ((a->*b (list '()
                            (random-list 1 5 :random-fn #'random-function)
                            (functions))))
          (fcl/tests.applicative:interchange-test 'list a->*b a))))))

(deftest monad
  (testing "Left Identity"
    (dotimes (i 10)
      (let ((a (random-number -1.0d6 1.0d6)))
        (mlet ((a->*b (list (random-list 1 5 :random-fn #'random-function)
                            (functions)))
               (a->b* (list (constantly '())
                            (lambda (a)
                              (fmap (lambda (a->b) (funcall a->b a)) a->*b)))))
          (fcl/tests.monad:left-identity-test 'list a->b* a)))))
  (testing "Right Identity"
    (dotimes (i 10)
      (mlet ((a* (list '() (random-list 1 500 :random-fn #'random-function))))
        (fcl/tests.monad:right-identity-test 'list a*))))
  (testing "Associativity"
    (dotimes (i 10)
      (mlet ((a*    (list '() (random-list 1 500)))
             (a->*b (list (random-list 1 5 :random-fn #'random-function)
                          (functions)))
             (b->*c (list (random-list 1 5 :random-fn #'random-function)
                          (functions)))
             (a->b* (list (constantly '())
                          (lambda (a)
                            (fmap (lambda (a->b) (funcall a->b a)) a->*b))))
             (b->c* (list (constantly '())
                          (lambda (b)
                            (fmap (lambda (b->c) (funcall b->c b)) b->*c)))))
        (fcl/tests.monad:associativity-test a->b* b->c* a*)))))

(deftest monoid
  (testing "Left Identity"
    (dotimes (i 10)
      (mlet ((a* (list '() (random-list 1 500 :random-fn #'random-object))))
        (fcl/tests.monoid:left-identity-test 'list a*))))
  (testing "Right Identity"
    (dotimes (i 10)
      (mlet ((a* (list '() (random-list 1 500 :random-fn #'random-object))))
        (fcl/tests.monoid:right-identity-test 'list a*))))
  (testing "Associativity"
    (dotimes (i 10)
      (mlet ((a* (list '() (random-list 1 500 :random-fn #'random-object)))
             (b* (list '() (random-list 1 500 :random-fn #'random-object)))
             (c* (list '() (random-list 1 500 :random-fn #'random-object))))
        (fcl/tests.monoid:associativity-test a* b* c*)))))
