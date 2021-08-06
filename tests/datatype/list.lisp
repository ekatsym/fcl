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
    (dotimes (i 100)
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
    (dotimes (i 100)
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
    (dotimes (i 100)
      (let ((a (random-object)))
        (ok (data= (list a) (unit 'list a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 100)
      (mlet ((a* (list '()
                       (random-list 1 1000))))
        (ok (data= (fmap #'identity a*)
                   a*))
        '())))
  (testing "Composition"
    (dotimes (i 100)
      (mlet ((a* (list '()
                       (random-list 1 1000))))
        (let ((a->b (lambda (x) (* x x)))
              (b->c (lambda (x) (+ x x))))
          (ok (data= (fmap (compose b->c a->b) a*)
                     (fmap b->c (fmap a->b a*))))
          '())))))
