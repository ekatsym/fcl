(defpackage fcl/tests.queue
  (:nicknames :fcl/tests.data.queue :fcl/t.qu)
  (:use :common-lisp :rove :fcl/tests.util :fcl.queue)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose #:partial #:curry))
(in-package :fcl/tests.queue)


;;; Utility for Property Tests
(defun random-queue ()
  (unfoldr 'queue
           #'zerop
           (lambda (_)
             (declare (ignore _))
             (random-object))
           #'1-
           (random 30)))

(defun random-number-queue ()
  (unfoldr 'queue
           #'zerop
           (lambda (_)
             (declare (ignore _))
             (random-number -1.0d6 1.0d6))
           #'1-
           (random 30)))

(defun random-function-queue ()
  (unfoldr 'queue
           #'zerop
           (lambda (_)
             (declare (ignore _))
             (random-function))
           #'1-
           (random 10)))

(defun queue-functions ()
  (list->queue (functions)))


;;; Tests
(deftest empty=mzero
  (testing "Equality of EMPTY and MZERO"
    (ok (data= (empty) (mzero 'queue)))))

(deftest add+empty=unit
  (testing "Equality of composition of ADD and EMPTY and UNIT"
    (dotimes (i 100)
      (let ((a (random-object)))
        (ok (data= (add a (empty)) (unit 'queue a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 100)
      (mlet ((a* (list (empty) (random-queue))))
        (fcl/tests.functor:identity-test a*))))
  (testing "Composition"
    (dotimes (i 100)
      (let ((a->b (random-function))
            (b->c (random-function)))
        (mlet ((a* (list (empty) (random-number-queue))))
          (fcl/tests.functor:composition-test b->c a->b a*))))))

(deftest applicative
  (testing "Identity"
    (dotimes (i 100)
      (mlet ((a* (list (empty) (random-number-queue))))
        (fcl/tests.applicative:identity-test 'queue a*))))
  (testing "Composition"
    (dotimes (i 100)
      (mlet ((a*    (list (empty) (random-number-queue)))
             (a->*b (list (empty) (random-function-queue) (queue-functions)))
             (b->*c (list (empty) (random-function-queue) (queue-functions))))
        (fcl/tests.applicative:composition-test 'queue b->*c a->*b a*))))
  (testing "Homomorphism"
    (dotimes (i 100)
      (let ((a    (random-number -1.0d6 1.0d6))
            (a->b (random-function)))
        (fcl/tests.applicative:homomorphism-test 'queue a->b a))))
  (testing "Interchange"
    (dotimes (i 100)
      (let ((a (random-number -1.0d6 1.0d6)))
        (mlet ((a->*b (list (empty) (random-function-queue) (queue-functions))))
          (fcl/tests.applicative:interchange-test 'queue a->*b a))))))

(deftest monad
  (testing "Left Identity"
    (dotimes (i 100)
      (let ((a (random-number -1.0d6 1.0d6)))
        (mlet ((a->*b (list (empty) (random-function-queue) (queue-functions)))
               (a->b* (list (constantly (empty))
                            (lambda (a)
                              (fmap (lambda (a->b) (funcall a->b a)) a->*b)))))
          (fcl/tests.monad:left-identity-test 'queue a->b* a)))))
  (testing "Right Identity"
    (dotimes (i 100)
      (mlet ((a* (list (empty) (random-queue))))
        (fcl/tests.monad:right-identity-test 'queue a*))))
  (testing "Associativity"
    (dotimes (i 40)
      (mlet ((a*    (list (empty) (random-number-queue)))
             (a->*b (list (empty) (random-function-queue) (queue-functions)))
             (b->*c (list (empty) (random-function-queue) (queue-functions)))
             (a->b* (list (constantly (empty))
                          (lambda (a)
                            (fmap (lambda (a->b) (funcall a->b a)) a->*b))))
             (b->c* (list (constantly (empty))
                          (lambda (a)
                            (fmap (lambda (b->c) (funcall b->c a)) b->*c)))))
        (fcl/tests.monad:associativity-test a->b* b->c* a*)))))

(deftest monoid
  (testing "Left Identity"
    (dotimes (i 100)
      (mlet ((a* (list (empty) (random-queue))))
        (fcl/tests.monoid:left-identity-test 'queue a*))))
  (testing "Right Identity"
    (dotimes (i 100)
      (mlet ((a* (list (empty) (random-queue))))
        (fcl/tests.monoid:right-identity-test 'queue a*))))
  (testing "Associativity"
    (dotimes (i 50)
      (mlet ((a* (list (empty) (random-queue)))
             (b* (list (empty) (random-queue)))
             (c* (list (empty) (random-queue))))
        (fcl/tests.monoid:associativity-test a* b* c*)))))
