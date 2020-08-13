(defpackage fcl.datatypes.writer
  (:nicknames fcl.dt.writer)
  (:use :common-lisp)
  (:import-from
    :fcl
    #:writer
    #:run-writer
    #:pass
    #:hear
    #:tell
    #:hears
    #:censor)
  (:import-from
    :fcl.defdata
    #:defdata)
  (:import-from
    :fcl.monad
    #:unit
    #:fmap
    #:amap
    #:mmap
    #:monad-do
    #:mprogn
    #:mlet)
  (:import-from
    :fcl.monoid
    #:mzero
    #:mplus
    #:msum)
  (:import-from
    :fcl.monad+
    #:guard)
  (:export
    #:writer
    #:run-writer
    #:pass
    #:hear
    #:tell
    #:hears
    #:censor
    #:unit
    #:fmap
    #:amap
    #:mmap
    #:monad-do
    #:mprogn
    #:mlet
    #:mzero
    #:mplus
    #:msum
    #:guard))

(in-package fcl.dt.writer)


;;; Message
(defdata message
  (%message function))

(defun get-message (message)
  (check-type message message)
  (%message%0 message))

(defun to-message (as)
  (check-type as list)
  (%message (lambda (xs) (append as xs))))

(defun from-message (msg)
  (check-type msg message)
  (funcall (%message%0 msg) '()))

(defmethod mzero ((class (eql 'message)))
  (to-message '()))

(defmethod mplus ((monoid1 message) monoid2)
  (check-type monoid2 message)
  (%message
    (lambda (xs)
      (funcall (%message%0 monoid1)
               (funcall (%message%0 monoid2) xs)))))


;;; Definition
(defdata writer
  (%writer t message))

(defun run-writer (writer)
  (check-type writer writer)
  (list (%writer%0 writer) (from-message (%writer%1 writer))))


;;; Functor, Applicative and Monad
(defmethod unit ((class (eql 'writer)) x)
  (%writer x (to-message '())))

(defmethod fmap (a->b (a* writer))
  (check-type a->b function)
  (with-slots ((a %0) (w %1)) a*
    (%writer (funcall a->b a) w)))

(defmethod amap (a->*b (a* writer))
  (check-type a->*b writer)
  (with-slots ((a->b %0) (w0 %1)) a->*b
    (with-slots ((a %0) (w1 %1)) a*
      (%writer (funcall a->b a) (mplus w0 w1)))))

(defmethod mmap (a->b* (a* writer))
  (check-type a->b* function)
  (with-slots ((a0 %0) (w0 %1)) a*
    (with-slots ((a1 %0) (w1 %1)) (funcall a->b* a0)
      (%writer a1 (mplus w0 w1)))))


;;; General Utility
(defun pass (writer)
  (check-type writer writer)
  (with-slots ((a %0) (w %1)) writer
    (destructuring-bind (x f) a
      (%writer x (%message (lambda (xs) (funcall f (funcall (%message%0 w) xs))))))))

(defun hear (writer)
  (check-type writer writer)
  (with-slots ((a %0) (w %1)) writer
    (%writer (list a (from-message w)) w)))

(defun tell (message)
  (%writer '() (to-message (list message))))

(defun hears (function writer)
  (check-type function function)
  (check-type writer writer)
  (mlet ((aw (hear writer)))
    (destructuring-bind (a w) aw
      (unit 'writer (list a (funcall function (from-message w)))))))

(defun censor (function writer)
  (check-type function function)
  (check-type writer writer)
  (pass
    (mlet ((a writer))
      (unit 'writer (list a function)))))
