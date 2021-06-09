(defpackage fcl.reader
  (:nicknames :fcl.data.reader :fcl.rd)
  (:use :common-lisp :fcl.monad)
  (:import-from
    :fcl.adata
    #:defdata)
  (:import-from
    :fcl.match
    #:ematch)
  (:export
    #:reader #:run-reader #:getr #:local
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo))
(in-package :fcl.reader)


;;; Definition
(defdata reader
  (%reader function))

(defun run-reader (reader r)
  (check-type reader reader)
  (funcall (%reader%0 reader) r))

(defun getr ()
  (%reader #'identity))

(defun local (r->r a*)
  (check-type r->r function)
  (check-type a* reader)
  (%reader
    (lambda (r)
      (run-reader a* (funcall r->r r)))))


;;; MONAD
(defmethod unit ((class (eql 'reader)) a)
  (%reader (constantly a)))

(define-fmap-by-monad reader)

(define-amap-by-monad reader)

(defmethod mmap (a->b* (a* reader))
  (check-type a->b* function)
  (%reader
    (lambda (r)
      (run-reader (funcall a->b* (run-reader a* r)) r))))
