(defpackage fcl.io
  (:nicknames :fcl.data.io)
  (:use :common-lisp :fcl.monad-plus)
  (:import-from :fcl.adata #:defdata)
  (:import-from :fcl.match #:ematch)
  (:export
    ;; core
    #:io-action #:run-io-action #:run-wizard
    #:put-char #:put-string #:put-line #:put-object
    #:get-char #:get-string #:get-line #:get-object
    #:interact

    ;; monad-plus
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:mzero #:mplus #:msum
    #:guard))
(in-package :fcl.io)


;;; Definition
(defdata io-action
  (%io-action function))

(defun run-io-action (io-action)
  (ematch io-action
    ((%io-action act) (funcall act))))


;;; Output functions
(defun put-char (character)
  (check-type character character)
  (%io-action (lambda () (write-char character) nil)))

(defun put-string (string)
  (check-type string string)
  (%io-action (lambda () (write-string string) nil)))

(defun put-line (string)
  (check-type string string)
  (%io-action (lambda () (write-line string) nil)))

(defun put-control-string (control-string &rest args)
  (check-type control-string string)
  (%io-action (lambda () (apply #'format t control-string args) nil)))

(defun put-object (object)
  (%io-action (lambda () (write object) nil)))

;;; Input functions
(defun get-char ()
  (%io-action #'read-char))

(defun get-string ()
  (%io-action
    (lambda ()
      (coerce (loop for c = (read-char *standard-input* nil)
                    while c
                    collect c)
              'string))))

(defun get-line ()
  (%io-action #'read-line))

(defun get-object ()
  (%io-action #'read))

(defun interact (respondent)
  (check-type respondent function)
  (%io-action
    (lambda ()
      (loop for in = (read-line)
            with out = (funcall respondent in)
            do (check-type out string)
            do (write-line out)))))


;;; Monad Plus
(defmethod unit ((class (eql 'io-action)) a)
  (%io-action (lambda () a)))

(define-fmap-by-monad io-action)

(define-amap-by-monad io-action)

(defmethod mmap (a->b* (a* io-action))
  (%io-action
    (lambda ()
      (run-io-action (funcall a->b* (run-io-action a*))))))

(defmethod mzero ((class (eql 'io-action)))
  (unit 'io-action nil))

(defmethod mplus ((monoid1 io-action) (monoid2 io-action))
  (mlet ((a1 monoid1)
         (a2 monoid2))
    (unit 'io-action (mplus a1 a2))))
