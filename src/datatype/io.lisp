(defpackage fcl.io
  (:nicknames :fcl.data.io)
  (:use :common-lisp :fcl.monad-plus)
  (:import-from :fcl.adata #:defdata)
  (:import-from :fcl.match #:ematch)
  (:export
    ;; core
    #:action #:run-action #:run-wizard
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
(defdata action
  (%action function))

(defun run-action (action)
  (ematch action
    ((%action act) (funcall act))))


;;; Output functions
(defun put-char (character)
  (check-type character character)
  (%action (lambda () (write-char character) nil)))

(defun put-string (string)
  (check-type string string)
  (%action (lambda () (write-string string) nil)))

(defun put-line (string)
  (check-type string string)
  (%action (lambda () (write-line string) nil)))

(defun put-control-string (control-string &rest args)
  (check-type control-string string)
  (%action (lambda () (apply #'format t control-string args) nil)))

(defun put-object (object)
  (%action (lambda () (write object) nil)))

;;; Input functions
(defun get-char ()
  (%action #'read-char))

(defun get-string ()
  (%action
    (lambda ()
      (coerce (loop for c = (read-char *standard-input* nil)
                    while c
                    collect c)
              'string))))

(defun get-line ()
  (%action #'read-line))

(defun get-object ()
  (%action #'read))

(defun interact (respondent)
  (check-type respondent function)
  (%action
    (lambda ()
      (loop for in = (read-line)
            with out = (funcall respondent in)
            do (check-type out string)
            do (write-line out)))))


;;; Monad Plus
(defmethod unit ((class (eql 'action)) a)
  (%action (lambda () a)))

(define-fmap-by-monad action)

(define-amap-by-monad action)

(defmethod mmap (a->b* (a* action))
  (%action
    (lambda ()
      (run-action (funcall a->b* (run-action a*))))))

(defmethod mzero ((class (eql 'action)))
  (unit 'action nil))

(defmethod mplus ((monoid1 action) (monoid2 action))
  (mlet ((a1 monoid1)
         (a2 monoid2))
    (unit 'action (mplus a1 a2))))
