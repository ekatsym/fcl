(defpackage fcl.match
  (:nicknames :fcl.core.match :fcl.mt)
  (:use :common-lisp :fcl.lazy :fcl.util :fcl.match.parser)
  (:export
    #:match #:ematch

    ;; common-lisp
    #:quote #:cons #:list #:vector

    ;; lazy
    #:delay))
(in-package :fcl.match)


(defmacro match (datum &body clauses)
  (let ((g!datum (gensym "MATCH")))
    `(let ((,g!datum ,datum))
       (cond ,@(mapcar (lambda (clause) (parse-clause g!datum clause)) clauses)))))

(defmacro ematch (datum &body clauses)
  `(match ,datum
     ,@clauses
     (_ (error 'ematch-error
               :datum ,datum
               :expected-patterns ',(mapcar #'first clauses)))))

(define-condition ematch-error (type-error)
  ((datum             :initarg :datum             :accessor ematch-datum)
   (expected-patterns :initarg :expected-patterns :accessor ematch-expected-patterns))
  (:report
    (lambda (o s)
      (format s "~S does not match any of patterns.~%Patterns: ~S"
              (ematch-datum o)
              (ematch-expected-patterns o)))))
