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


(defmacro match (data &body clauses)
  (let ((g!data (gensym "MATCH")))
    `(let ((,g!data ,data))
       (cond ,@(mapcar (lambda (clause) (parse-clause g!data clause)) clauses)))))

(defmacro ematch (data &body clauses)
  `(match ,data
     ,@clauses
     (_ (error 'ematch-error
               :datum ,data
               :expected-patterns ',(mapcar #'first clauses)))))

(define-condition ematch-error (type-error)
  ((datum             :initarg :datum             :accessor ematch-datum)
   (expected-patterns :initarg :expected-patterns :accessor ematch-expected-patterns))
  (:report
    (lambda (o s)
      (format s "~S does not match any of patterns.~%Patterns: ~S"
              (ematch-datum o)
              (ematch-expected-patterns o)))))
