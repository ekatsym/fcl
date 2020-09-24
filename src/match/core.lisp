(defpackage fcl.match.core
  (:nicknames :fcl.ma.core)
  (:use
    :common-lisp
    :fcl.ma.util
    :fcl.ma.parser)
  (:export
    #:match))
(in-package :fcl.match.core)


(defmacro match (data &body clauses)
  (let ((g!data (gensym "MATCH")))
    `(let ((,g!data ,data))
       (cond ,@(mapcar (lambda (clause) (parse-clause g!data clause)) clauses)))))
