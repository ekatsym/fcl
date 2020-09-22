(defpackage fcl.defdata
  (:use :common-lisp)
  (:import-from
    :fcl
    #:defdata)
  (:import-from
    :fcl.util
    #:index
    #:proper-list
    #:symbolicate
    #:rpartial
    #:filter)
  (:import-from
    :fcl.lazy
    #:delay
    #:force
    #:promise)
  (:export
    #:defdata))

(in-package :fcl.defdata)


;;; Util
(defun make-parameters (n)
  "Returns a list of parameters whose size is N like (%0 %1 ... %N)."
  (check-type n index)
  (loop :for i :from 0 :below n
        :collect (symbolicate "%" (write-to-string i))))

(defun lazy-type-specifier-p (specifier)
  "Test whether SPECIFIER is (:LAZY <CL type specifier>) or not."
  (and (consp specifier) (eq (first specifier) :lazy)))

;;; Core
(defstruct (algebraic-data-type (:constructor nil)
                                (:copier nil)
                                (:predicate nil)))

(defmacro defdata (name &body constructors)
  "A macro for definition the structures as algebraic datatype."
  (every (lambda (constructor)
           (check-type constructor proper-list))
         constructors)
  `(progn
     (defstruct (,name (:constructor nil)
                       (:copier nil)
                       (:include algebraic-data-type)
                       (:predicate nil)))
     ,@(mapcar (rpartial #'parse-constructor name)
               constructors)
     ,@(mapcar #'parse-printer constructors)
     ',name))

;;; Constructor Parser
(defun parse-constructor (constructor data-name)
  "Parse algebraic constructor style code into CL's DEFSTRUCT code."
  (if (member-if #'lazy-type-specifier-p (rest constructor))
      (parse-lazy-constructor constructor data-name)
      (parse-strict-constructor constructor data-name)))

(defun parse-strict-constructor (constructor data-name)
  "PARSE-CONSTRUCTOR for strict evaluation."
  (let ((constructor-name (first constructor))
        (types (remove :lazy (rest constructor)))
        (parameters (make-parameters (length (remove :lazy (rest constructor))))))
    `(defstruct (,constructor-name (:conc-name ,constructor-name)
                                   (:constructor ,constructor-name ,parameters)
                                   (:copier nil)
                                   (:include ,data-name)
                                   (:predicate nil))
       ,@(mapcar (lambda (type parameter)
                   `(,parameter ,parameter :type ,type :read-only t))
                 types
                 parameters))))

(defun parse-lazy-constructor (constructor data-name)
  "PARSE-CONSTRUCTOR for lazy evaluation."
  (let* ((constructor-name (first constructor))
         (%constructor-name (symbolicate "%" constructor-name (write-to-string (random 1000000000000))))
         (types (rest constructor))
         (parameters (make-parameters (length (rest constructor))))
         (accessors (mapcar (lambda (parameter)
                              (symbolicate constructor-name parameter))
                            parameters)))
    `(progn
       ;; Define the constructor datatype as structure.
       (defstruct (,constructor-name (:conc-name ,%constructor-name)
                                     (:constructor ,%constructor-name ,parameters)
                                     (:copier nil)
                                     (:include ,data-name)
                                     (:predicate nil))
         ,@(mapcar (lambda (type parameter)
                     (if (lazy-type-specifier-p type)
                         `(,parameter ,parameter :type promise :read-only t)
                         `(,parameter ,parameter :type ,type :read-only t)))
                   types
                   parameters))

       ;; Define the constructor as macro for lazye evaluation.
       (defmacro ,constructor-name ,parameters
         `(,',%constructor-name ,@(mapcar (lambda (type parameter)
                                            (if (lazy-type-specifier-p type)
                                                (let ((var (gensym)))
                                                  `(delay
                                                     (let ((,var ,parameter))
                                                       (check-type ,var ,(second type))
                                                       ,var)))
                                                parameter))
                                          ',types
                                          (list ,@parameters))))

       ;; Define the accessors as the function including FORCE lazy evaluation
       ;; or the simple alias function for strict evaluation.
       ,@(mapcar (lambda (accessor type parameter)
                   `(defun ,accessor (instance)
                      ,(if (lazy-type-specifier-p type)
                           `(force (,(symbolicate %constructor-name parameter) instance))
                           `(,(symbolicate %constructor-name parameter) instance))))
                 accessors
                 types
                 parameters))))


;;; Data Printer Parser
(defun parse-printer (constructor)
  "Parse algebraic constructor style code into CL's PRINT-OBJECT definition to print algebraic datatypes"
  (let ((name (first constructor))
        (parameters (make-parameters (length (rest constructor)))))
    `(defmethod print-object ((object ,name) stream)
       (format stream
               "(~S~{ ~S~})"
               ',name
               (with-slots ,parameters object
                 (list ,@parameters))))))
