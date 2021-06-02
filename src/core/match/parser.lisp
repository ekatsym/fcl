(defpackage fcl.match.parser
  (:use :common-lisp :fcl.match.util)
  (:import-from
    :fcl.util
    #:nlist?
    #:enum
    #:group
    #:mappend
    #:zip
    #:partial
    #:symbolicate)
  (:import-from
    :fcl.lazy
    #:delay
    #:force
    #:promise)
  (:import-from
    :fcl.adata
    #:algebraic-datatype)
  (:export
    #:parse-clause))
(in-package :fcl.match.parser)


(defun parse-clause (data clause)
  (destructuring-bind (pattern . body) clause
    `(,(pattern->test data pattern)
      ,(pattern->bind data pattern (if (nlist? 1 body)
                                       (car body)
                                       `(progn ,@body))))))

;;; Test Parser
(defun pattern->test (data pattern)
  (typecase pattern
    (cons      (%cons-pattern->test data pattern))
    (boolean   (%literal-pattern->test data pattern))
    (symbol    (%symbol-pattern->test data pattern))
    (otherwise (%literal-pattern->test data pattern))))

(defun %cons-pattern->test (data pattern)
  (case (car pattern)
    (quote     (%literal-pattern->test data pattern))
    (cons      (%%cons-pattern->test data pattern))
    (list      (%%list-pattern->test data pattern))
    (vector    (%%vector-pattern->test data pattern))
    (delay     (%%lazy-pattern->test data pattern))
    (otherwise (%%class-pattern->test data pattern))))

(defun %symbol-pattern->test (data pattern)
  (if (member pattern *standard-atomic-type-specifiers*)
      `(typep ,data ',pattern)
      't))

(defun %literal-pattern->test (data pattern)
  `(equalp ,data ,pattern))

(defun %%cons-pattern->test (data pattern)
  (destructuring-bind (_ car-pat cdr-pat) pattern
    (declare (ignore _))
    (let ((g!car (gensym "CAR"))
          (g!cdr (gensym "CDR")))
      `(and (consp ,data)
            (let ((,g!car (car ,data))
                  (,g!cdr (cdr ,data)))
              (declare (ignorable ,g!car ,g!cdr))
              (and ,(pattern->test g!car car-pat)
                   ,(pattern->test g!cdr cdr-pat)))))))

(defun %%list-pattern->test (data pattern)
  (destructuring-bind (_ . pats) pattern
    (declare (ignore _))
    (pattern->test data (reduce (lambda (pat acc) `(cons ,pat ,acc))
                                pats
                                :initial-value 'nil
                                :from-end t))))

(defun %%vector-pattern->test (data pattern)
  (destructuring-bind (_ . pats) pattern
    (declare (ignore _))
    `(and (vectorp ,data)
          (= ,(length pats) (length ,data))
          ,@(mapcar (lambda (pat i)
                      (let ((g!vi (gensym "VI")))
                        `(let ((,g!vi (svref ,data ,i)))
                           (declare (ignorable ,g!vi))
                           ,(pattern->test g!vi pat))))
                    pats
                    (enum 0 (length pats))))))

(defun %%lazy-pattern->test (data pattern)
  (destructuring-bind (_ pat) pattern
    (declare (ignore _))
    `(and (typep ,data 'promise)
          ,(let ((g!forced (gensym "FORCED")))
             `(let ((,g!forced (force ,data)))
                (declare (ignorable ,g!forced))
                ,(pattern->test g!forced pat))))))

(defun %%class-pattern->test (data pattern)
  (let ((name (first pattern)))
    (if (nlist? 1 pattern)
        `(typep ,data ',name)
        `(if (subtypep ',name 'algebraic-datatype)
             ,(%%%algebraic-datatype-pattern->test data pattern)
             ,(%%%normal-class-pattern->test data pattern)))))

(defun %%%algebraic-datatype-pattern->test (data pattern)
  (destructuring-bind (name . pats) pattern
    `(and (typep ,data ',name)
          (typep ,data 'algebraic-datatype)
          ,@(mapcar (lambda (pat param)
                      (let ((g!accessor (gensym "ACCESSOR"))
                            (g!slot (gensym "SLOT")))
                        `(let* ((,g!accessor (symbol-function
                                               (intern ,(concatenate 'string (string name) (string param))
                                                       (symbol-package ',name))))
                                (,g!slot (funcall ,g!accessor ,data)))
                           (declare (ignorable ,g!slot))
                           ,(pattern->test g!slot pat))))
                    pats
                    (make-parameters (length pats))))))

(defun %%%normal-class-pattern->test (data pattern)
  (destructuring-bind (name . key-pats) pattern
    `(and (typep ,data ',name)
          ,@(mapcar (lambda (key-pat)
                      (destructuring-bind (key pat) key-pat
                        (if (keywordp key)
                            (let ((g!slot (gensym "SLOT")))
                              `(let ((,g!slot (slot-value ,data ',(symbolicate key))))
                                 (declare (ignorable ,g!slot))
                                 ,(pattern->test g!slot pat)))
                            nil)))
                    (group 2 key-pats)))))


;;; Bind Parser
(defun pattern->bind (data pattern body)
  (typecase pattern
    (cons      (%cons-pattern->bind data pattern body))
    (symbol    (%symbol-pattern->bind data pattern body))
    (otherwise body)))

(defun %cons-pattern->bind (data pattern body)
  (case (car pattern)
    (quote     body)
    (cons      (%%cons-pattern->bind data pattern body))
    (list      (%%list-pattern->bind data pattern body))
    (vector    (%%vector-pattern->bind data pattern body))
    (delay     (%%lazy-pattern->bind data pattern body))
    (otherwise (%%class-pattern->bind data pattern body))))

(defun %symbol-pattern->bind (data pattern body)
  (cond ((member pattern *standard-atomic-type-specifiers*) body)
        ((string= "_" (string pattern)) body)
        (t `(let ((,pattern ,data)) ,body))))

(defun %%cons-pattern->bind (data pattern body)
  (destructuring-bind (_ car-pat cdr-pat) pattern
    (declare (ignore _))
    (let ((g!car (gensym "CAR"))
          (g!cdr (gensym "CDR")))
      `(let ((,g!car (car ,data))
             (,g!cdr (cdr ,data)))
         (declare (ignorable ,g!car ,g!cdr))
         ,(pattern->bind g!car
                         car-pat
                         (pattern->bind g!cdr
                                        cdr-pat
                                        body))))))

(defun %%list-pattern->bind (data pattern body)
  (destructuring-bind (_ . pats) pattern
    (declare (ignore _))
    (pattern->bind data
                   (reduce (lambda (pat acc) `(cons ,pat ,acc))
                           pats
                           :initial-value 'nil
                           :from-end t)
                   body)))

(defun %%vector-pattern->bind (data pattern body)
  (destructuring-bind (_ . pats) pattern
    (declare (ignore _))
    (reduce (lambda (i-pat body)
              (destructuring-bind (i pat) i-pat
                (let ((g!vi (gensym "VI")))
                  `(let ((,g!vi (svref ,data ,i)))
                     (declare (ignorable ,g!vi))
                     ,(pattern->bind g!vi pat body)))))
            (zip (enum 0 (length pats)) pats)
            :initial-value body
            :from-end t)))

(defun %%lazy-pattern->bind (data pattern body)
  (destructuring-bind (_ pat) pattern
    (declare (ignore _))
    (let ((g!forced (gensym "FORCED")))
      `(let ((,g!forced (force ,data)))
         (declare (ignorable ,g!forced))
         ,(pattern->bind g!forced pat body)))))

(defun %%class-pattern->bind (data pattern body)
  (let ((name (first pattern)))
    (if (nlist? 1 pattern)
        body
        `(if (subtypep ',name 'algebraic-datatype)
             ,(%%%algebraic-datatype-pattern->bind data pattern body)
             ,(%%%normal-class-pattern->bind data pattern body)))))

(defun %%%algebraic-datatype-pattern->bind (data pattern body)
  (destructuring-bind (name . pats) pattern
    (reduce (lambda (param-pat body)
              (destructuring-bind (param pat) param-pat
                (let ((g!accessor (gensym "ACCESSOR"))
                      (g!slot (gensym "SLOT")))
                  `(let* ((,g!accessor (symbol-function
                                         (intern ,(concatenate 'string (string name) (string param))
                                                 (symbol-package ',name))))
                          (,g!slot (funcall ,g!accessor ,data)))
                     (declare (ignorable ,g!slot))
                     ,(pattern->bind g!slot pat body)))))
            (zip (make-parameters (length pats)) pats)
            :initial-value body
            :from-end t)))

(defun %%%normal-class-pattern->bind (data pattern body)
  (destructuring-bind (name . pats) pattern
    (declare (ignore name))
    (reduce (lambda (key-pat body)
              (destructuring-bind (key pat) key-pat
                (if (keywordp key)
                    (let ((g!slot (gensym "SLOT")))
                      `(let ((,g!slot (slot-value ,data ',(symbolicate key))))
                         (declare (ignorable ,g!slot))
                         ,(pattern->bind g!slot pat body)))
                    `(error 'unknown-keyword-argument :name ',key))))
            (group 2 pats)
            :initial-value body
            :from-end t)))


(define-condition unknown-keyword-argument (program-error)
  ((name :reader unknown-keyword-argument-name :initarg :name))
  (:report
   (lambda (condition stream)
     (format stream "Unknown &KEY argument: ~S"
             (unknown-keyword-argument-name condition)))))
