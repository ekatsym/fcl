(defpackage fcl.recursive
  (:nicknames :fcl.generics.recursive :fcl.rc)
  (:use :common-lisp :fcl.functor)
  (:import-from :fcl.adata #:defdata)
  (:import-from :fcl.match #:ematch)
  (:import-from :fcl.util #:partial)
  (:export
    #:polynomi
    #:construct #:destruct
    #:define-polynomial-conversions
    #:cata #:para #:ana #:apo

    ;;; Functor
    #:fmap))
(in-package :fcl.generics.recursive)


;;; Polynomial
(defdata polynomial
  (term symbol symbol list list))

(defgeneric construct (class f*))

(defgeneric destruct (i))

(defmacro define-polynomial-convertions (name &body clauses)
  (let ((g!f* (gensym "F*"))
        (g!class (gensym "CLASS"))
        (g!i (gensym "I")))
    `(progn
       (defmethod construct ((,g!class (eql ',name)) ,g!f*)
         (ematch ,g!f*
           ,@(mapcar (lambda (clause) (ematch clause
                                        ((list f poly) `(,poly ,f))))
                     clauses)))
       (defmethod destruct ((,g!i ,name))
         (ematch ,g!i ,@clauses)))))

(defun cata (a*->a i)
  (funcall a*->a (fmap (partial #'cata a*->a) (destruct i))))

(defun ana (a->a* a)
  (ematch (funcall a->a* a)
    ((term data _ _ _)
     (construct data (fmap (partial #'ana a->a*) (funcall a->a* a))))))


;;; Functor
(defmethod fmap (a->b (a* polynomial))
  (check-type a->b function)
  (ematch a*
    ((term data constructor b+a*s types)
     (term data
           constructor
           (mapcar (lambda (b+a* type)
                     (if (eq type data)
                         (funcall a->b b+a*)
                         b+a*))
                   b+a*s types)
           types))))
