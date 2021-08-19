(defpackage fcl.recursive
  (:nicknames :fcl.generics.recursive :fcl.rc)
  (:use :common-lisp)
  (:import-from :fcl.adata #:defdata)
  (:import-from :fcl.match #:ematch)
  (:import-from :fcl.util #:partial #:group)
  (:export
    #:polynomial #:term
    #:construct #:destruct
    #:cata #:para #:ana #:apo))
(in-package :fcl.generics.recursive)


;;; Term
(defdata polynomial
  (term symbol list))


;;; Construct and Destruct
(defgeneric construct (class adt*))

(defgeneric destruct (adt))

(defmacro define-polynomial-convertors (class &body pattern-pairs)
  (let ((g!adt (gensym "ADT"))
        (g!adt* (gensym "ADT*")))
    `(progn
       (defmethod construct ((class (eql ',class)) ,g!adt*)
         (ematch ,g!adt*
           ,@(mapcar (lambda (pairs)
                       (assert (= (length pairs) 2))
                       (list (second pairs) (first pairs)))
                     pattern-pairs)))
       (defmethod destruct ((,g!adt ,class))
         (ematch ,g!adt
           ,@pattern-pairs)))))


;;; Catamorphisms and Anamorphisms
(defun cata (x*->a adt)
  (ematch (destruct adt)
    ((term var product)
     (funcall x*->a (fmap (partial #'cata x*->a) var t (term var product))))))

(defun ana (class a->x* a)
  (ematch (funcall a->x* a)
    ((term var product)
     (construct class (fmap (partial #'ana class a->x*) var var (term var product))))))

(defun para (adt&x*->a adt)
  (ematch (destruct adt)
    ((term var product)
     (ematch (fmap (partial #'para adt&x*->a) var t (term var product))
       ((term _ product)
        (funcall adt&x*->a (term var (acons adt var product))))))))

(defun apo (class a->adt+x* a)
  (ematch (funcall a->adt+x* a)
    ((term var product)
     (construct class (fmap (partial #'apo class a->adt+x*) var var (term var product))))
    (adt adt)))

(defun fmap (a->b tag-a tag-b x*)
  (check-type a->b function)
  (check-type tag-a symbol)
  (check-type tag-b symbol)
  (check-type x* polynomial)
  (ematch x*
    ((term var product)
     (term var (mapcar (lambda (tagged)
                         (destructuring-bind (val . tag) tagged
                           (if (eq tag tag-a)
                               (cons (funcall a->b val) tag-b)
                               tagged)))
                       product)))))
