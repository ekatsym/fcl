(defpackage fcl.array
  (:nicknames :fcl.data.array :fcl.ar)
  (:use :common-lisp :fcl.monad-plus :fcl.foldable)
  (:export
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:guard
    #:ac
    #:mzero #:mplus #:msum
    #:foldr #:foldr+ #:unfoldr #:unfoldr+
    #:foldl #:foldl+ #:unfoldl #:unfoldl+
    #:lfoldr #:lfoldr+
    #:lfoldl #:lfoldl+
    #:scanr #:scanr+ #:scanl #:scanl+))
(in-package :fcl.array)


;;; FOLDABLE
(defmethod foldr (a&x->x x0 (as array))
  (check-type a&x->x function)
  (let* ((dims (array-dimensions as))
         (first-dim (first dims))
         (rest-dims (rest dims))
         (total-size (array-total-size as)))
    (do ((x x0 (funcall a&x->x 
                        (make-array rest-dims
                                    :displaced-to as
                                    :displaced-index-offset (* total-size
                                                               (/ i first-dim)))
                        x))
         (i (1- first-dim) (1- i)))
        ((<= i -1) x))))

(defmethod foldr+ (a&as&x->x x0 (as array))
  (check-type a&as&x->x function)
  (let* ((dims (array-dimensions as))
         (first-dim (first dims))
         (rest-dims (rest dims))
         (total-size (array-total-size as)))
    (do ((x x0 (funcall a&as&x->x
                        (make-array rest-dims
                                    :displaced-to as
                                    :displaced-index-offset (* total-size
                                                               (/ i first-dim)))
                        (make-array (cons c rest-dims)
                                    :displaced-to as
                                    :displaced-index-offset (* total-size
                                                               (/ (1+ i) first-dim)))
                        x))
         (i (1- first-dim) (1- i))
         (c 0 (1+ c)))
        ((<= i -1) x))))

(defmethod unfoldr ((class (eql 'array)) x->? x->a x->x x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (let* ((as-list (unfoldr 'list x->? x->a x->x x)))
    (if (and (every #'arrayp as-list)
             (every (lambda (a) (equal (array-dimensions a)
                                       (array-dimensions (first as-list))))
                    (rest as-list)))
        (let* ((as (make-array (cons (length as-list) (array-dimensions (first as-list)))))
               (i 0))
          (dolist (a as-list as)
            (dotimes (j (array-total-size a))
              (setf (row-major-aref as (+ i j))
                    (row-major-aref a j)))
            (incf i (array-total-size a))))
        (coerce as-list 'vector))))


;;; MONAD-PLUS
(defmethod unit ((class (eql 'array)) a)
  (typecase a
    (array (let ((dims (cons 1 (array-dimensions a))))
             (make-array dims :displaced-to a)))
    (otherwise (make-array 1 :initial-element a))))

(define-fmap-by-monad array)

(define-amap-by-monad array)

(defmethod mmap (a->b* (a* array))
  (check-type a->b* function)
  (let* ((bs (foldr (lambda (a bs) (cons (funcall a->b* a) bs)) '() a*))
         (b-dims (array-dimensions (first bs)))
         (b* (make-array (cons (* (first b-dims) (length bs)) (rest b-dims))))
         (i 0))
    (assert (every (lambda (b) (equal (array-dimensions b) b-dims)) (rest bs)) (a->b*))
    (dolist (b bs b*)
      (dotimes (j (array-total-size b))
        (setf (row-major-aref b* (+ i j))
              (row-major-aref b j)))
      (incf i (array-total-size b)))))

(defmethod mzero ((class (eql 'array)))
  (make-array '() :initial-element nil))

(defmethod mplus ((monoid1 array) monoid2)
  (check-type monoid2 array)
  (let* ((dims1 (array-dimensions monoid1))
         (dims2 (array-dimensions monoid2)))
    (assert (equal (rest dims1) (rest dims2)) (monoid1 monoid2))
    (let ((acc (make-array (cons (+ (first dims1) (first dims2)) (rest dims1))))
          (i 0))
      (dolist (monoid (list monoid1 monoid2) acc)
        (dotimes (j (array-total-size monoid))
          (setf (row-major-aref acc (+ i j))
                (row-major-aref monoid j)))
        (incf i (array-total-size monoid))))))

