(defpackage fcl.array
  (:nicknames :fcl.data.array :fcl.ar)
  (:use :common-lisp :fcl.monad-plus :fcl.foldable :fcl.unfoldable)
  (:export
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:guard
    #:ac
    #:mzero #:mplus #:msum
    #:foldr #:foldr+ #:unfoldr #:unfoldr+
    #:foldl #:foldl+ #:unfoldl #:unfoldl+
    #:delay #:force
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
                        (make-array (cons (- first-dim i 1) rest-dims)
                                    :displaced-to as
                                    :displaced-index-offset (* total-size
                                                               (/ (1+ i) first-dim)))
                        x))
         (i (1- first-dim) (1- i)))
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

(defmethod unfoldr+ ((class (eql 'array)) x->? x->a x->x as0 x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (check-type as0 array)
  (let* ((as-list (unfoldr 'list x->? x->a x->x x)))
    (if (and (every #'arrayp as-list)
             (every (lambda (a) (equal (array-dimensions a)
                                       (rest (array-dimensions as0))))
                    as-list))
        (let* ((as (make-array (cons (+ (length as-list) (array-dimension as0 0))
                                     (array-dimensions (first as-list)))))
               (i 0))
          (dolist (a as-list)
            (dotimes (j (array-total-size a))
              (setf (row-major-aref as (+ i j))
                    (row-major-aref a j)))
            (incf i (array-total-size a)))
          (dotimes (j (array-total-size as0) as)
            (setf (row-major-aref as (+ i j))
                  (row-major-aref as0 j))))
        (concatenate
          'vector
          as-list
          (loop for i below (array-dimension as0 0) collect
                (make-array
                  (rest (array-dimensions as0))
                  :displaced-to as0
                  :displaced-index-offset (* i (/ (array-total-size as0)
                                                  (array-dimension as0 0)))))))))

(defmethod foldl (x&a->x x0 (as array))
  (check-type x&a->x function)
  (let* ((dims (array-dimensions as))
         (first-dim (first dims))
         (rest-dims (rest dims))
         (total-size (array-total-size as)))
    (do ((x x0 (funcall x&a->x
                        x
                        (make-array rest-dims
                                    :displaced-to as
                                    :displaced-index-offset (* total-size
                                                               (/ i first-dim)))))
         (i 0 (1+ i)))
        ((>= i first-dim) x))))

(defmethod foldl+ (x&a&as->x x0 (as array))
  (check-type x&a&as->x function)
  (let* ((dims (array-dimensions as))
         (first-dim (first dims))
         (rest-dims (rest dims))
         (total-size (array-total-size as)))
    (do ((x x0 (funcall x&a&as->x
                        x
                        (make-array rest-dims
                                    :displaced-to as
                                    :displaced-index-offset (* total-size
                                                               (/ i first-dim)))
                        (make-array (cons (- first-dim i 1) rest-dims)
                                    :displaced-to as
                                    :displaced-index-offset (* total-size
                                                               (/ (1+ i) first-dim)))))
         (i 0 (1+ i)))
        ((>= i first-dim) x))))

(defmethod unfoldl ((class (eql 'array)) x->? x->x x->a x)
  (check-type x->? function)
  (check-type x->x function)
  (check-type x->a function)
  (let* ((as-list (unfoldl 'list x->? x->a x->x x)))
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

(defmethod unfoldl+ ((class (eql 'array)) x->? x->a x->x as0 x)
  (check-type x->? function)
  (check-type x->a function)
  (check-type x->x function)
  (check-type as0 array)
  (let* ((as-list (unfoldl 'list x->? x->a x->x x)))
    (if (and (every #'arrayp as-list)
             (every (lambda (a) (equal (array-dimensions a)
                                       (rest (array-dimensions as0))))
                    as-list))
        (let* ((as (make-array (cons (+ (length as-list) (array-dimension as0 0))
                                     (array-dimensions (first as-list)))))
               (i 0))
          (dolist (a as-list)
            (dotimes (j (array-total-size a))
              (setf (row-major-aref as (+ i j))
                    (row-major-aref a j)))
            (incf i (array-total-size a)))
          (dotimes (j (array-total-size as0) as)
            (setf (row-major-aref as i)
                  (row-major-aref as0 j))
            (incf i)))
        (concatenate
          'vector
          as-list
          (loop for i below (array-dimension as0 0) collect
                (make-array
                  (rest (array-dimensions as0))
                  :displaced-to as0
                  :displaced-index-offset (* i (/ (array-total-size as0)
                                                  (array-dimension as0 0)))))))))

(defmethod lfoldr (a&$x->x x0 (as array))
  (check-type a&$x->x function)
  (let* ((dims (array-dimensions as))
         (first-dim (first dims))
         (rest-dims (rest dims))
         (total-size (array-total-size as)))
    (labels ((rec (i)
               (if (>= i first-dim)
                   x0
                   (funcall a&$x->x
                            (make-array rest-dims
                                        :displaced-to as
                                        :displaced-index-offset (* total-size
                                                                   (/ i first-dim)))
                            (delay (rec (1+ i)))))))
      (rec 0))))

(defmethod lfoldr+ (a&as&$x->x x0 (as array))
  (check-type a&as&$x->x function)
  (let* ((dims (array-dimensions as))
         (first-dim (first dims))
         (rest-dims (rest dims))
         (total-size (array-total-size as)))
    (labels ((rec (i)
               (if (>= i first-dim)
                   x0
                   (funcall a&as&$x->x
                            (make-array rest-dims
                                        :displaced-to as
                                        :displaced-index-offset (* total-size
                                                                   (/ i first-dim)))
                            (make-array (cons (- first-dim i 1) rest-dims)
                                        :displaced-to as
                                        :displaced-index-offset (* total-size
                                                                   (/ (1+ i) first-dim)))
                            (delay (rec (1+ i)))))))
      (rec 0))))

(defmethod lfoldl ($x&a->x x0 (as array))
  (check-type $x&a->x function)
  (let* ((dims (array-dimensions as))
         (first-dim (first dims))
         (rest-dims (rest dims))
         (total-size (array-total-size as)))
    (labels ((rec (i $x)
               (if (>= i first-dim)
                   (force $x)
                   (rec (1+ i)
                        (delay
                          (funcall
                            $x&a->x
                            $x
                            (make-array rest-dims
                                        :displaced-to as
                                        :displaced-index-offset (* total-size
                                                                   (/ i first-dim)))))))))
      (rec 0 (delay x0)))))

(defmethod lfoldl+ ($x&a&as->x x0 (as array))
  (check-type $x&a&as->x function)
  (let* ((dims (array-dimensions as))
         (first-dim (first dims))
         (rest-dims (rest dims))
         (total-size (array-total-size as)))
    (labels ((rec (i $x)
               (if (>= i first-dim)
                   (force $x)
                   (rec (1+ i)
                        (delay
                          (funcall
                            $x&a&as->x
                            $x
                            (make-array rest-dims
                                        :displaced-to as
                                        :displaced-index-offset (* total-size
                                                                   (/ i first-dim)))
                            (make-array (cons (- first-dim i 1) rest-dims)
                                        :displaced-to as
                                        :displaced-index-offset (* total-size
                                                                 (/ (1+ i) first-dim)))))))))
      (rec 0 (delay x0)))))


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

(defmethod mplus ((monoid1 array) (monoid2 array))
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

