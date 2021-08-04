(defpackage fcl.util.list
  (:use :common-lisp)
  (:import-from
    :fcl.util.type
    #:index)
  (:import-from
    :fcl.util
    #:nlist?
    #:take
    #:drop
    #:enum
    #:insert-at
    #:zip
    #:unzip
    #:transpose
    #:group))
(in-package :fcl.util.list)


(defun nlist? (n list)
  (check-type n index)
  (check-type list list)
  (do ((i n (1- i))
       (lst list (rest lst)))
      ((or (zerop i) (endp lst))
       (and (zerop i) (endp lst)))))

(defun length= (sequence1 sequence2)
  (check-type sequence1 sequence)
  (check-type sequence2 sequence)
  (cond ((and (listp sequence1) (listp sequence2))
         (do ((lst1 sequence1 (rest lst1))
              (lst2 sequence2 (rest lst2)))
             ((or (endp lst1) (endp lst2))
              (and (endp lst1) (endp lst2)))))
        ((listp sequence1)
         (nlist? (length sequence2) sequence1))
        ((listp sequence2)
         (nlist? (length sequence1) sequence2))
        (t
         (= (length sequence1) (length sequence2)))))

(defun take (n list)
  (check-type n index)
  (check-type list list)
  (do ((i n (1- i))
       (lst list (rest lst))
       (acc '() (cons (first lst) acc)))
      ((zerop i) (nreverse acc))))

(defun drop (n list)
  (check-type n index)
  (check-type list list)
  (nthcdr n list))

(defun sublist (list start &optional end)
  (check-type list list)
  (check-type start index)
  (check-type end (or null index))
  (if end
      (take (- end start) (drop start list))
      (drop start list)))

(defun enum (start end)
  (assert (typep (- end start) 'index) (start end)
          'type-error :datum (- end start) :expected-type 'index)
  (do ((i start (1+ i))
       (acc '() (cons i acc)))
      ((>= i end) (nreverse acc))))

(defun insert-at (n x list)
  (check-type n index)
  (check-type list list)
  (do ((i n (1- i))
       (tail list (rest tail))
       (rhead '() (cons (first tail) rhead)))
      ((or (zerop i) (endp tail)) (revappend rhead (cons x tail)))))

(defun filter (predicate list)
  (check-type predicate function)
  (check-type list list)
  (reduce (lambda (x acc)
            (let ((x (funcall predicate x)))
              (if x (cons x acc) acc)))
          list
          :initial-value '()
          :from-end t))

(defun mappend (function list &rest more-lists)
  (check-type function function)
  (check-type list list)
  (mapc (lambda (lst) (check-type lst list)) more-lists)
  (reduce (lambda (args acc) (append (apply function args) acc))
          (zip (cons list more-lists))
          :initial-value '()
          :from-end t))

(defun zip (list &rest more-lists)
  (check-type list list)
  (mapc (lambda (lst) (check-type lst list)) more-lists)
  (apply #'mapcar #'list list more-lists))

(defun unzip (list)
  (check-type list list)
  (mapc (lambda (x) (check-type x list)) list)
  (apply #'values (apply #'mapcar #'list list)))

(defun transpose (lists)
  (check-type lists list)
  (mapc (lambda (lst) (check-type lst list)) lists)
  (mapcar #'list lists))

(defun group (n list)
  (check-type n index)
  (check-type list list)
  (do ((lst list (drop n lst))
       (acc '() (cons (take n lst) acc)))
      ((endp lst) (nreverse acc))))
