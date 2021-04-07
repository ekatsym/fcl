(defpackage fcl.util.list
  (:nicknames :fcl.u.list)
  (:use :common-lisp)
  (:import-from
    :fcl.u.type
    #:index)
  (:export
    #:nlist?
    #:take
    #:drop
    #:enum
    #:insert-at
    #:zip
    #:group
    #:reverse+))
(in-package :fcl.u.list)


(defun nlist? (n list)
  (check-type n index)
  (check-type list list)
  (do ((i n (1- i))
       (lst list (rest lst)))
      ((or (zerop i) (endp lst))
       (and (zerop i) (endp lst)))))

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

(defun zip (list &rest more-lists)
  (check-type list list)
  (every (lambda (lst) (check-type lst list)) more-lists)
  (apply #'mapcar #'list list more-lists))

(defun group (n list)
  (check-type n index)
  (check-type list list)
  (do ((lst list (drop n lst))
       (acc '() (cons (take n lst) acc)))
      ((endp lst) (nreverse acc))))

(defun reverse+ (list)
  (check-type list list)
  (do ((lst list (rest lst))
       (acc '() (cons lst acc)))
      ((endp lst) acc)))
