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


(defun nlist? (n lst)
  (check-type n index)
  (check-type lst list)
  (do ((i n (1- i))
       (l lst (rest l)))
      ((or (zerop i) (endp l))
       (and (zerop i) (endp l)))))

(defun take (n lst)
  (check-type n index)
  (check-type lst list)
  (do ((i n (1- i))
       (l lst (rest l))
       (acc '() (cons (first l) acc)))
      ((or (zerop i) (endp l))
       (nreverse acc))))

(defun drop (n lst)
  (check-type n index)
  (check-type lst list)
  (nthcdr n lst))

(defun enum (start end)
  (assert (typep (- end start) 'index) (start end)
          'type-error :datum (- end start) :expected-type 'index)
  (do ((i start (1+ i))
       (acc '() (cons i acc)))
      ((>= i end) (nreverse acc))))

(defun insert-at (n x lst)
  (check-type n index)
  (check-type lst list)
  (do ((i n (1- i))
       (tail lst (rest tail))
       (rhead '() (cons (first tail) rhead)))
      ((or (zerop i) (endp tail)) (revappend rhead (cons x tail)))))

(defun zip (lst &rest more-lsts)
  (check-type lst list)
  (every (lambda (lst) (check-type lst list)) more-lsts)
  (apply #'mapcar #'list lst more-lsts))

(defun group (n lst)
  (check-type n index)
  (check-type lst list)
  (do ((l lst (drop n l))
       (acc '() (cons (take n l) acc)))
      ((endp l) (nreverse acc))))

(defun reverse+ (lst)
  (check-type lst list)
  (do ((lst lst (rest lst))
       (acc '() (cons lst acc)))
      ((endp lst) acc)))
