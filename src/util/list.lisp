(defpackage fcl.util.list
  (:nicknames :fcl.u.list)
  (:use :common-lisp)
  (:import-from
    :fcl.u.type
    #:index)
  (:export
    #:nlist?
    #:insert-at
    #:filter
    #:mappend
    #:zip))
(in-package :fcl.u.list)


(defun nlist? (n lst)
  (check-type n index)
  (check-type lst list)
  (do ((i n (1- i))
       (l lst (rest l)))
      ((or (zerop i) (endp l))
       (and (zerop i) (endp l)))))

(defun insert-at (n x lst)
  (check-type n index)
  (check-type lst list)
  (do ((i n (1- i))
       (tail lst (rest tail))
       (rhead '() (cons (first tail) rhead)))
      ((or (zerop i) (endp tail)) (revappend rhead (cons x tail)))))

(defun filter (func lst &rest more-lsts)
  (check-type func function)
  (check-type lst list)
  (every (lambda (lst) (check-type lst list)) more-lsts)
  (do ((lsts (cons lst more-lsts) (mapcar #'rest lsts))
       (acc '() (let ((x (apply func (mapcar #'first lsts))))
                  (if (null x)
                      acc
                      (cons x acc)))))
      ((some #'endp lsts) (nreverse acc))))

(defun mappend (func lst &rest more-lsts)
  (check-type func function)
  (check-type lst list)
  (every (lambda (lst) (check-type lst list)) more-lsts)
  (do ((lsts (cons lst more-lsts) (mapcar #'rest lsts))
       (acc '() (revappend (apply func (mapcar #'first lsts)) acc)))
      ((some #'endp lsts) (nreverse acc))))

(defun zip (lst &rest more-lsts)
  (check-type lst list)
  (every (lambda (lst) (check-type lst list)) more-lsts)
  (apply #'mapcar #'list lst more-lsts))
