(defpackage fcl/tests.util
  (:use :common-lisp :fiveam)
  (:export #:gen-object
           #:gen-function
           #:gen-num-function))
(in-package :fcl/tests.util)


(defun gen-object (&rest more-elements)
  (funcall
    (apply #'gen-one-element
           (gen-integer)
           (gen-float)
           (gen-character)
           (gen-string)
           (gen-string :elements (gen-character :alphanumericp t))
           (gen-list)
           (gen-list :elements (if (zerop (random 5))
                                   (apply #'gen-object more-elements)
                                   (gen-integer :min -10 :max 10)))
           (gen-tree :size (random 20))
           (gen-tree :size (random 10)
                     :elements (if (zerop (random 5))
                                   (apply #'gen-object more-elements)
                                   (gen-integer :min -10 :max 10)))
           (gen-buffer)
           (gen-buffer :element-type t
                       :elements (if (zerop (random 5))
                                     (apply #'gen-object more-elements)
                                     (gen-integer :min -10 :max 10)))
           more-elements)))

(defun gen-function (&rest more-functions)
  (let ((n (1+ (random 10))))
    (lambda ()
    (nth (random (+ 4 (length more-functions)))
         (append
           (list #'list
                 (lambda (x) (make-list n :initial-element x))
                 #'vector
                 (lambda (x) (make-array n :initial-element x)))
           more-functions)))))

(defun gen-num-function (&rest more-functions)
  (let ((n (funcall (gen-integer :max 100 :min -100))))
    (lambda ()
      (nth (random (+ 7 (length more-functions)))
           (append
             (list #'1+ #'1- #'abs
                   (lambda (x) (+ x n))
                   (lambda (x) (* x n))
                   (lambda (x) (- x n))
                   (lambda (x) (/ x (if (zerop n) 1 n))))
             more-functions)))))
