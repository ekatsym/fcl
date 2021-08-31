(defpackage fcl/tests.util
  (:nicknames :fcl/tests.util)
  (:use :common-lisp :rove)
  (:export
    #:random-number
    #:random-character
    #:random-list
    #:random-string
    #:random-object
    #:random-function))
(in-package :fcl/tests.util)


(setq *default-reporter* :dot)

(defun random-number (min max)
  (assert (<= min max) (min max)
          'simple-error
          :format-control "~S is larger than ~S."
          :format-args (list min max))
  (+ (random (- max min)) min))

(defun random-character (&key (min (code-char #x20)) (max (code-char #x7e)))
  (check-type min character)
  (check-type max character)
  (assert (char<= min max) (min max)
          'simple-error
          :format-control "~S is larger than ~S."
          :format-args (list min max))
  (code-char (random-number (char-code min) (char-code max))))

(defun random-list (min-length max-length &key random-fn)
  (let ((random-fn (or random-fn (lambda () (random-number -1.0d6 1.0d6)))))
    (loop repeat (random-number min-length max-length) collect
          (funcall random-fn))))

(defun random-string (min-length max-length)
  (map 'string
       #'code-char
       (random-list min-length max-length
                    :random-fn (lambda () (random-number #x20 #x7e)))))

(defun random-object ()
  (case (random 5)
    (0 (random-number (floor -1.0d6) (floor 1.0d6)))
    (1 (random-number -1.0d6 1.0d6))
    (2 (random-character))
    (3 (random-list 0 1000))
    (4 (random-string 0 1000))))

(defun random-function ()
  (case (random 6)
    (0 (lambda (x) (+ x x)))
    (1 (lambda (x) (+ x x x)))
    (2 (lambda (x) (* x x)))
    (3 (lambda (x) (* x x x)))
    (4 #'sin)
    (5 #'cos)))
