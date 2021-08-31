(defpackage fcl/tests.util
  (:nicknames :fcl/tests.util)
  (:use :common-lisp :rove)
  (:export
    #:random-number
    #:random-character
    #:random-list
    #:random-string
    #:random-object
    #:functions
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
  (check-type min-length (integer 0 *))
  (check-type max-length (integer 0 *))
  (assert (<= min-length max-length))
  (let ((random-fn (or random-fn (lambda () (random-number -1.0d6 1.0d6)))))
    (loop repeat (random-number min-length max-length) collect (funcall random-fn))))

(defun random-string (min-length max-length)
  (map 'string
       #'code-char
       (random-list min-length max-length
                    :random-fn (lambda () (random-number #x20 #x7e)))))

(defun random-object ()
  (let ((p (random 10000)))
    (cond ((< p 3000) (random-number -1000000 1000000))
          ((< p 6000) (random-number -1.0d6 1.0d6))
          ((< p 9000) (random-character))
          ((< p 9600) (random-string 0 10))
          ((< p 9660) (random-string 0 100))
          ((< p 9666) (random-string 0 1000))
          ((< p 9966) (random-list 0 1    :random-fn #'random-object))
          ((< p 9996) (random-list 0 10   :random-fn #'random-object))
          ((< p 9999) (random-list 0 100  :random-fn #'random-object))
          (t          (random-list 0 1000 :random-fn #'random-object)))))

(defun functions ()
  (list (lambda (x) (+ x x))
        (lambda (x) (+ x x x))
        (lambda (x) (* x x))
        (lambda (x) (* x x x))
        #'sin
        #'cos))

(defun random-function ()
  (let ((fs (functions)))
    (nth (random (length fs)) fs)))
