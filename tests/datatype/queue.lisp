(defpackage fcl/tests.queue
  (:nicknames :fcl/tests.data.queue :fcl/t.qu)
  (:use :common-lisp :fiveam :fcl/tests.util :fcl.queue)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose #:partial #:curry))
(in-package :fcl/tests.queue)


(defun gen-queue (&key (length (gen-integer :min 0 :max 10)) (elements (gen-integer :min -10 :max 10)))
  (lambda ()
    (unfoldl 'queue
             #'zerop
             #'1-
             (lambda (_)
               (declare (ignore _))
               (funcall elements))
             (funcall length))))


(def-suite* queue-tests :in :fcl/tests)

(def-suite* monad-plus :in queue-tests)

(fcl/tests.functor:functor-test
  queue1
  (gen-queue :length (gen-integer :min 0 :max 30)
             :elements (gen-object))
  (gen-function)
  (gen-function))

(fcl/tests.functor:functor-test
  queue2
  (gen-queue :length (gen-integer :min 0 :max 30)
             :elements (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.applicative:applicative-test
  queue1
  queue
  (gen-object)
  (gen-queue :length (gen-integer :min 0 :max 30)
             :elements (gen-object))
  (gen-function)
  (gen-function))

(fcl/tests.applicative:applicative-test
  queue2
  queue
  (gen-integer :min -100 :max 100)
  (gen-queue :length (gen-integer :min 0 :max 30)
             :elements (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.monad:monad-test
  queue1
  queue
  (gen-object)
  (gen-queue :length (gen-integer :min 0 :max 30)
             :elements (gen-object))
  (gen-one-element
    (constantly (empty))
    #'queue
    (lambda (a) (unfoldl 'queue #'zerop #'1- (constantly a) 10))
    (lambda (a) (queue a (list a) (vector a))))
  (gen-one-element
    (constantly (empty))
    #'queue
    (lambda (b) (unfoldl 'queue #'zerop #'1- (constantly b) 20))
    (lambda (b) (queue (queue b b b) (queue (queue b b b))))))

(fcl/tests.monad:monad-test
  queue2
  queue
  (gen-integer :min -100 :max 100)
  (gen-queue :length (gen-integer :min 0 :max 30)
             :elements (gen-integer :min -100 :max 100))
  (gen-one-element
    (constantly (empty))
    #'queue
    (lambda (a) (unfoldr 'queue #'zerop (lambda (x) (+ a x -5)) #'1- 10))
    (lambda (a) (qu-append (unfoldr 'queue #'zerop (lambda (x) (* a x)) #'1- 5)
                          (unfoldr 'queue #'zerop (lambda (x) (- (* a x))) #'1- 5))))
  (gen-one-element
    (constantly '())
    #'queue
    (lambda (b) (unfoldr 'queue #'zerop (lambda (x) (+ b x -5)) #'1- 10))
    (lambda (b) (qu-append (unfoldr 'queue #'zerop (lambda (x) (* b x)) #'1- 5)
                           (unfoldr 'queue #'zerop (lambda (x) (- (* b x))) #'1- 5)))))

(fcl/tests.monoid:monoid-test
  queue
  queue
  (gen-queue :length (gen-integer :min 0 :max 30)
            :elements (gen-object)))

