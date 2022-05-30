(defpackage fcl/tests.list
  (:nicknames :fcl/tests.data.list :fcl/t.ls)
  (:use :common-lisp :fiveam :fcl/tests.util :fcl.list)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose #:partial #:curry))
(in-package :fcl/tests.list)


(def-suite* list-tests :in :fcl/tests)

(def-suite* pattern-match :in list-tests)

(test match-nil
  "Pattern Match for NIL"
  ;; nil
  (match '()
    (() (pass))
    (_   (fail)))
  (match '()
    ('() (pass))
    (_   (fail)))
  (match '()
    (nil (pass))
    (_   (fail)))
  (match '()
    ('nil (pass))
    (_   (fail))))

(test match-cons
  "Pattern Match for CONS"
  (for-all ((a (gen-object))
            (b (gen-object)))
    (match (cons a b)
      ('()        (fail))
      ((cons x y) (is (and (data= a x) (data= b y))))
      (_          (fail)))
    (match (cons a '())
      ('()          (fail))
      ((cons x '()) (is (data= a x)))
      (_            (fail)))
    (match (cons a (cons b '()))
      ('()                   (fail))
      ((cons x (cons y '())) (is (and (data= a x) (data= b y))))
      (_                     (fail)))
    (match (cons a '())
      ('()        (fail))
      ((cons x _) (is (data= a x)))
      (_          (fail)))
    (match (cons a (cons b '()))
      ('()        (fail))
      ((cons x _) (is (data= a x)))
      (_          (fail)))))

(test match-list
  "Pattern Match for LIST"
  (for-all ((a (gen-object))
            (b (gen-object))
            (c (gen-object)))
    (match (list)
      ((list)       (pass))
      ((list _)     (fail))
      ((list _ _)   (fail))
      ((list _ _ _) (fail))
      (_            (fail)))
    (match (list a)
      ((list)       (fail))
      ((list x)     (is (data= a x)))
      ((list _ _)   (fail))
      ((list _ _ _) (fail))
      (_            (fail)))
    (match (list a b)
      ((list)       (fail))
      ((list _)     (fail))
      ((list x y)   (is (and (data= a x) (data= b y))))
      ((list _ _ _) (fail))
      (_            (fail)))
    (match (list a b c)
      ((list)       (fail))
      ((list _)     (fail))
      ((list _ _)   (fail))
      ((list x y z) (is (and (data= a x) (data= b y) (data= c z))))
      (_            (fail)))))

(def-suite* monad-plus :in list-tests)

(fcl/tests.functor:functor-test
  list1
  (gen-list :length (gen-integer :min 0 :max 30)
            :elements (gen-object))
  (gen-function)
  (gen-function))

(fcl/tests.functor:functor-test
  list2
  (gen-list :length (gen-integer :min 0 :max 30)
            :elements (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.applicative:applicative-test
  list1
  list
  (gen-object)
  (gen-list :length (gen-integer :min 0 :max 30)
            :elements (gen-object))
  (gen-function)
  (gen-function))

(fcl/tests.applicative:applicative-test
  list2
  list
  (gen-integer :min -100 :max 100)
  (gen-list :length (gen-integer :min 0 :max 30)
            :elements (gen-integer :min -100 :max 100))
  (gen-num-function)
  (gen-num-function))

(fcl/tests.monad:monad-test
  list1
  list
  (gen-object)
  (gen-list :length (gen-integer :min 0 :max 30)
            :elements (gen-object))
  (gen-one-element
    (constantly '())
    #'list
    (lambda (a) (make-list 10 :initial-element a))
    (lambda (a) (list a (list a) (vector a))))
  (gen-one-element
    (constantly '())
    #'list
    (lambda (b) (make-list 20 :initial-element b))
    (lambda (b) (list b (list b b) (list b b b)))))

(fcl/tests.monad:monad-test
  list2
  list
  (gen-integer :min -100 :max 100)
  (gen-list :length (gen-integer :min 0 :max 30)
            :elements (gen-integer :min -100 :max 100))
  (gen-one-element
    (constantly '())
    #'list
    (lambda (a) (unfoldr 'list #'zerop (lambda (x) (+ a x -5)) #'1- 10))
    (lambda (a) (append (unfoldr 'list #'zerop (lambda (x) (* a x)) #'1- 5)
                        (unfoldr 'list #'zerop (lambda (x) (- (* a x))) #'1- 5))))
  (gen-one-element
    (constantly '())
    #'list
    (lambda (b) (unfoldr 'list #'zerop (lambda (x) (+ b x -5)) #'1- 10))
    (lambda (b) (append (unfoldr 'list #'zerop (lambda (x) (* b x)) #'1- 5)
                        (unfoldr 'list #'zerop (lambda (x) (- (* b x))) #'1- 5)))))

(fcl/tests.monoid:monoid-test
  list
  list
  (gen-list :length (gen-integer :min 0 :max 30)
            :elements (gen-object)))
