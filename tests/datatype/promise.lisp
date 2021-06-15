(defpackage fcl/tests.promise
  (:nicknames :fcl/tests.data.promise :fcl/t.pm)
  (:use :common-lisp :rove :fcl/tests.util :fcl.promise)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match)
  (:import-from :fcl.util #:compose #:partial))
(in-package fcl/tests.promise)


(deftest matching
  (testing "DELAY"
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (match (delay a)
              ((delay b) (equal a b))))))))

(deftest delay=unit
  (testing "Equality of DELAY and UNIT"
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (data= (delay a) (unit 'promise a)))))))

(deftest functor
  (testing "Identity"
    (dotimes (i 1000)
      (let ((a (random-object)))
        (ok (data= (fmap #'identity (delay a)) (delay a))))))
  (testing "Composition"
    (dotimes (i 1000)
      (let ((a (random-number (floor -1.0e6) (floor 1.0e6))))
        (ok (data= (fmap (lambda (x) (1+ (* x x))) (delay a))
                   (fmap #'1+ (fmap (lambda (x) (* x x)) (delay a))))))
      (let ((a (random-number -1.0e6 1.0e6)))
        (ok (data= (fmap (lambda (x) (1+ (* x x))) (delay a))
                   (fmap #'1+ (fmap (lambda (x) (* x x)) (delay a))))))
      (let ((a (random-character)))
        (ok (data= (fmap (compose #'code-char #'1+ #'char-code) (delay a))
                   (fmap (compose #'code-char #'1+) (fmap #'char-code (delay a))))))
      (let ((a (random-list 0 1000)))
        (ok (data= (fmap (compose #'reverse (partial #'mapcar #'1+)) (delay a))
                   (fmap #'reverse (fmap (partial #'mapcar #'1+) (delay a))))))
      (let ((a (random-string 0 1000)))
        (ok (data= (fmap (compose #'reverse (partial #'map 'string #'char-upcase))
                         (delay a))
                   (fmap #'reverse
                         (fmap (partial #'map 'string #'char-upcase) (delay a)))))))))
