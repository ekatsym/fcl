(defpackage fcl/tests.promise
  (:nicknames :fcl/tests.data.promise :fcl/t.pm)
  (:use :common-lisp :rove :fcl/tests.util :fcl.promise)
  (:import-from :fcl.adata #:data=)
  (:import-from :fcl.match #:match))
(in-package fcl/tests.promise)


(deftest matching
  (testing "DELAY"
    (dotimes (i 1000)
      (ok (let ((a (random-number (floor -1.0e9) (floor 1.0e9))))
            (match (delay a)
              ((delay b) (= a b)))))
      (ok (let ((a (random-number -1.0e9 1.0e9)))
            (match (delay a)
              ((delay b) (= a b)))))
      (ok (let ((a (random-character)))
            (match (delay a)
              ((delay b) (char= a b)))))
      (ok (let ((a (random-list 0 1000)))
            (match (delay a)
              ((delay b) (equal a b)))))
      (ok (let ((a (random-string 0 1000)))
            (match (delay a)
              ((delay b) (string= a b))))))))

(deftest delay=unit
  (testing "Equality of DELAY and UNIT"
    (dotimes (i 1000)
      (ok (let ((a (random-number (floor -1.0e9) (floor 1.0e9))))
            (data= (delay a) (unit 'promise a))))
      (ok (let ((a (random-number -1.0e9 1.0e9)))
            (data= (delay a) (unit 'promise a))))
      (ok (let ((a (random-character)))
            (data= (delay a) (unit 'promise a))))
      (ok (let ((a (random-list 0 1000)))
            (data= (delay a) (unit 'promise a))))
      (ok (let ((a (random-string 0 1000)))
            (data= (delay a) (unit 'promise a)))))))
