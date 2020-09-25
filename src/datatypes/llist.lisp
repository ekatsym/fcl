(defpackage fcl.datatypes.llist
  (:nicknames fcl.dt.llist)
  (:use :common-lisp)
  (:import-from
    :fcl.data
    #:defdata)
  (:import-from
    :fcl.match
    #:match)
  (:export
    #:llist
    #:lnil
    #:lcons #:lcons%0 #:lcons%1))
(in-package :fcl.datatypes.llist)


(defdata llist
  (lnil)
  (lcons (:lazy t) (:lazy llist)))

(defmethod print-object ((object lnil) stream)
  (format stream "#.(LNIL)"))

(defmethod print-object ((object lcons) stream)
  (match object
    ((lcons a b)
     (format stream "#.(LCONS ~S ~S)" a b))))
