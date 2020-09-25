(defpackage fcl.datatypes.maybe
  (:nicknames fcl.dt.maybe)
  (:use :common-lisp)
  (:import-from
    :fcl.data
    #:defdata)
  (:export
    #:maybe
    #:nothing
    #:just))
(in-package :fcl.datatypes.maybe)


(defdata maybe
  (nothing)
  (just t))
