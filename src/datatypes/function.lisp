(defpackage fcl.datatypes.function
  (:nicknames :fcl.dt.function :fcl.dt.f :fcl.function :fcl.f)
  (:use
    :common-lisp
    :fcl.util.function)
  (:export
    #:compose
    #:partial
    #:rpartial
    #:partial-at
    #:curry
    #:rcurry
    #:pipeline))
(in-package :fcl.datatypes.function)
