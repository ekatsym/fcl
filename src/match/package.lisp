(defpackage :fcl.match
  (:use
    :common-lisp
    :fcl.match.core)
  (:import-from
    :fcl.lazy
    #:delay)
  (:export
    #:match
    #:delay))
