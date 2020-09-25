(defpackage fcl.generics.recursive
  (:nicknames :fcl.g.recursive :fcl.recursive)
  (:use
    :common-lisp
    :fcl.generics.functor)
  (:export
    #:fmap
    #:cata
    #:para
    #:ana
    #:apo))
(in-package :fcl.generics.recursive)


