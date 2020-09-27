(defpackage fcl.generics.iterable
  (:nicknames :fcl.g.iterable :fcl.iterable)
  (:use :common-lisp
        :fcl.generics.recursive)
  (:export
    #:cata
    #:para
    #:ana
    #:apo
    #:iterate
    #:iterate+
    #:accumulate
    #:accumulate+
    #:iteratel
    #:iteratel+
    #:accumulatel
    #:accumulatel+))
(in-package :fcl.generics.iterable)


(defgeneric iterate (x->x n))

(defgeneric iterate+ (i&x->x n))

(defgeneric accumulate (x->? x->x x))

(defgeneric accumulate+ (x->? x->x n0 x))

(defgeneric iteratel (x->x n))

(defgeneric iteratel+ (i&x->x n))

(defgeneric accumulatel (x->? x->x x))

(defgeneric accumulatel+ (x->? x->x n0 x))
