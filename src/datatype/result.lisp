(defpackage fcl.result
  (:nicknames :fcl.data.result :fcl.rs)
  (:use :common-lisp :fcl.monad-plus)
  (:import-from :fcl.adata #:defdata)
  (:import-from :fcl.match #:ematch)
  (:export
    #:result #:success #:failure
    #:run-result #:ensure-result
    #:unit #:fmap #:amap #:mmap
    #:mlet #:mprogn #:mdo
    #:mzero #:mplus #:msum
    #:guard))
(in-package :fcl.result)


;;; Stack Trace
(defdata stack-trace
  (condit condition)
  (strace list stack-trace))

(defun run-stack-trace (stack-trace)
  (when (typep stack-trace 'strace)
    (format *error-output* "~%Output of FCL.RESULT:RUN-STACK-TRACE:~%"))
  (labels ((rec (strc)
             (declare (optimize (space 3)))
             (ematch strc
               ((condit cnd)          (format *error-output*
                                              "~&~2T~S~%~%"
                                              cnd)
                                      (error cnd))
               ((strace lst trc) (format *error-output*
                                              "~&~2T(~{~A~^ ~})~%"
                                              lst)
                                     (rec trc)))))
    (rec stack-trace)))


;;; Definition
(defdata result
  (success t)
  (failure stack-trace))

(defun run-result (result)
  (ematch result
    ((success val) val)
    ((failure trc) (run-stack-trace trc))))

(defmacro ensure-result (form)
  (let ((g!c (gensym)))
    `(handler-case ,form
       (condition (,g!c) (failure (strace ',form (condit ,g!c))))
       (:no-error (,g!c) (success ,g!c)))))

(define-condition result-empty-condition (condition) ())


;;; Monad+
(defmethod unit ((class (eql 'result)) a)
  (success a))

(define-fmap-by-monad result)

(define-amap-by-monad result)

(defmethod mmap (a->b* (a* result))
  (ematch a*
    ((success a)   (funcall a->b* a))
    ((failure trc) (failure (strace (list 'mmap a->b* "#FAILURE#") trc)))))

(defmethod mzero ((class (eql 'result)))
  (failure (condit (make-condition 'result-empty-condition))))

(defmethod mplus ((monoid1 result) (monoid2 result))
  (ematch (cons monoid1 monoid2)
    ((cons (success a1) (success a2)) (success (mplus a1 a2)))
    ((cons (success a1) _)            monoid1)
    ((cons _ (success a2))            monoid2)
    ((cons (failure _) (failure _))   monoid2)
    )
  )
