(defsystem "fcl"
  :version "0.1.0"
  :author "ekatsym"
  :license "LLGPL"
  :components ((:module "src"
                :components
                (;; Core
                 (:file "package")
                 (:file "util"
                  :depends-on ("package"))
                 (:file "lazy"
                  :depends-on ("package" "util"))
                 (:file "defdata"
                  :depends-on ("package" "util" "lazy"))
                 (:file "functor"
                  :depends-on ("package" "util"))
                 (:file "applicative"
                  :depends-on ("package" "util" "functor"))
                 (:file "monad"
                  :depends-on ("package" "util" "applicative"))
                 (:file "monoid"
                  :depends-on ("package" "util"))
                 (:file "monad-plus"
                  :depends-on ("package" "util" "monad" "monoid"))
                 (:file "foldable"
                  :depends-on ("package" "util"))

                 ;; Datatypes
                 (:file "datatypes/list"
                  :depends-on ("package" "util" "monad" "monoid"))
                 (:file "datatypes/lazy-list"
                  :depends-on ("package" "util" "defdata" "monad" "monoid" "datatypes/list"))
                 (:file "datatypes/maybe"
                  :depends-on ("package" "util" "defdata" "monad" "monoid"))
                 (:file "datatypes/either"
                  :depends-on ("package" "util" "defdata" "monad" "monoid"))
                 (:file "datatypes/reader"
                  :depends-on ("package" "util" "defdata" "monad" "monoid"))
                 (:file "datatypes/writer"
                  :depends-on ("package" "util" "defdata" "monad" "monoid"))
                 (:file "datatypes/state"
                  :depends-on ("package" "util" "defdata" "monad" "monoid")))))
  :description ""
  :in-order-to ((test-op (test-op "fcl/tests"))))

(defsystem "fcl/tests"
  :author "ekatsym"
  :license "LLGPL"
  :depends-on ("fcl"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for fcl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
