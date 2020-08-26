(defsystem "fcl"
  :version "0.1.0"
  :author "ekatsym"
  :license "LLGPL"
  :components ((:module "src"
                :components
                (;; Core
                 (:file "package")
                 (:file "util"        :depends-on ("package"))
                 (:file "lazy"        :depends-on ("package" "util"))
                 (:file "defdata"     :depends-on ("package" "util" "lazy"))
                 (:file "foldable"    :depends-on ("package" "util"))
                 (:file "functor"     :depends-on ("package" "util"))
                 (:file "applicative" :depends-on ("package" "util" "functor"))
                 (:file "monad"       :depends-on ("package" "util" "applicative"))
                 (:file "monoid"      :depends-on ("package" "util"))
                 (:file "monad-plus"  :depends-on ("package" "util" "monad" "monoid"))

                 ;; Datatypes
                 (:module "datatypes"
                  :depends-on ("package" "util" "lazy" "defdata" "foldable"
                               "functor" "applicative" "monad" "monoid" "monad-plus")
                  :components
                  ((:file "list")
                   (:file "lazy-list")
                   (:file "maybe")
                   (:file "either")
                   (:file "reader")
                   (:file "writer")
                   (:file "state"))))))
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
