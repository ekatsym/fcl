(defsystem "fcl-core"
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
                 (:file "monad-plus"  :depends-on ("package" "util" "monad" "monoid")))))
  :description ""
  :in-order-to ((test-op (test-op "fcl/tests"))))
