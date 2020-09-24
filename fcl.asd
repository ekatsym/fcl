(defsystem "fcl"
  :version "0.2.0"
  :author "ekatsym"
  :license "LLGPL"
  :components ((:module "src"
                :components
                ((:module "util"
                  :components
                  ((:file "type")
                   (:file "list"        :depends-on ("type"))
                   (:file "function"    :depends-on ("list"))
                   (:file "symbol")
                   (:file "package"     :depends-on ("type" "list" "function" "symbol"))))
                 (:module "lazy"
                  :depends-on ("util")
                  :components
                  ((:file "core")
                   (:file "package")))
                 (:module "data"
                  :depends-on ("util" "lazy")
                  :components
                  ((:file "util")
                   (:file "parser")
                   (:file "core")
                   (:file "package")))
                 (:module "match"
                  :depends-on ("util" "data" "lazy")
                  :components
                  ((:file "util")
                   (:file "parser")
                   (:file "core")
                   (:file "package")))
                 (:module "datatypes"
                  :depends-on ("lazy" "data")
                  :components
                  ((:file "maybe")
                   (:file "either")
                   (:file "llist")
                   (:file "reader")
                   (:file "writer")
                   (:file "state")))
                 (:module "generics"
                  :depends-on ("util")
                  :components
                  ((:file "functor")
                   (:file "applicative" :depends-on ("functor"))
                   (:file "monad"       :depends-on ("applicative"))
                   (:file "monoid")
                   (:file "monad-plus"  :depends-on ("monad" "monoid"))
                   (:file "recursive"   :depends-on ("functor"))
                   (:file "iterable"    :depends-on ("recursive"))
                   (:file "foldable"    :depends-on ("recursive"))
                   (:file "traversable" :depends-on ("recursive"))))
                 (:module "methods"
                  :depends-on ("util" "datatypes" "generics")
                  :components
                  ((:file "list")
                   (:file "vector")
                   (:file "array")
                   (:file "maybe")
                   (:file "either")
                   (:file "llist")
                   (:file "reader")
                   (:file "writer")
                   (:file "state")))
                 (:file "package"))))
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
