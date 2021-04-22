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

                 (:module "generics"
                  :depends-on ("util")
                  :components
                  ((:file "functor")
                   (:file "applicative" :depends-on ("functor"))
                   (:file "monad"       :depends-on ("applicative"))
                   (:file "monoid")
                   (:file "monad-plus"  :depends-on ("monad" "monoid"))
                   (:file "recursive")
                   (:file "foldable"    :depends-on ("recursive"))))

                 (:module "datatypes"
                  :depends-on ("util" "lazy" "data" "match" "generics")
                  :components
                  ((:file "promise")
                   (:file "maybe")
                   (:file "either")
                   (:file "list"        :depends-on ("maybe" "either"))
                   (:file "vector"      :depends-on ("maybe" "either"))
                   (:file "array"       :depends-on ("maybe" "either"))
                   (:file "llist"       :depends-on ("maybe" "either"))
                   (:file "queue"       :depends-on ("maybe" "either" "llist"))
                   (:file "bheap"       :depends-on ("maybe" "either" "llist"))
                   (:file "function")
                   (:file "reader")
                   (:file "writer"      :depends-on ("queue"))
                   (:file "state")))

                 (:file "package"
                  :depends-on ("lazy" "data" "match" "generics" "datatypes")))))
  :description ""
  :in-order-to ((test-op (test-op "fcl/tests"))))

(defsystem "fcl/tests"
  :author "ekatsym"
  :license "LLGPL"
  :depends-on ("fcl" "rove")
  :components ((:module "tests"
                        :components
                        ((:module "datatypes"
                          :components
                          ((:file "maybe")
                           (:file "either")
                           (:file "package" :depends-on ("maybe" "either"))))
                         (:file "package" :depends-on ("datatypes")))))
  :description "Test system for fcl"
  :perform (test-op (op c) (symbol-call :rove :run c)))
