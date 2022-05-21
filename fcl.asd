(defsystem "fcl"
  :version "0.2.0"
  :author "ekatsym"
  :license "LLGPL"
  :components ((:module "src"
                :components
                ((:file "package"       :depends-on ("core" "generics" "datatype"))
                 (:module "core"        :depends-on ("util")
                  :components
                  ((:file "lazy")
                   (:module "adata"       :depends-on ("lazy")
                    :components
                    ((:file "main"          :depends-on ("util" "parser"))
                     (:file "parser"        :depends-on ("util"))
                     (:file "util")))
                   (:module "match"       :depends-on ("lazy" "adata")
                    :components
                    ((:file "main"          :depends-on ("util" "parser"))
                     (:file "parser"        :depends-on ("util"))
                     (:file "util")))))
                 (:module "generics"    :depends-on ("util" "core")
                  :components
                  ((:file "functor")
                   (:file "applicative"   :depends-on ("functor"))
                   (:file "monad"         :depends-on ("applicative"))
                   (:file "monoid")
                   (:file "monad-plus"    :depends-on ("monad" "monoid"))
                   (:file "foldable")
                   (:file "unfoldable")))
                 (:module "datatype"    :depends-on ("util" "core" "generics")
                  :components
                  ((:file "promise")
                   (:file "maybe")
                   (:file "either")
                   (:file "list")
                   (:file "vector")
                   (:file "array")
                   (:file "function")
                   (:file "reader")
                   (:file "writer")
                   (:file "state")
                   (:file "result")
                   (:file "io")
                   (:file "queue"         :depends-on ("list"))
                   (:file "llist"         :depends-on ("list"))))
                 (:module "util"
                  :components
                  ((:file "package")
                   (:file "type"          :depends-on ("package"))
                   (:file "list"          :depends-on ("package" "type"))
                   (:file "function"      :depends-on ("package" "list"))
                   (:file "symbol"        :depends-on ("package")))))))
  :description ""
  :in-order-to ((test-op (test-op "fcl/tests"))))

(defsystem "fcl/tests"
  :author "ekatsym"
  :license "LLGPL"
  :depends-on ("fcl" "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main")
                 (:file "util")
                 (:module "generics"
                  :components
                  ((:file "functor")
                   (:file "applicative"   :depends-on ("functor"))
                   (:file "monad"         :depends-on ("applicative"))
                   (:file "monoid")
                   (:file "foldable")))
                 (:module "datatype"    :depends-on ("main" "generics")
                  :components
                  ((:file "promise")
                   (:file "maybe")
                   (:file "either")
                 ;  (:file "list")
                 ;  (:file "vector")
                 ;  (:file "queue")
                 ;  (:file "package"       :depends-on ("maybe" "either"))
                 )))))
  :description "Test system for fcl"
  :perform (test-op (o c) (symbol-call :fiveam :run! :fcl/tests)))
