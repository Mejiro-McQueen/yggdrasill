(defsystem "bifrost-yggdrasill"
  :version "0.1.0"
  :author "Adrian Vazquez"
  :license "MIT"
  :depends-on ("cxml" "alexandria" "uiop")
  :components ((:module "src"
                :components
                ((:file "xtce")
				 )))
  :description ""
  :in-order-to ((test-op (test-op "bifrost-integral/tests"))))

(defsystem "bifrost-yggdrasill/tests"
  :author "Adrian Vazquez"
  :license "MIT"
  :depends-on ("bifrost-yggdrasill"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for bifrost-yggdrasill"
  :perform (test-op (op c) (symbol-call :rove :run c)))
