(defsystem "bifrost-integral"
  :version "0.1.0"
  :author "Adrian Vazquez"
  :license "MIT"
  :depends-on ("cxml" "alexandria" "uiop")
  :components ((:module "src"
                :components
                ((:file "xtce")
                 (:file "encodings")
                 (:file "calibrators")
                 (:file "parameter-types")
				 )))
  :description ""
  :in-order-to ((test-op (test-op "bifrost-integral/tests"))))

(defsystem "bifrost-integral/tests"
  :author "Adrian Vazquez"
  :license "MIT"
  :depends-on ("bifrost-integral"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for bifrost-integral"
  :perform (test-op (op c) (symbol-call :rove :run c)))
