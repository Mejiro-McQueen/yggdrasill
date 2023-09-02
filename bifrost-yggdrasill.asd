(defsystem "bifrost-yggdrasill"
  :version "0.1.0"
  :author "Adrian Vazquez"
  :license "MIT"
  :depends-on (#:cxml #:alexandria #:filesystem-hash-table)
  :components ((:file "packages")
			   (:module "src"
				:components ((:file "utils")))
			   (:module "src/xtce"
				:components ((:file "xtce"))
				:depends-on ("packages" "src"))
			   (:module "src/xtce-engine"
				:depends-on ("packages" "src" "src/xtce")
				:components ((:file "xtce-engine")
							 (:file "standard-template-constructs"))))
  :description ""
  :in-order-to ((test-op (test-op "bifrost-integral/tests"))))

(defsystem "bifrost-yggdrasill/tests"
  :author "Adrian Vazquez"
  :license "MIT"
  :depends-on ("bifrost-yggdrasill"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for bifrost-yggdrasill"
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :bifrost-yggdrasill  :bifrost-yggdrasill/test))))


