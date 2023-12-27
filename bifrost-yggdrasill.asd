(defsystem "bifrost-yggdrasill"
  :version "0.1.0"
  :author "Adrian Vazquez"
  :license "MIT"
  :depends-on (#:cxml #:alexandria #:filesystem-hash-table #:log4cl #:lparallel)
  :components ((:file "packages")
			   (:module "src"
				:components ((:file "main"))
				:depends-on ("packages" "src/xtce" "src/xtce-engine"))
			   (:module "src/xtce"
				:components ((:file "xtce")
							 (:file "utils"))
				:depends-on ("packages"))
			   (:module "src/xtce-engine"
				:depends-on ("packages" "src/xtce")
				:components ((:file "xtce-engine")
							 (:file "standard-template-constructs")
							 (:file "aos")
							 (:file "space-packet")
							 (:file "mpdu")
							 (:file "binops")
							 (:file "decoding")
							 (:file "encoding")
							 (:file "frame-service")
							 (:file "dev")
							 )))
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

