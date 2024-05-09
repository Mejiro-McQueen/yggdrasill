(defsystem "yggdrasill"
  :version "0.1.0"
  :author "Adrian Vazquez"
  :license "MIT"
  :depends-on (#:cxml #:alexandria #:log4cl #:lparallel #:serapeum #:websocket-driver #:clack :ironclad)
  :components ((:file "packages")
			   (:module "src"
				:components ((:file "main"))
				:depends-on ("packages" "src/xtce" "src/xtce-engine"))
			   (:module "src/xtce"
				:components ((:file "xtce")
							 (:file "utils")
							 (:file "filesystem-hash-table"))
				:depends-on ("packages"))
			   (:module "src/xtce-engine"
				:depends-on ("packages" "src/xtce")
				:components ((:file "xtce-engine")
							 (:file "decoding")
							 (:file "standard-template-constructs")
							 (:file "aos")
							 (:file "space-packet")
							 (:file "mpdu")
							 (:file "binops")
							 (:file "encoding")
							 (:file "frame-service")
							 (:file "dev"))))
  :description ""
  :in-order-to ((test-op (test-op "yggdrasill/tests"))))

(defsystem "yggdrasill/tests"
  :author "Adrian Vazquez"
  :license "MIT"
  :depends-on ("yggdrasill"
               "fiveam")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for yggdrasill"
  :perform (test-op (op c)
                    (symbol-call :fiveam :run!
                                 (find-symbol* :yggdrasill  :yggdrasill/test))))
