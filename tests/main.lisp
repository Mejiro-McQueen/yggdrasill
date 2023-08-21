(ql:quickload "fiveam")
(in-package :cl-user)
(defpackage bifrost-yggdrasill-test 
  (:use :cl
		:xtce
        :fiveam))
(in-package :bifrost-yggdrasill-test)

(setf fiveam:*run-test-when-defined* t)
(setf fiveam:*on-failure* :debug)

(def-suite my-system
  :description "Test my system")

;; Define a suite and set it as the default for the following tests.
(def-suite read-file-as-string
  :description "Test the read-file-as-string function."
  :in my-system)

(in-suite read-file-as-string)

(test read-file-as-string-normal-file
  (let ((result (read-file-as-string "/tmp/hello.txt")))
    ;; Tip: put the expected value as the first argument of = or equal, string= etc.
    ;; FiveAM generates a more readable report following this convention.
    (is (string= "hello" result))))

;; We read an empty file.
(test read-file-as-string-empty-file
  (let ((result (read-file-as-string "/tmp/empty.txt")))
    (is (not (null result)))
    ;; The reason can be used to provide formatted text.
    (is (= 0 (length result)))
        "Empty string expected but got ~a" result))

;; Now we test that reading a non-existing file signals our condition.
(test read-file-as-string-non-existing-file
  (let ((result (read-file-as-string "/tmp/non-existing-file.txt"
                                     :error-if-not-exists nil)))
    (is (null result)
      "Reading a file should return NIL when :ERROR-IF-NOT-EXISTS is set to NIL"))
  ;; SIGNALS accepts the unquoted name of a condition and a body to evaluate.
  ;; Here it checks if FILE-NOT-EXISTING-ERROR is signaled.
  (signals file-not-existing-error
    (read-file-as-string "/tmp/non-existing-file.txt"
                         :error-if-not-exists t)))


;; (setf *TEST* #x1acffc1dFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA)
;; (print-hex (look-for-pattern *TEST*
;; 							 (make-sync-pattern #x1acffc1d (integer-length #x1acffc1d))))


;;;;;
(def-suite find-xtce-key
  :description "Test my system")

(in-suite find-xtce-key)
(defmacro with-fixture-hash-table-tree (&body body)
  `(let* ((TEST (make-hash-table :test 'equalp))
		  (SPICA (make-hash-table :test 'equalp))
		  (SPICA-1 (make-hash-table :test 'equalp))
		  (SPICA-2 (make-hash-table :test 'equalp))
		  (SPICA-Tenneshou (make-hash-table :test 'equalp))
		  (SPICA-Tenneshou-Spring (make-hash-table :test 'equalp))
		  (SPICA-Tenneshou-Autumn (make-hash-table :test 'equalp))
		  (SYMBOLI (make-hash-table :test 'equalp))
		  (MEJIRO (make-hash-table :test 'equalp))
		  (MEJIRO-1 (make-hash-table :test 'equalp))
		  (MEJIRO-2 (make-hash-table :test 'equalp))
		  (MEJIRO-3 (make-hash-table :test 'equalp)))

										;TEST
	 (setf (gethash '|Admire| TEST) 'VEGA)
	 (setf (gethash '|Machikane| TEST) 'TANEHAUSER)
	 (setf (gethash '|Manhattan| TEST) 'CAFE)
	 (setf (gethash 'SPICA TEST) SPICA)
	 (setf (gethash 'SYMBOLI TEST) SYMBOLI)
	 (setf (gethash 'MEJIRO TEST) MEJIRO)
	   
										;SPICA
	 (setf (gethash 'PARENT SPICA) TEST)
	 (setf (gethash 'SPICA-1 SPICA) SPICA-1)
	 (setf (gethash 'SPICA-2 SPICA) SPICA-2)
	
										; SPICA/1
	 (setf (gethash 'PARENT SPICA-1) SPICA )
	 (setf (gethash '|Special| SPICA-1) 'WEEK)
	 (setf (gethash '|Silence| SPICA-1) 'SUZUKA)
	 (setf (gethash '|Mejiro| SPICA-1) 'MCQUEEN)

										; SPICA/2
	 (setf (gethash 'PARENT SPICA-2) SPICA)
	 (setf (gethash '|Vodka| SPICA-2) 'VODKA)
	 (setf (gethash '|Gold| SPICA-2) 'SHIP)
	 (setf (gethash '|Daiwa| SPICA-2) 'SCARLET)
	 (setf (gethash '|Tokai| SPICA-2) 'TEIO)

										;SYMBOLI

	 (setf (gethash 'PARENT SYMBOLI) TEST)
	 (setf (gethash '|KRIS| SYMBOLI) 'KRIS)
	 (setf (gethash '|RUDOLF| SYMBOLI) 'RUDOLF)
	 (setf (gethash '|SIRIUS| SYMBOLI) 'SIRIUS)
	   
										;MEJIRO
	 (setf (gethash 'PARENT MEJIRO) TEST)
	 (setf (gethash 'MEJIRO-1 MEJIRO) MEJIRO-1)
	 (setf (gethash 'MEJIRO-2 MEJIRO) MEJIRO-2)
	 (setf (gethash 'MEJIRO-3 MEJIRO) MEJIRO-3)
	 (setf (gethash '|McQueen| MEJIRO) 'MCQUEEN)
	 (setf (gethash '|Palmer| MEJIRO) 'PALMER)
	 (setf (gethash '|Ramonu| MEJIRO) 'RAMONU)
	   
										;MEJIRO/1
	 (setf (gethash 'PARENT MEJIRO-1) MEJIRO)
	 (setf (gethash '|Ardan| MEJIRO-1) 'ARDAN)
	 (setf (gethash '|Bright| MEJIRO-1) 'BRIGHT)
	 
										;MEJIRO/2
	 (setf (gethash 'PARENT MEJIRO-2) MEJIRO)
	 (setf (gethash '|Dober| MEJIRO-2) 'DOBER)
	   
										;MEJIRO/3
	 (setf (gethash 'PARENT MEJIRO-3) MEJIRO)
	 (setf (gethash '|Ryan| MEJIRO-3) 'RYAN)
	 (progn ,@body)))

(test find-key-in-current-map
  "User wants to find an absolute key in a subpath"
  (with-fixture-hash-table-tree
	(is (equal (find-xtce-key "/TEST/SPICA/SPICA-1/Mejiro" TEST TEST) 'McQueen))
	(is (equal (find-xtce-key "/TEST/SPICA/SPICA-1/Mejiro" SPICA TEST) 'McQueen))
	(is (equal (find-xtce-key "/TEST/Admire" SPICA TEST) 'Vega))
	))

(test find-key-in-relative-path
  "User wants to find a relative key"
  (with-fixture-hash-table-tree
	(print (gethash 'PARENT MEJIRO-1))
	(is (equal (find-xtce-key "../McQueen" MEJIRO-1 TEST) 'McQueen))
	(is (equal (find-xtce-key "McQueen" MEJIRO TEST) 'McQueen))
	(is (equal (find-xtce-key "./McQueen" MEJIRO TEST) 'McQueen))))
