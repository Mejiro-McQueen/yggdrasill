(ql:quickload "fiveam")
(in-package :cl-user)
(defpackage bifrost-yggdrasill-test 
  (:use :cl
		:filesystem-hash-table
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
  `(let* ((TEST (make-filesystem-hash-table t))
		  (SPICA (make-filesystem-hash-table))
		  (SPICA-1 (make-filesystem-hash-table))
		  (SPICA-2 (make-filesystem-hash-table))
		  (SPICA-Injured (make-filesystem-hash-table))
		  (SPICA-Tenno-Sho (make-filesystem-hash-table))
		  (SPICA-Tenno-Sho-Spring (make-filesystem-hash-table))
		  (SPICA-Tenno-Sho-Autumn (make-filesystem-hash-table))
		  (SYMBOLI (make-filesystem-hash-table))
		  (MEJIRO (make-filesystem-hash-table))
		  (MEJIRO-1 (make-filesystem-hash-table))
		  (MEJIRO-2 (make-filesystem-hash-table))
		  (MEJIRO-3 (make-filesystem-hash-table)))

										;TEST
	 (add-unique-key '|Admire| 'VEGA TEST)
	 (add-unique-key '|Machikane| 'TANEHAUSER TEST)
	 (add-unique-key '|Manhattan| 'CAFE TEST)
	   
										;SPICA
	 (register-filesystem-hash-table TEST SPICA 'SPICA)
	 (add-unique-key '|SPICA-Tenno-Sho| SPICA-Tenno-Sho SPICA)
	
										; SPICA-1
	 (register-filesystem-hash-table SPICA SPICA-1 'SPICA-1)
	 (add-unique-key '|Special| 'WEEK SPICA-1)
	 (add-unique-key '|Silence| 'SUZUKA SPICA-1)
	 (add-unique-key '|Mejiro| 'MCQUEEN SPICA-1)

										; SPICA-2
	 (register-filesystem-hash-table SPICA SPICA-2 'SPICA-2)
	 (add-unique-key '|Vodka| 'VODKA SPICA-2)
	 (add-unique-key '|Gold|  'SHIP SPICA-2)
	 (add-unique-key '|Daiwa| 'SCARLET SPICA-2)
	 (add-unique-key '|Tokai| 'TEIO SPICA-2)

										;SPICA-Injured (Disconnected-Table)
	 (add-unique-key' |Tokai| '|Teio-Injured| SPICA-Injured)
	 (add-unique-key' |Mejiro| '|McQueen-Injured| SPICA-Injured)

										;SPICA-Tenno-Sho
	 (register-filesystem-hash-table SPICA SPICA-Tenno-Sho 'SPICA-Tenno-Sho)
	 (add-unique-key '|SPICA-Tenno-Sho-Spring| SPICA-Tenno-Sho-Spring Spica-Tenno-Sho)
	 (add-unique-key '|SPICA-Tenno-Sho-Autumn| SPICA-Tenno-Sho-Autumn Spica-Tenno-Sho)
	 (add-unique-key '|Vodka| '|Vodka-Contender| Spica-Tenno-Sho)
	 (add-unique-key '|Mejiro|  '|Mejiro-McQueen-Contender| Spica-Tenno-Sho)
	 (add-unique-key '|Gold|  '|Gold-Ship-Contender| Spica-Tenno-Sho)
	 (add-unique-key '|Daiwa| '|Daiwa-Scarlet-Contender| Spica-Tenno-Sho)

										;SPICA-Tenno-Sho-Spring
	 (register-filesystem-hash-table SPICA-Tenno-Sho SPICA-Tenno-Sho-Spring 'SPICA-Tenno-Sho-Spring)
	 (add-unique-key '|Tokai| '|Teio-Spring-Champion| SPICA-Tenno-Sho-Spring)
	 (add-unique-key '|Mejiro| '|Mejiro-McQueen-Spring-Champion| SPICA-Tenno-Sho-Spring)
	 (add-unique-key '|Gold| '|Ship-Spring-Champion| SPICA-Tenno-Sho-Spring)

										;SPICA-Tenno-Sho-Autumn
	 (register-filesystem-hash-table SPICA-Tenno-Sho SPICA-Tenno-Sho-Autumn 'SPICA-Tenno-Sho-Autumn)
	 (add-unique-key '|Vodka| '|Vodka-Autumn-Champion| SPICA-Tenno-Sho-Autumn)
	 (add-unique-key '|Mejiro| '|Mejiro-McQueen-Autumn-Champion| SPICA-Tenno-Sho-Autumn)
	 (add-unique-key '|Gold| '|Gold-Ship-Autumn-Champion| SPICA-Tenno-Sho-Autumn)
	 
										;SYMBOLI
	 (register-filesystem-hash-table TEST SYMBOLI 'SYMBOLI)
	 (add-unique-key '|KRIS| 'KRIS SYMBOLI)
	 (add-unique-key '|RUDOLF| '|Symboli-Rudolf| SYMBOLI)
	 (add-unique-key '|SIRIUS| 'SIRIUS SYMBOLI)
	   
										;MEJIRO
	 (register-filesystem-hash-table TEST MEJIRO 'MEJIRO)
	 (add-unique-key '|McQueen| '|Mejiro-McQueen| MEJIRO)
	 (add-unique-key '|Palmer| 'PALMER MEJIRO)
	 (add-unique-key '|Ramonu| 'RAMONU MEJIRO)
	   
										;MEJIRO-1
	 (register-filesystem-hash-table MEJIRO MEJIRO-1 'MEJIRO-1)
	 (add-unique-key '|Ardan| 'ARDAN  MEJIRO-1)
	 (add-unique-key '|Bright| 'BRIGHT  MEJIRO-1)
	 
										;MEJIRO-2
	 (register-filesystem-hash-table MEJIRO MEJIRO-2 'MEJIRO-2)
	 (add-unique-key'|Dober| 'DOBER MEJIRO-2)
	   
										;MEJIRO-3
	 (register-filesystem-hash-table MEJIRO MEJIRO-3 'MEJIRO-3)
	 (add-unique-key'|Ryan| 'RYAN MEJIRO-3)
	 (progn ,@body)))

(test find-key-in-current-map
  "User wants to find an absolute key in a subpath"
  (with-fixture-hash-table-tree
	(is (equal (find-key-by-path "/TEST/SPICA/SPICA-1/Special" TEST TEST) 'Week))
	(is (equal (find-key-by-path "/TEST/SPICA/SPICA-1/Special" SPICA TEST) 'Week))
	(is (equal (find-key-by-path "/TEST/SPICA/SPICA-1/Special" SYMBOLI TEST) 'Week))
	(is (equal (find-key-by-path "/TEST/SPICA/SPICA-2/Daiwa" SYMBOLI TEST) 'Scarlet))
	(is (equal (find-key-by-path "/TEST/SPICA/SPICA-2/Daiwa" Mejiro-3 TEST) 'Scarlet))
	(is (equal (find-key-by-path "/TEST/Admire" SPICA TEST) 'Vega))
	(is (equal (find-key-by-path "/TEST/Admire" SYMBOLI TEST) 'Vega))
	(is (equal (find-key-by-path "/TEST/SPICA/SPICA-Tenno-Sho/SPICA-Tenno-Sho-Spring/Mejiro" SYMBOLI TEST) '|Mejiro-McQueen-Spring-Champion|))
	(is (equal (find-key-by-path "/TEST/SPICA/SPICA-Tenno-Sho/SPICA-Tenno-Sho-Spring/Mejiro" SPICA-1 TEST) '|Mejiro-McQueen-Spring-Champion|))
	))

(test find-key-in-relative-path
  "User wants to find a relative key"
  (with-fixture-hash-table-tree
	(is (equal (find-key-by-path "../McQueen" MEJIRO-1 TEST) '|Mejiro-McQueen|))
	(is (equal (find-key-by-path "McQueen" MEJIRO TEST) '|Mejiro-McQueen|))
	(is (equal (find-key-by-path "./McQueen" MEJIRO TEST) '|Mejiro-McQueen|))
	(is (equal (find-key-by-path "../../SPICA-1/Mejiro" SPICA-Tenno-Sho-Spring TEST) 'MCQUEEN))
	(is (equal (find-key-by-path "../../SPICA/SPICA-Tenno-Sho/SPICA-Tenno-Sho-Spring/Mejiro" MEJIRO-1 TEST) '|Mejiro-McQueen-Spring-Champion|))
	(is (equal (find-key-by-path "../../SPICA/SPICA-Tenno-Sho/SPICA-Tenno-Sho-Spring/Narita" MEJIRO-1 TEST) nil))
	(is (equal (find-key-by-path "../../../SYMBOLI/RUDOLF" SPICA-Tenno-Sho-Autumn TEST) '|Symboli-Rudolf|))
	))


(test user-back-tracks-beyond-root
  "User wants to find a key, but the table path is disconnected"
  (with-fixture-hash-table-tree
	(is (equal (find-key-by-path "../../../../../../../../../../McQueen" MEJIRO-1 TEST) nil))
	(signals warn (find-key-by-path "../../McQueen" MEJIRO-1 TEST) '|Mejiro-McQueen|)
	))
  

(test find-key-in-disconnected-path
  "User wants to find a key, but the table path is disconnected"
  (is (equal (find-key-by-path "../../McQueen" MEJIRO-1 TEST) '|Mejiro-McQueen|))
  

  )
  

; Cached results per space system?

