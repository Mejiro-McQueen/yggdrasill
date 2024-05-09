(ql:quickload "fiveam")
(ql:quickload "filesystem-hash-table")
(defpackage filesystem-hash-table-test 
  (:use :cl
		:filesystem-hash-table
        :fiveam))

(in-package filesystem-hash-table-test)

(setf fiveam:*run-test-when-defined* t)
(setf fiveam:*on-failure* :debug)

(def-suite find-xtce-key
  :description "Test my system")

(in-suite find-xtce-key)

(defmacro with-fixture-hash-table-tree (&body body)
  `(let* ((Tracen (make-filesystem-hash-table :root t))
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

										;Tracen
	 (add-unique-key "Admire" 'VEGA Tracen)
	 (add-unique-key "Machikane" 'TANEHAUSER Tracen)
	 (add-unique-key "Manhattan" 'CAFE Tracen)
	   
										;SPICA
	 (link-filesystem-hash-tables Tracen SPICA "SPICA")
	
										; SPICA-1
	 (link-filesystem-hash-tables SPICA SPICA-1 "SPICA-1")
	 (add-unique-key "Special" 'WEEK SPICA-1)
	 (add-unique-key "Silence" 'SUZUKA SPICA-1)
	 (add-unique-key "Mejiro" 'MCQUEEN SPICA-1)

										; SPICA-2
	 (link-filesystem-hash-tables SPICA SPICA-2 "SPICA-2")
	 (add-unique-key "Vodka" 'VODKA SPICA-2)
	 (add-unique-key "Gold"  'SHIP SPICA-2)
	 (add-unique-key "Daiwa" 'SCARLET SPICA-2)
	 (add-unique-key "Tokai" 'TEIO SPICA-2)

										;SPICA-Injured (Disconnected-Table)
	 (add-unique-key' "Tokai" "Teio-Injured" SPICA-Injured)
	 (add-unique-key' "Mejiro" "McQueen-Injured" SPICA-Injured)

										;SPICA-Tenno-Sho
	 (link-filesystem-hash-tables SPICA SPICA-Tenno-Sho "SPICA-Tenno-Sho")
	 (add-unique-key "Vodka" "Vodka-Contender" Spica-Tenno-Sho)
	 (add-unique-key "Mejiro" "Mejiro-McQueen-Contender" Spica-Tenno-Sho)
	 (add-unique-key "Gold" "Gold-Ship-Contender" Spica-Tenno-Sho)
	 (add-unique-key "Daiwa" "Daiwa-Scarlet-Contender" Spica-Tenno-Sho)

										;SPICA-Tenno-Sho-Spring
	 (link-filesystem-hash-tables SPICA-Tenno-Sho SPICA-Tenno-Sho-Spring "SPICA-Tenno-Sho-Spring")
	 (add-unique-key "Tokai" "Teio-Spring-Champion" SPICA-Tenno-Sho-Spring)
	 (add-unique-key "Mejiro" "Mejiro-McQueen-Spring-Champion" SPICA-Tenno-Sho-Spring)
	 (add-unique-key "Gold" "Ship-Spring-Champion" SPICA-Tenno-Sho-Spring)

										;SPICA-Tenno-Sho-Autumn
	 (link-filesystem-hash-tables SPICA-Tenno-Sho SPICA-Tenno-Sho-Autumn "SPICA-Tenno-Sho-Autumn")
	 (add-unique-key "Vodka" "Vodka-Autumn-Champion" SPICA-Tenno-Sho-Autumn)
	 (add-unique-key "Mejiro" "Mejiro-McQueen-Autumn-Champion" SPICA-Tenno-Sho-Autumn)
	 (add-unique-key "Gold" "Gold-Ship-Autumn-Champion" SPICA-Tenno-Sho-Autumn)
	 
										;SYMBOLI
	 (link-filesystem-hash-tables Tracen SYMBOLI "SYMBOLI")
	 (add-unique-key "KRIS" 'KRIS SYMBOLI)
	 (add-unique-key "RUDOLF" "Symboli-Rudolf" SYMBOLI)
	 (add-unique-key "SIRIUS" 'SIRIUS SYMBOLI)
	   
										;MEJIRO
	 (link-filesystem-hash-tables Tracen MEJIRO "MEJIRO")
	 (add-unique-key "McQueen" "Mejiro-McQueen" MEJIRO)
	 (add-unique-key "Palmer" 'PALMER MEJIRO)
	 (add-unique-key "Ramonu" 'RAMONU MEJIRO)
	   
										;MEJIRO-1
	 (link-filesystem-hash-tables MEJIRO MEJIRO-1 "MEJIRO-1")
	 (add-unique-key "Ardan" 'ARDAN  MEJIRO-1)
	 (add-unique-key "Bright" 'BRIGHT  MEJIRO-1)
	 
										;MEJIRO-2
	 (link-filesystem-hash-tables MEJIRO MEJIRO-2 "MEJIRO-2")
	 (add-unique-key "Dober" 'DOBER MEJIRO-2)
	   
										;MEJIRO-3
	 (link-filesystem-hash-tables MEJIRO MEJIRO-3 "MEJIRO-3")
	 (add-unique-key "Ryan" 'RYAN MEJIRO-3)
	 (progn ,@body)))

(with-fixture-hash-table-tree
  Tracen
  )

(test find-key-in-current-map
  "User wants to find an absolute key in a subpath"
  (with-fixture-hash-table-tree
	(is (equal (find-key-by-path "/Tracen/SPICA/SPICA-1/Special" Tracen) 'Week))
	(is (equal (find-key-by-path "/Tracen/SPICA/SPICA-1/Special" SPICA) 'Week))
	(is (equal (find-key-by-path "/Tracen/SPICA/SPICA-1/Special" SYMBOLI) 'Week))
	(is (equal (find-key-by-path "/Tracen/SPICA/SPICA-2/Daiwa" SYMBOLI) 'Scarlet))
	(is (equal (find-key-by-path "/Tracen/SPICA/SPICA-2/Daiwa" Mejiro-3) 'Scarlet))
	(is (equal (find-key-by-path "/Tracen/Admire" SPICA) 'Vega))
	(is (equal (find-key-by-path "/Tracen/Admire" SYMBOLI) 'Vega))
	(is (equal (find-key-by-path "/Tracen/SPICA/SPICA-Tenno-Sho/SPICA-Tenno-Sho-Spring/Mejiro" SYMBOLI) "Mejiro-McQueen-Spring-Champion"))
	(is (equal (find-key-by-path "/Tracen/SPICA/SPICA-Tenno-Sho/SPICA-Tenno-Sho-Spring/Mejiro" SPICA-1) "Mejiro-McQueen-Spring-Champion"))
	))

(test find-key-in-relative-path
  "User wants to find a relative key"
  (with-fixture-hash-table-tree
	(is (equal (find-key-by-path "../McQueen" MEJIRO-1) "Mejiro-McQueen"))
	(is (equal (find-key-by-path "McQueen" MEJIRO) "Mejiro-McQueen"))
	(is (equal (find-key-by-path "./McQueen" MEJIRO) "Mejiro-McQueen"))
	(is (equal (find-key-by-path "../../SPICA-1/Mejiro" SPICA-Tenno-Sho-Spring) 'MCQUEEN))
	(is (equal (find-key-by-path "../../SPICA/SPICA-Tenno-Sho/SPICA-Tenno-Sho-Spring/Mejiro" MEJIRO-1) "Mejiro-McQueen-Spring-Champion"))
	(is (equal (find-key-by-path "../../SPICA/SPICA-Tenno-Sho/SPICA-Tenno-Sho-Spring/Narita" MEJIRO-1) nil))
	(is (equal (find-key-by-path "../../../SYMBOLI/RUDOLF" SPICA-Tenno-Sho-Autumn) "Symboli-Rudolf"))
	))


(test user-back-tracks-beyond-root
  "User wants to find a key, but the table path is disconnected"
  (with-fixture-hash-table-tree
	(is (equal (find-key-by-path "../../../../../../../../../../McQueen" MEJIRO-1) nil))
	;(signals warn (find-key-by-path "../../McQueen" MEJIRO-1) "Mejiro-McQueen")
	))
  
(test find-key-in-disconnected-path
  (with-fixture-hash-table-tree
	"User wants to find a key, but the table path is disconnected"
	(is (equal (find-key-by-path "../../Daiwa" SPICA-Injured) nil))))

(with-fixture-hash-table-tree
	(alexandria:hash-table-keys Tracen))
