(ql:quickload "fiveam")
(ql:quickload "bifrost-yggdrasill")
(in-package :cl-user)
(defpackage bifrost-yggdrasill-test 
  (:use :cl
		:filesystem-hash-table
        :fiveam
		:xtce-engine))
(in-package :bifrost-yggdrasill-test)

(setf fiveam:*run-test-when-defined* t)
(setf fiveam:*on-failure* :debug)

(def-suite mpdu-tests
  :description "MPDU Tests")

(in-suite mpdu-tests)

;; (setf *TEST* #x1acffc1dFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA)
;; (print-hex (look-for-pattern *TEST*
;; 							 (make-sync-pattern #x1acffc1d (integer-length #x1acffc1d))))

(defmacro with-AOS-TEST-1 (&body body)
  "AOS Frame with no spanning MPDU and short circuiting idle packets"
  `(let* ((AOS-TEST-HEADER (alist->bit-vector
							(list (cons 'transfer-frame-version-number #*01)
								  (cons 'spacecraft-id #*01100011) ;0x63
								  (cons 'virtual-channel-id #*101011) ;43
								  (cons 'virtual-channel-frame-count #*100101110000100010101011); 9898155
								  (cons 'replay-flag #*0)
								  (cons 'virtual-channel-frame-count-usage-flag #*1)
								  (cons 'reserved-space #*00)
								  (cons 'vc-frame-count-cycle #*1010))))

		  (test-mpdu-header (alist->bit-vector
							 (list (cons 'spare #*00000)
								   (cons 'first-header-pointer #*00000000000))))

		  (test-space-packet (alist->bit-vector
							  (list (cons 'packet-version-number  #*000)
									(cons 'packet-type #*0)
									(cons 'sec-hdr-flag #*0)
									(cons 'apid #*00000000001)
									(cons 'sequence-flags #*11)
									(cons 'sequence-count #*00001010011010)
									(cons 'data-len (uint->bit-vector (- (/ (length (uint->bit-vector #xBADC0DED)) 8) 1) 16))
									(cons 'data (uint->bit-vector #xBADC0DED)))))

		  (test-idle-packet (alist->bit-vector
							 (list (cons 'packet-version-number  #*000)
								   (cons 'packet-type #*0)
								   (cons 'sec-hdr-flag #*0)
								   (cons 'appid #*11111111111)
								   (cons 'sequence-flags #*11)
								   (cons 'sequence-count #*00001010011010)
								   (cons 'data-len (uint->bit-vector (- (/ (length (uint->bit-vector #xFFFFFFFF)) 8) 1) 16))
								   (cons 'data (uint->bit-vector #xFFFFFFFF))))))
	 ,@body
	 ))


(defmacro with-pack-frame (&body body)
  `(let* ((space-packets (concatenate-bit-arrays
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet ;30
						  test-idle-packet ;31 
						  test-idle-packet ;32
						  test-space-packet)) ;32

		  (full-frame (pad-bit-vector 
					   (concatenate-bit-arrays
						AOS-TEST-HEADER
						test-mpdu-header
						space-packets)
					   8192
					   :position :right
					   :pad-element 1))


		  (TEST-TABLE (xtce::register-keys-in-sequence
					   (stc::with-ccsds.space-packet.parameters
						   (stc::with-ccsds.space-packet.types
							   (stc::with-ccsds.space-packet.containers
								   (stc::with-ccsds.mpdu.containers
									   (stc::with-ccsds.mpdu.types
										   (stc::with-ccsds.mpdu.parameters
											   (stc::with-ccsds.aos.containers
												   (stc::with-ccsds.aos.header.parameters
													   (stc::with-ccsds.aos.header.types '())))))))))
					   (filesystem-hash-table:make-filesystem-hash-table) 'Test)))
	 ,@body
	 ))


(test AOS-decode
  "Simple decode test of AOS frame"
  (with-AOS-TEST-1
	(with-pack-frame
	;;;Types
	  (is (equal #*01 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number-Type" TEST-TABLE) TEST-TABLE '() 0)))
	  (is (equal #*01100011 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Spacecraft-Identifier-Type" TEST-TABLE) TEST-TABLE '() 2)))
	  (is (equal 43 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-ID-Type" TEST-TABLE) TEST-TABLE '() 10)))
	  (is (equal 9898155 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Type" TEST-TABLE) TEST-TABLE '() 16)))
	  (is (equal "False" (decode full-frame (gethash "STC.CCSDS.AOS.Header.Replay-Flag-Type" TEST-TABLE) TEST-TABLE '() 40)))
	  (is (equal "Interpreted" (decode full-frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag-Type" TEST-TABLE) TEST-TABLE '() 41)))
	  (is (equal #*00 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Reserved-Spare-Type" TEST-TABLE) TEST-TABLE '() 42)))
	  (is (equal 10 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle-Type" TEST-TABLE) TEST-TABLE '() 44)))
	  (is (equal (subseq full-frame (* 6 8)) ;6 octets
				 (decode full-frame (gethash "STC.CCSDS.AOS.Transfer-Frame-Data-Field-Type" TEST-TABLE) TEST-TABLE '() 48)))

	;;;Containers
	  (is (equal (list (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count| 9898155)
					   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-ID| 43)
					   (cons STC::'|STC.CCSDS.AOS.Header.Spacecraft-Identifier| #*01100011)
					   (cons STC::'|STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number| #*01)
					   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle| 10)
					   (cons STC::'|STC.CCSDS.AOS.Header.Reserved-Spare| #*00)
					   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag| "Interpreted")
					   (cons STC::'|STC.CCSDS.AOS.Header.Replay-Flag| "False"))
		   (decode full-frame (gethash "STC.CCSDS.AOS.Container.Transfer-Frame-Primary-Header" TEST-TABLE) TEST-TABLE '() 0)))

	  		  
	  (is (equal (list (cons STC::'|STC.CCSDS.AOS.Header.Spacecraft-Identifier| #*01100011)
					   (cons STC::'|STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number| #*01))
				 (decode full-frame (gethash "STC.CCSDS.AOS.Container.Transfer-Frame-Primary-Header.Master-Channel-ID" TEST-TABLE) TEST-TABLE '() 0)))

	  (is (equal (list (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count| 9898155)
					   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-ID| 43)
					   (cons STC::'|STC.CCSDS.AOS.Header.Spacecraft-Identifier| #*01100011)
					   (cons STC::'|STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number| #*01)
					   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle| 10)
					   (cons STC::'|STC.CCSDS.AOS.Header.Reserved-Spare| #*00)
					   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag| "Interpreted")
					   (cons STC::'|STC.CCSDS.AOS.Header.Replay-Flag| "False")
					   (cons STC::'|STC.CCSDS.AOS.Transfer-Frame-Data-Field| (subseq full-frame (* 6 8)))) ;6 octets
				 (decode full-frame (gethash "STC.CCSDS.AOS.Container.Frame" TEST-TABLE) TEST-TABLE '() 0)))

	;;;Parameters
	   (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number|  #*01)
				 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number" TEST-TABLE) TEST-TABLE '() 0)))
	  
	  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Spacecraft-Identifier| #*01100011)
				 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Spacecraft-Identifier" TEST-TABLE) TEST-TABLE '() 2)))

	  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-ID| 43)
				 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-ID" TEST-TABLE) TEST-TABLE '() 10)))
	  
	  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count| 9898155)
				 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count" TEST-TABLE) TEST-TABLE '() 16)))
	 
	  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Replay-Flag| "False")
				 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Replay-Flag" TEST-TABLE) TEST-TABLE '() 40)))
	  
	  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag| "Interpreted")
				 (decode full-frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag" TEST-TABLE) TEST-TABLE '() 41)))
	  
	  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Reserved-Spare| #*00)
		   (decode full-frame (gethash "STC.CCSDS.AOS.Header.Reserved-Spare" TEST-TABLE) TEST-TABLE '() 42)))
	  
	  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle| 10)
		  (decode full-frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle" TEST-TABLE) TEST-TABLE '() 44)))
	  
	  (is (equal (cons STC::'|STC.CCSDS.AOS.Transfer-Frame-Data-Field| (subseq full-frame (* 6 8))) ;6 octets
		   (decode full-frame (gethash "STC.CCSDS.AOS.Transfer-Frame-Data-Field" TEST-TABLE) TEST-TABLE '() 48)))
	  )))


(test AOS-decode
  "Simple decode test of AOS frame"
  (with-AOS-TEST-1
	(with-pack-frame
	  (let ((packet-list (xtce-engine::monad full-frame TEST-TABLE)))
		(is (equal 31 (length packet-list)))
	  (dolist (i packet-list)
		(is (equal
			 i
			 (list (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Data-Length| 3)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Version-Number| 0)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier| #*00000000001)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag| 0)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Type| 0)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count| 666)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Sequence-Flags| #*11)
				   (cons STC::'|STC.CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field| #*10111010110111000000110111101101)))))))))



(defmacro with-AOS-TEST-2 (&body body)
  "AOS Frame with lead fragment short circuiting idle packets"
  `(let* ((AOS-TEST-HEADER (alist->bit-vector
							(list (cons 'transfer-frame-version-number #*01)
								  (cons 'spacecraft-id #*01100011) ;0x63
								  (cons 'virtual-channel-id #*101011) ;43
								  (cons 'virtual-channel-frame-count #*100101110000100010101011); 9898155
								  (cons 'replay-flag #*0)
								  (cons 'virtual-channel-frame-count-usage-flag #*1)
								  (cons 'reserved-space #*00)
								  (cons 'vc-frame-count-cycle #*1010))))

		  (test-space-packet (alist->bit-vector
							  (list (cons 'packet-version-number  #*000)
									(cons 'packet-type #*0)
									(cons 'sec-hdr-flag #*0)
									(cons 'apid #*00000000001)
									(cons 'sequence-flags #*11)
									(cons 'sequence-count #*00001010011010)
									(cons 'data-len (uint->bit-vector (- (/ (length (uint->bit-vector #xBADC0DED)) 8) 1) 16))
									(cons 'data (uint->bit-vector #xBADC0DED)))))

		  (test-idle-packet (alist->bit-vector
							 (list (cons 'packet-version-number  #*000)
								   (cons 'packet-type #*0)
								   (cons 'sec-hdr-flag #*0)
								   (cons 'appid #*11111111111)
								   (cons 'sequence-flags #*11)
								   (cons 'sequence-count #*00001010011010)
								   (cons 'data-len (uint->bit-vector (- (/ (length (uint->bit-vector #xFFFFFFFF)) 8) 1) 16))
								   (cons 'data (uint->bit-vector #xFFFFFFFF)))))

		  (fragged-space-packet-lead (subseq test-space-packet 0 (/ (length test-space-packet) 2)))
		  
		  (fragged-space-packet-rear (subseq test-space-packet (/ (length test-space-packet) 2)))
		  
		  (test-mpdu-header (alist->bit-vector
							 (list (cons 'spare #*00000)
								   (cons 'first-header-pointer #*00000000101)))))
	 
	 ,@body
	 ))


(defmacro with-pack-lead-fragment-frame (&body body)
  `(let* ((space-packets (concatenate-bit-arrays
						  fragged-space-packet-rear
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet
						  test-space-packet  ;30
						  test-idle-packet   ;31 
						  test-idle-packet)) ;32

		  (full-frame (pad-bit-vector 
					   (concatenate-bit-arrays
						AOS-TEST-HEADER
						test-mpdu-header
						space-packets)
					   8192
					   :position :right
					   :pad-element 1))


		  (TEST-TABLE (xtce::register-keys-in-sequence
					   (stc::with-ccsds.space-packet.parameters
						   (stc::with-ccsds.space-packet.types
							   (stc::with-ccsds.space-packet.containers
								   (stc::with-ccsds.mpdu.containers
									   (stc::with-ccsds.mpdu.types
										   (stc::with-ccsds.mpdu.parameters
											   (stc::with-ccsds.aos.containers
												   (stc::with-ccsds.aos.header.parameters
													   (stc::with-ccsds.aos.header.types '())))))))))
					   (filesystem-hash-table:make-filesystem-hash-table) 'Test)))
	 ,@body
	 ))

(test AOS-decode
  "Simple decode test of AOS frame"
  (with-AOS-TEST-2
	(with-pack-lead-fragment-frame
	  (is (equal test-space-packet (concatenate-bit-arrays fragged-space-packet-lead fragged-space-packet-rear)))
	  (let ((packet-list (xtce-engine::monad full-frame TEST-TABLE
											 :packet-extractor
											 (lambda (data first-header-pointer symbol-table alist)
											   (extract-space-packets data first-header-pointer symbol-table alist fragged-space-packet-lead 40)))))
		(is (equal 30 (length packet-list)))
	  (dolist (i packet-list)
		(is (equal
			 i
			 (list (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Data-Length| 3)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Version-Number| 0)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier| #*00000000001)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag| 0)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Type| 0)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count| 666)
				   (cons STC::'|STC.CCSDS.Space-Packet.Header.Sequence-Flags| #*11)
				   (cons STC::'|STC.CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field| #*10111010110111000000110111101101)))))))))

(defmacro with-aos-header (&body body)
  `(let* ((aos-header (alist->bit-vector
					   (list (cons 'transfer-frame-version-number #*01)
							 (cons 'spacecraft-id #*01100011) ;0x63
							 (cons 'virtual-channel-id #*101011) ;43
							 (cons 'virtual-channel-frame-count #*100101110000100010101011); 9898155
							 (cons 'replay-flag #*0)
							 (cons 'virtual-channel-frame-count-usage-flag #*1)
							 (cons 'reserved-space #*00)
							 (cons 'vc-frame-count-cycle #*1010)))))
	 ,@body))

(defmacro with-space-packet (&body body)
  `(let ((space-packet (alist->bit-vector
						 (list (cons 'packet-version-number  #*000)
							   (cons 'packet-type #*0)
							   (cons 'sec-hdr-flag #*0)
							   (cons 'apid #*00000000001)
							   (cons 'sequence-flags #*11)
							   (cons 'sequence-count #*00001010011010)
							   (cons 'data-len (uint->bit-vector (- (/ (length (uint->bit-vector #xBADC0DED)) 8) 1) 16))
							   (cons 'data (uint->bit-vector #xBADC0DED))))))
	 ,@body))

(defmacro with-idle-packet (&body body)
  `(let ((idle-packet (alist->bit-vector
					   (list (cons 'packet-version-number  #*000)
							 (cons 'packet-type #*0)
							 (cons 'sec-hdr-flag #*0)
							 (cons 'appid #*11111111111)
							 (cons 'sequence-flags #*11)
							 (cons 'sequence-count #*00001010011010)
							 (cons 'data-len (uint->bit-vector (- (/ (length (uint->bit-vector #xFFFFFFFF)) 8) 1) 16))
							 (cons 'data (uint->bit-vector #xFFFFFFFF))))))
	 ,@body))

(defmacro with-test-table (&body body)
  `(let ((test-table (xtce::register-keys-in-sequence
					  (stc::with-ccsds.space-packet.parameters
						  (stc::with-ccsds.space-packet.types
							  (stc::with-ccsds.space-packet.containers
								  (stc::with-ccsds.mpdu.containers
									  (stc::with-ccsds.mpdu.types
										  (stc::with-ccsds.mpdu.parameters
											  (stc::with-ccsds.aos.containers
												  (stc::with-ccsds.aos.header.parameters
													  (stc::with-ccsds.aos.header.types '())))))))))
					  (filesystem-hash-table:make-filesystem-hash-table) 'Test)))
	 ,@body))

(defmacro with-pack-lead-fragment-idle-frame (&body body)
  `(let* ((payload-1 (list space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet  ;30
						   idle-packet   ;31 
						   idle-packet))
		  (payload-2 nil)
		  (mpdu-1 (make-mpdu-header 0 4096))
		  (mpdu-2 nil)
		  (frame-1 nil)
		  (frame-2 nil)
		  (lead-frag nil)
		  (rear-frag nil)
		  )
	 (multiple-value-bind (frame padding-required) (apply #'pack-arrays-with-padding idle-packet 8192 AOS-HEADER mpdu-1 payload-1)
	   (multiple-value-bind (next-mpdu-header lead-frag_ rear-frag_) 
		   (fragment-packet idle-packet padding-required 1024)
		 (setf lead-frag lead-frag_)
		 (setf rear-frag rear-frag_)
		 (setf frame-1 (concatenate-bit-arrays frame lead-frag))
		 (setf mpdu-2 next-mpdu-header)
		 (setf frame-2 (pad-bit-vector (pack-arrays-with-padding idle-packet 8192 AOS-HEADER next-mpdu-header rear-frag) 8192 :position :right))
		 
		 ))
	 ,@body))

(test leading-idle-frame-fragment
  "Simple decode test of AOS frame"
  (with-test-table
	(with-aos-header
	  (with-space-packet
		(with-idle-packet
		  (with-pack-lead-fragment-idle-frame
			(is (equal idle-packet (concatenate-bit-arrays lead-frag rear-frag)))
			(let ((packet-list (xtce-engine::monad
								frame-1 test-table
								:packet-extractor
								(lambda (data first-header-pointer symbol-table alist)
								  (extract-space-packets data first-header-pointer symbol-table alist lead-frag 40)))))
			  (is (equal 29 (length packet-list))))
			))))))

(test spanning-packet
  (with-test-table
	(with-aos-header
	  (with-space-packet
		(with-idle-packet
		  (with-pack-lead-fragment-idle-frame
			(let ((packets nil))
			  (multiple-value-bind (res next-monad)
				  (monad frame-1 test-table
						 :packet-extractor
						 (lambda (data first-header-pointer symbol-table alist)
						   (extract-space-packets data first-header-pointer symbol-table alist #* 0)))
				(setf packets res)

				(monad frame-2 test-table
					   :packet-extractor
						 (lambda (data first-header-pointer symbol-table alist)
						   (extract-space-packets data first-header-pointer symbol-table alist lead-frag (- (length idle-packet) (length lead-frag)))))
				;;(nconc (funcall next-monad frame-2 test-table) packets)
				;(is (equal 29 (length packets)))
				))))))))


(defmacro with-pack-lead-fragment-space-frame (&body body)
  `(let* ((payload-1 (list space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet
						   space-packet  ;30
						   idle-packet   ;31 
						   idle-packet))
		  (payload-2 nil)
		  (mpdu-1 (make-mpdu-header 0 4096))
		  (mpdu-2 nil)
		  (frame-1 nil)
		  (frame-2 nil)
		  (lead-frag nil)
		  (rear-frag nil)
		  )
	 (multiple-value-bind (frame padding-required) (apply #'pack-arrays-with-padding idle-packet 8192 AOS-HEADER mpdu-1 payload-1)
	   (multiple-value-bind (next-mpdu-header lead-frag_ rear-frag_) 
		   (fragment-packet space-packet padding-required 1024)
		 (setf lead-frag lead-frag_)
		 (setf rear-frag rear-frag_)
		 (setf frame-1 (concatenate-bit-arrays frame lead-frag))
		 (setf mpdu-2 next-mpdu-header)
		 (setf frame-2 (pad-bit-vector (pack-arrays-with-padding idle-packet 8192 AOS-HEADER next-mpdu-header rear-frag) 8192 :position :right))
		 
		 ))
	 ,@body))

(test spanning-packet-2
  (with-test-table
	(with-aos-header
	  (with-space-packet
		(with-idle-packet
		  (with-pack-lead-fragment-space-frame
			(let ((packets nil))
			 (setf packets (monad frame-2 test-table
					 :packet-extractor
					 (lambda (data first-header-pointer symbol-table alist)
					   (extract-space-packets data first-header-pointer symbol-table alist lead-frag (length rear-frag)))))
			  (is (equal 1 (length packets)))
			  (dolist (i packets)
				(is (equal i
						   (list (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Data-Length| 3)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Version-Number| 0)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier| #*00000000001)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag| 0)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Type| 0)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count| 666)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Sequence-Flags| #*11)
								 (cons STC::'|STC.CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field| #*10111010110111000000110111101101))
						   )
			  )))))))))


(test spanning-packet
  (with-test-table
	(with-aos-header
	  (with-space-packet
		(with-idle-packet
		  (with-pack-lead-fragment-space-frame
			(let ((packets nil))
			  (multiple-value-bind (res next-monad)
				  (monad frame-1 test-table
						 :packet-extractor
						 (lambda (data first-header-pointer symbol-table alist)
						   (extract-space-packets data first-header-pointer symbol-table alist #* 0)))
				(setf packets res)
				(is (equal 29 (length packets)))
				(nconc packets (funcall next-monad frame-2 test-table))
				;;(nconc (funcall next-monad frame-2 test-table) packets)
				;(print packets)
				(is (equal 30 (length packets)))
				(dolist (i packets)
				(is (equal i
						   (list (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Data-Length| 3)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Version-Number| 0)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier| #*00000000001)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag| 0)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Type| 0)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count| 666)
								 (cons STC::'|STC.CCSDS.Space-Packet.Header.Sequence-Flags| #*11)
								 (cons STC::'|STC.CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field| #*10111010110111000000110111101101))
						   )
			  ))
				))))))))
