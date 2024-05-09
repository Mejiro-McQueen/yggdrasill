(ql:quickload "fiveam")
(in-package :cl-user)
(defpackage yggdrasill-test 
  (:use :cl
		:filesystem-hash-table
        :fiveam
		:xtce-engine))
(in-package :yggdrasill-test)

(setf fiveam:*run-test-when-defined* t)
(setf fiveam:*on-failure* :debug)

(def-suite mpdu-tests
  :description "MPDU Tests")

(in-suite mpdu-tests)

;; (setf *TEST* #x1acffc1dFAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA)
;; (print-hex (look-for-pattern *TEST*
;; 							 (make-sync-pattern #x1acffc1d (integer-length #x1acffc1d))))

(defmacro with-aos-header (&body body)
  `(let* ((header-result (list (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Data-Length| 3)
							   (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Version-Number| 0)
							   (cons STC::'|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier| #*00000000001)
							   (cons STC::'|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag| 0)
							   (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Type| 0)
							   (cons STC::'|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count| 666)
							   (cons STC::'|STC.CCSDS.Space-Packet.Header.Sequence-Flags| #*11)
							   (cons STC::'|STC.CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field| #*10111010110111000000110111101101)))
		  
		  (aos-header (alist->bit-vector
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

(defmacro with-pack-fragment-idle-frame (&body body)
  `(let* ((payload-1 (nconc (make-list 30 :initial-element space-packet) (make-list 2 :initial-element idle-packet)))
		  (payload-2 nil)
		  (mpdu-1 (make-mpdu-header 0 4096))
		  (mpdu-2 nil)
		  (frame-1 nil)
		  (frame-2 nil)
		  (lead-frag nil)
		  (rear-frag nil))
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

(defmacro with-pack-lead-fragment-space-frame (&body body)
  `(let* ((payload-1 (nconc (make-list 30 :initial-element space-packet) (make-list 2 :initial-element idle-packet)))
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

(defmacro with-huge-space-packet (&body body)
  `(let* ((payload (pack-arrays-with-padding (uint->bit-vector #xBADC0DED) (* 4096 8)))
		  (space-packet (alist->bit-vector
						 (list (cons 'packet-version-number  #*000)
							   (cons 'packet-type #*0)
							   (cons 'sec-hdr-flag #*0)
							   (cons 'apid #*00000000001)
							   (cons 'sequence-flags #*11)
							   (cons 'sequence-count #*00001010011010)
							   (cons 'data-len (uint->bit-vector (- (/ (length payload) 8) 1) 16))
							   (cons 'data payload)))))
	 ,@body))


(test AOS-decode
  "Simple decode test of AOS frame: We decode segments of the bit vector manually."
  (with-aos-header
	(with-test-table
	  (with-space-packet
		(let ((frame (pad-bit-vector (concatenate-bit-arrays aos-header (make-mpdu-header 0 0) space-packet) 8192 :position :right :pad-element 1 )))
	;;;Types
		  (is (equal #*01 (decode frame (gethash "STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number-Type" TEST-TABLE) TEST-TABLE '() 0)))
		  (is (equal #*01100011 (decode frame (gethash "STC.CCSDS.AOS.Header.Spacecraft-Identifier-Type" TEST-TABLE) TEST-TABLE '() 2)))
		  (is (equal 43 (decode frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-ID-Type" TEST-TABLE) TEST-TABLE '() 10)))
		  (is (equal 9898155 (decode frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Type" TEST-TABLE) TEST-TABLE '() 16)))
		  (is (equal "False" (decode frame (gethash "STC.CCSDS.AOS.Header.Replay-Flag-Type" TEST-TABLE) TEST-TABLE '() 40)))
		  (is (equal "Interpreted" (decode frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag-Type" TEST-TABLE) TEST-TABLE '() 41)))
		  (is (equal #*00 (decode frame (gethash "STC.CCSDS.AOS.Header.Reserved-Spare-Type" TEST-TABLE) TEST-TABLE '() 42)))
		  (is (equal 10 (decode frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle-Type" TEST-TABLE) TEST-TABLE '() 44)))
		  (is (equal (subseq frame (* 6 8)) ;6 octets
					 (decode frame (gethash "STC.CCSDS.AOS.Transfer-Frame-Data-Field-Type" TEST-TABLE) TEST-TABLE '() 48)))

	;;;Containers
		  (is (equal (list (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count| 9898155)
						   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-ID| 43)
						   (cons STC::'|STC.CCSDS.AOS.Header.Spacecraft-Identifier| #*01100011)
						   (cons STC::'|STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number| #*01)
						   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle| 10)
						   (cons STC::'|STC.CCSDS.AOS.Header.Reserved-Spare| #*00)
						   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag| "Interpreted")
						   (cons STC::'|STC.CCSDS.AOS.Header.Replay-Flag| "False"))
					 (decode frame (gethash "STC.CCSDS.AOS.Container.Transfer-Frame-Primary-Header" TEST-TABLE) TEST-TABLE '() 0)))

		  
		  (is (equal (list (cons STC::'|STC.CCSDS.AOS.Header.Spacecraft-Identifier| #*01100011)
						   (cons STC::'|STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number| #*01))
					 (decode frame (gethash "STC.CCSDS.AOS.Container.Transfer-Frame-Primary-Header.Master-Channel-ID" TEST-TABLE) TEST-TABLE '() 0)))

		  (is (equal (list (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count| 9898155)
						   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-ID| 43)
						   (cons STC::'|STC.CCSDS.AOS.Header.Spacecraft-Identifier| #*01100011)
						   (cons STC::'|STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number| #*01)
						   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle| 10)
						   (cons STC::'|STC.CCSDS.AOS.Header.Reserved-Spare| #*00)
						   (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag| "Interpreted")
						   (cons STC::'|STC.CCSDS.AOS.Header.Replay-Flag| "False")
						   (cons STC::'|STC.CCSDS.AOS.Transfer-Frame-Data-Field| (subseq frame (* 6 8)))) ;6 octets
					 (decode frame (gethash "STC.CCSDS.AOS.Container.Frame" TEST-TABLE) TEST-TABLE '() 0)))

	;;;Parameters
		  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number|  #*01)
					 (decode frame (gethash "STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number" TEST-TABLE) TEST-TABLE '() 0)))
		  
		  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Spacecraft-Identifier| #*01100011)
					 (decode frame (gethash "STC.CCSDS.AOS.Header.Spacecraft-Identifier" TEST-TABLE) TEST-TABLE '() 2)))

		  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-ID| 43)
					 (decode frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-ID" TEST-TABLE) TEST-TABLE '() 10)))
		  
		  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count| 9898155)
					 (decode frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count" TEST-TABLE) TEST-TABLE '() 16)))
		  
		  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Replay-Flag| "False")
					 (decode frame (gethash "STC.CCSDS.AOS.Header.Replay-Flag" TEST-TABLE) TEST-TABLE '() 40)))
		  
		  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag| "Interpreted")
					 (decode frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag" TEST-TABLE) TEST-TABLE '() 41)))
		  
		  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Reserved-Spare| #*00)
					 (decode frame (gethash "STC.CCSDS.AOS.Header.Reserved-Spare" TEST-TABLE) TEST-TABLE '() 42)))
		  
		  (is (equal (cons STC::'|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle| 10)
					 (decode frame (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle" TEST-TABLE) TEST-TABLE '() 44)))
		  
		  (is (equal (cons STC::'|STC.CCSDS.AOS.Transfer-Frame-Data-Field| (subseq frame (* 6 8))) ;6 octets
					 (decode frame (gethash "STC.CCSDS.AOS.Transfer-Frame-Data-Field" TEST-TABLE) TEST-TABLE '() 48))))))))


(test simple-30-packet-decode
  "Simple decode test of AOS frame: Generate a frame with 30 space packets, run through monad and check decoded packet each manually."
  (with-aos-header
	(with-space-packet
	  (with-test-table
		(let* ((frame (pad-bit-vector (apply #'concatenate-bit-arrays aos-header (make-mpdu-header 0 0) (make-list 30 :initial-element space-packet)) 8192 :position :right :pad-element 1))
			   (packet-list (xtce-engine::monad frame TEST-TABLE)))
		  (is (equal 30 (length packet-list)))
		  (dolist (i packet-list)
			(is (equal i header-result))))))))

(test packet-fragmenter
  "Test packet fragmenter"
  (with-space-packet
	(multiple-value-bind (next-mpdu lead-frag rear-frag) (fragment-packet space-packet (/ (length space-packet) 2) 1024)
	  (is (equal (length lead-frag) 40))
	  (is (equal (length rear-frag) 40))
	  (is (equal space-packet (concatenate-bit-arrays lead-frag rear-frag)))
	  (is (equal next-mpdu #*0000000000000101) ) ; equivalent to: bit-vector->uint #*0000000000000101 => 5 bytes => (* 8 5) = 40 bits)
	  
	  (multiple-value-bind (next-mpdu lead-frag rear-frag) (fragment-packet space-packet 80 1024)
		(is (equal next-mpdu #*0000000000000000))
		(is (equal lead-frag space-packet))
		(is (equal rear-frag #*)))
	  
	  (multiple-value-bind (next-mpdu lead-frag rear-frag) (fragment-packet space-packet 16 40)
										;No frame start first header pointer
		(is (equal next-mpdu #*0000011111111111))
										;First 16 bits
		(is (equal lead-frag #*0000000000000001))
										;The rest
		(is (equal rear-frag #*1100001010011010000000000000001110111010110111000000110111101101))
		(is (equal (concatenate-bit-arrays lead-frag rear-frag) space-packet))))))



(test space-packet-with-lead-frag
  "Simple decode test of AOS frame: decode 30 space packets where the end of the array is the leading idle packet fragment. Designed to test fragmented-packet-error condition"
  (with-aos-header
	(with-idle-packet
	  (with-space-packet
		(with-pack-fragment-idle-frame
		  (with-test-table
			(let ((packet-list (xtce-engine::monad frame-1 TEST-TABLE)))
			  (is (equal 30 (length packet-list)))
			  (dolist (i packet-list)
				(is (equal i header-result))))))))))

(test fragmented-idle-packet-restoration
  "Restore and discard a fragmented idle packet"
  (with-test-table
	(with-aos-header
	  (with-space-packet
		(with-idle-packet
		  (with-pack-lead-fragment-idle-frame
			(let ((packet-list nil))
			  (multiple-value-bind (res next-monad)
				  (monad frame-1 test-table)
				(setf packet-list res)
				(is (equal 30 (length packet-list)))

				(multiple-value-bind (res next-monad) (funcall next-monad frame-2 test-table)
				  (is (equal nil res)))
				
				(is (equal 30 (length packet-list)))

				(dolist (i packet-list)
				  (is (equal i header-result)))))))))))

(test fragmented-idle-packet-restoration
  "Start extraction with second frame containing a rear frag. Since frame-1 containing the lead-frag was never seen, do not attempt to reconstruct."
  (with-test-table
	(with-aos-header
	  (with-space-packet
		(with-idle-packet
		  (with-pack-lead-fragment-space-frame
			(let ((packet-list nil))
			  (multiple-value-bind (res next-monad)
				  (monad frame-1 test-table)
				(setf packet-list res)
				(is (equal 30 (length packet-list)))

				(multiple-value-bind (res next-monad) (funcall next-monad frame-2 test-table)
				  (is (equal (list header-result) res))
				  (nconc packet-list res ))
				
				(is (equal 31 (length packet-list)))

				(dolist (i packet-list)
				  (is (equal i header-result)))))))))))

(test missed-lead-frag
  "Start extraction with second frame containing a rear frag. 
   Since frame-1 containing the lead-frag was never seen, do not attempt to reconstruct.
   Since the last frame contains junk data, no packets are extracted"
  (with-test-table
	(with-aos-header
	  (with-space-packet
		(with-idle-packet
		  (with-pack-lead-fragment-space-frame
			(let ((packet-list nil))
			  (multiple-value-bind (res) (monad frame-2 test-table)
				(is (equal (list header-result) res))
				(nconc packet-list res ))
			  (is (equal 0 (length packet-list))))))))))


(test massive-fragmented-packet
  (with-test-table
	  (with-test-table
		(with-aos-header
		  (with-idle-packet
			(with-huge-space-packet
			  (let ((rear space-packet)
					(size-to-frag (- 8192 48 16))
					(mpdu (make-mpdu-header 0 0))
					(frames)
					(current-frame))

				(loop
				  while (not (equal rear #*))
				  do
					 (multiple-value-bind (next-mpdu lead rear_) (fragment-packet rear size-to-frag 8192)
					   (multiple-value-bind (frame leftover) (pack-arrays-with-padding idle-packet 8192 aos-header mpdu lead)
						 (setf current-frame (pad-bit-vector frame 8192 :position :right :pad-element 1))
						 (push current-frame frames)
						 (decf size-to-frag leftover)
						 (setf mpdu next-mpdu)
						 (setf rear rear_))))

				
				(let ((m 'monad)
					  (packet-list nil))
				  (dolist (frame (reverse frames))
					(multiple-value-bind (res next-monad) (funcall m frame TEST-TABLE)
					  (setf m next-monad)
					  (when res
						(push res packet-list))))

				  ;; We should only have one packet
				  (is (equal (length packet-list) 1))))))))))


(defun simple-huge-packet-decode ()
  "Simple decode test of AOS frame: Generate a frame with 30 space packets, run through monad and check decoded packet each manually."
  (with-aos-header
	(with-space-packet
	  (with-test-table
		(let* ((frame (apply #'concatenate-bit-arrays aos-header (make-mpdu-header 0 0) (make-list 10000 :initial-element space-packet)))
			   (packets (xtce-engine::monad frame TEST-TABLE))))))))

(time (simple-huge-packet-decode))

  (with-aos-header
	(with-space-packet
	  (with-test-table
		;; (pad-bit-vector (apply #'concatenate-bit-arrays aos-header (make-mpdu-header 0 0)
		;; 					   (make-list 30 :initial-element space-packet)) 8192 :position :right :pad-element 1)
		
		
		(pad-bit-vector (apply #'concatenate-bit-arrays aos-header (make-mpdu-header 0 0)
							   (make-list 30 :initial-element space-packet)) 8192 :position :right :pad-element 1))))
		
