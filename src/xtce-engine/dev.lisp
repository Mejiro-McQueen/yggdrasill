(in-package :xtce-engine)

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
					  (make-filesystem-hash-table) 'Test)))
	 ,@body))



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
