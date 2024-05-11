(in-package :standard-template-constructs)
(use-package :xtce)

;;OPTIONS
(defparameter Space-Packet.Header.Sequence-or-Name :sequence)
(defparameter Space-Packet.Secondary-Header-Length 0)

;;
(defparameter Space-Packet.Secondary-Header-Type
  (make-binary-parameter-type
   "STC.CCSDS.Space-Packet.Header.Packet-Identification-Type"
   :short-description "CCSDS Space Secondary Packet Header element."
   :data-encoding
   (make-binary-data-encoding (make-size-in-bits (make-fixed-value Space-Packet.Secondary-Header-Length)))))

(defparameter Space-Packet.Header.Packet-Name "STC.CCSDS.Space-Packet.Header.Packet-Name")

(defparameter Space-Packet.Header.Packet-Name-Type "STC.CCSDS.Space-Packet.Header.Packet-Name-Type")

(defparameter Space-Packet.Header.Packet-Count "STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count")

(defparameter Space-Packet.Header.Packet-Name  "STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type")

(defun set-CCSDS.Space-Packet.Header.Sequence-or-Name (a)
  (assert (member a '(:sequence :name)) (a) "Option ~a must be one of <'sequence|'name>" a
  (setf Space-Packet.Header.Sequence-or-Name a)))

(defun get-CCSDS.Space-Packet.Header.Sequence-or-Name-Type ()
  (case Space-Packet.Header.Sequence-or-Name
	(:sequence
	 CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type)
	(:name
	 CCSDS.Space-Packet.Header.Packet-Name-Type)
	(t (error "Space-Packet.Header.Sequence-or-Name must be :name or :sequence"))))

(defun get-CCSDS.Space-Packet.Header.Sequence-or-Name-Ref ()
  (assert (member Space-Packet.Header.Sequence-or-Name '(:sequence :name)) (Space-Packet.Header.Sequence-or-Name)
		  "Invalid Option ~A: Must be :name or :sequence"  Space-Packet.Header.Sequence-or-Name)
  (case Space-Packet.Header.Sequence-or-Name
	(:sequence
	 Space-Packet.Header.Packet-Count)
	(:name
	 Space-Packet.Header.Packet-Name)
	(t (error "Space-Packet.Header.Sequence-or-Name must be :name or :sequence"))))

(defun set-CCSDS.Space-Packet.Secondary-Header-Length (n)
  (setf Space-Packet.Secondary-Header-Length n))

(defun get-CCSDS.Space-Packet.Header.Sequence-or-Name ()
  (assert (member Space-Packet.Header.Sequence-or-Name '(:sequence :name)) (Space-Packet.Header.Sequence-or-Name)
		  "Invalid Option ~A: Must be 'name or 'sequence"  Space-Packet.Header.Sequence-or-Name)
  (case Space-Packet.Header.Sequence-or-Name
	(:sequence
	 CCSDS.Space-Packet.Header.Packet-Sequence-Count)
	(:name
	 CCSDS.Space-Packet.Header.Packet-Name)))

(defparameter CCSDS.Space-Packet.Header.Packet-Version-Number-Type
  (make-integer-parameter-type
   "STC.CCSDS.Space-Packet.Header.Packet-Version-Number-Type"
   :short-description "CCSDS Space Packet Header element."
   :long-description (make-long-description "CCSDS Space Packet Header element. Mandatory. 3 bit field fixed to 000.")
   :data-encoding (make-integer-data-encoding :size-in-bits 3)
   ))

(defparameter CCSDS.Space-Packet.Header.Packet-Type-Type
  (make-enumerated-parameter-type
   "STC.CCSDS.Space-Packet.Header.Packet-Type-Type"
   :short-description "CCSDS Space Packet Header element."
   :long-description (make-long-description "CCSDS Space Packet Header element. The exact definition of ‘telemetry Packets’ and ‘telecommand Packets’ needs to be established by the project that uses this protocol. Element of Packet-Identification subdivision.")
   :enumeration-list
   (list (make-enumeration "Telemetry" 0) (make-enumeration "Telecommand" 1))
   :data-encoding (make-integer-data-encoding :size-in-bits 1)))

(defparameter CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type
  (make-enumerated-parameter-type
   "STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type"
   :short-description "CCSDS Space Packet Header element."
   :long-description (make-long-description "CCSDS Space Packet Header element. Indicates the presence or absence of the Packet Seconday Header within this space packet. This flag shall be static with respect to the APID and managed data path throughout a mission phase. Element of Packet-Identification subdivision.")
   :enumeration-list
   (list (make-enumeration "Absent" 0 :short-description "This packet contains a secondary header.")
		 (make-enumeration "Present" 1 :short-description "This packet does not contain a secondary header."))
   :data-encoding (make-integer-data-encoding :size-in-bits 1)))

(defparameter CCSDS.Space-Packet.Header.Application-Process-Identifier-Type
  (make-binary-parameter-type
   "STC.CCSDS.Space-Packet.Header.Application-Process-Identifier-Type"
   :short-description "CCSDS Space Packet Header element."
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 11)))))


(defparameter CCSDS.Space-Packet.Header.Packet-Identification-Type
  ;;We use binary-parameter-type because we need to check if this parameter matches a given idle-pattern.
  ;;We could treat it as an UINT or string, but it could get goofy fast.
  (make-binary-parameter-type
   "STC.CCSDS.Space-Packet.Header.Packet-Identification-Type"
   :short-description "CCSDS Space Packet Header element."
   :data-encoding
   (make-binary-data-encoding (make-size-in-bits (make-fixed-value 16)))))

(defparameter CCSDS.Space-Packet.Header.Packet-Sequence-Control-Type
  (make-binary-parameter-type
   "STC.CCSDS.Space-Packet.Header.Packet-Sequence-Control-Type"
   :short-description "CCSDS Space Packet Header element."
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 16)))))

(defparameter CCSDS.Space-Packet.Header.Sequence-Flags-Type
  (make-enumerated-parameter-type
   "STC.CCSDS.Space-Packet.Header.Sequence-Flags-Type"
   :enumeration-list (list
					  (make-enumeration #b00 "Continuation" :short-description "Space Packet contains a continuation segment of User Data.")
					  (make-enumeration #b01 "First-Segment" :short-description "Space Packet contains the first segment of User Data.")
					  (make-enumeration #b10 "Last-Segment" :short-description "Space Packet contains the last segment of User Data.")
					  (make-enumeration #b11 "Unsegmented" :short-description "Space Packet is unsegmented."))
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 2)))))

(defparameter CCSDS.Space-Packet.Header.Packet-Data-Length-Type
  (make-integer-parameter-type
   "STC.CCSDS.Space-Packet.Header.Packet-Data-Length-Type"
   :size-in-bits 16
   :short-description "CCSDS Space Packet Header Element."
   :long-description (make-long-description "Is one fewer than the length in octets of the Packet Data Field. Described by C=(Total Number of Octets in the Packet Data Field) -1")
   :data-encoding (make-integer-data-encoding :size-in-bits 16)))

(defparameter CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type
  (make-integer-parameter-type
   "STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type"
   :size-in-bits 14
   :short-description "CCSDS Space Packet Header Element. Part of Packet Sequence Control container."
   :long-description (make-long-description "The CCSDS spec calls out for either a packet name or packet count.")
  :data-encoding (make-integer-data-encoding :size-in-bits 14)))

(defparameter CCSDS.Space-Packet.Header.Packet-Name-Type
  (make-string-parameter-type
   "STC.CCSDS.Space-Packet.Header.Packet-Name"
   :short-description "Part of Packet Sequence Control container."
   :long-description (make-long-description "The CCSDS spec calls out for either a packet name or packet.")
   :data-encoding (make-string-data-encoding (make-size-in-bits (make-fixed (make-fixed-value 14))))))

(defparameter CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field-Type
  (make-binary-parameter-type
   "STC.CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field-Type"
   :short-description "Bytes containing the packet payload"
   :data-encoding (make-binary-data-encoding
				   (make-size-in-bits
					(make-dynamic-value
					 (make-parameter-instance-ref "STC.CCSDS.Space-Packet.Header.Packet-Data-Length")
					 :linear-adjustment (make-linear-adjustment :slope 8 :intercept 8)
					 )))))

(defun with-ccsds.space-packet.header.types (type-list)
  (append type-list
		  (list
		   CCSDS.Space-Packet.Header.Packet-Identification-Type
		   CCSDS.Space-Packet.Header.Packet-Sequence-Control-Type
		   CCSDS.Space-Packet.Header.Packet-Version-Number-Type
		   CCSDS.Space-Packet.Header.Packet-Type-Type
		   CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type
		   CCSDS.Space-Packet.Header.Application-Process-Identifier-Type
		   CCSDS.Space-Packet.Header.Sequence-Flags-Type
		   CCSDS.Space-Packet.Header.Packet-Data-Length-Type
		   CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field-Type
		   (get-CCSDS.Space-Packet.Header.Sequence-or-Name-Type))))

(defun with-ccsds.space-packet.types (type-list)
  (with-ccsds.space-packet.header.types type-list))

(defparameter CCSDS.Space-Packet.Header.Packet-Version-Number
  (make-parameter "STC.CCSDS.Space-Packet.Header.Packet-Version-Number" "STC.CCSDS.Space-Packet.Header.Packet-Version-Number-Type"))

(defparameter CCSDS.Space-Packet.Header.Packet-Type
	(make-parameter "STC.CCSDS.Space-Packet.Header.Packet-Type" "STC.CCSDS.Space-Packet.Header.Packet-Type-Type"))

(defparameter CCSDS.Space-Packet.Header.Secondary-Header-Flag
  (make-parameter "STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag" "STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type"))

(defparameter CCSDS.Space-Packet.Header.Application-Process-Identifier
  (make-parameter "STC.CCSDS.Space-Packet.Header.Application-Process-Identifier" "STC.CCSDS.Space-Packet.Header.Application-Process-Identifier-Type"))

(defparameter CCSDS.Space-Packet.Header.Packet-Identification
  (make-parameter "STC.CCSDS.Space-Packet.Header.Packet-Identification" "STC.CCSDS.Space-Packet.Header.Packet-Identification-Type"))

(defparameter CCSDS.Space-Packet.Header.Packet-Sequence-Control
  (make-parameter "STC.CCSDS.Space-Packet.Header.Packet-Sequence-Control" "STC.CCSDS.Space-Packet.Header.Packet-Sequence-Control-Type"))

(defparameter CCSDS.Space-Packet.Header.Sequence-Flags
  (make-parameter "STC.CCSDS.Space-Packet.Header.Sequence-Flags" "STC.CCSDS.Space-Packet.Header.Sequence-Flags-Type"))

(defparameter CCSDS.Space-Packet.Header.Packet-Data-Length
	(make-parameter "STC.CCSDS.Space-Packet.Header.Packet-Data-Length" "STC.CCSDS.Space-Packet.Header.Packet-Data-Length-Type"))

(defparameter CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field
	(make-parameter "STC.CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field" "STC.CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field-Type"))

(defparameter CCSDS.Space-Packet.Header.Packet-Sequence-Count
  (make-parameter "STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count" "STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type"))

(defparameter CCSDS.Space-Packet.Header.Packet-Name
  (make-parameter Space-Packet.Header.Packet-Name Space-Packet.Header.Packet-Name-Type))

(defun with-ccsds.space-packet.header.parameters (parameter-list)
  (append
   parameter-list
   (list
	CCSDS.Space-Packet.Header.Packet-Version-Number
	CCSDS.Space-Packet.Header.Packet-Type
	CCSDS.Space-Packet.Header.Secondary-Header-Flag
	CCSDS.Space-Packet.Header.Application-Process-Identifier
	CCSDS.Space-Packet.Header.Packet-Identification
	CCSDS.Space-Packet.Header.Packet-Sequence-Control
	CCSDS.Space-Packet.Header.Sequence-Flags
	CCSDS.Space-Packet.Header.Packet-Data-Length
	CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field
	(get-CCSDS.Space-Packet.Header.Sequence-or-Name))))

(defun with-ccsds.space-packet.parameters (parameter-list)
  (with-ccsds.space-packet.header.parameters parameter-list)
  )

(defparameter CCSDS.Space-Packet.Container.Packet-Primary-Header
  (make-sequence-container
   "STC.CCSDS.Space-Packet.Container.Packet-Primary-Header"
   (list
	(make-parameter-ref-entry "STC.CCSDS.Space-Packet.Header.Packet-Version-Number")
	(make-container-ref-entry "STC.CCSDS.Space-Packet.Container.Header.Packet-Identification")
	(make-container-ref-entry "STC.CCSDS.Space-Packet.Container.Header.Packet-Sequence-Control")
	(make-parameter-ref-entry "STC.CCSDS.Space-Packet.Header.Packet-Data-Length"))))

(defparameter CCSDS.Space-Packet.Container.Header.Packet-Identification
  (make-sequence-container
   "STC.CCSDS.Space-Packet.Container.Header.Packet-Identification"
   (list
	(make-parameter-ref-entry "STC.CCSDS.Space-Packet.Header.Packet-Type")
	(make-parameter-ref-entry "STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag")
	(make-parameter-ref-entry "STC.CCSDS.Space-Packet.Header.Application-Process-Identifier"))))

(defparameter CCSDS.Space-Packet.Container.Header.Packet-Sequence-Control
  (make-sequence-container
   "STC.CCSDS.Space-Packet.Container.Header.Packet-Sequence-Control"
   (list
	(make-parameter-ref-entry "STC.CCSDS.Space-Packet.Header.Sequence-Flags")
	(make-parameter-ref-entry (get-CCSDS.Space-Packet.Header.Sequence-or-Name-Ref)))))

(defparameter CCSDS.Space-Packet.Container.Packet-Data-Field
  (let ((seq '()))
	(when (not (equal Space-Packet.Secondary-Header-Length 0))
	  (push (make-container-ref-entry "STC.CCSDS.Space-Packet.Packet-Data-Field.Container.Secondary-Header")
			seq))
	(push (make-parameter-ref-entry "STC.CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field")
		  seq)
	(make-sequence-container
	 "STC.CCSDS.Space-Packet.Container.Packet-Data-Field"
	 seq)))

(defparameter CCSDS.Space-Packet.Container.Secondary-Header ())

(defparameter CCSDS.Space-Packet.Container.User-Data-Field
  (make-sequence-container
   "STC.CCSDS.Space-Packet.Container.Packet-Data-Field.User-Data-Field"
   (list
	(make-parameter-ref-entry "STC.CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field"))))

(defparameter CCSDS.Space-Packet.Container.Space-Packet
  (let ((entry-list
		  (append
		   (list (make-container-ref-entry "STC.CCSDS.Space-Packet.Container.Packet-Primary-Header"))
		   (list (make-container-ref-entry "STC.CCSDS.Space-Packet.Container.Packet-Data-Field")))))
	(make-sequence-container
	 "STC.CCSDS.Space-Packet.Container.Space-Packet"
	 entry-list
	 :idle-pattern #*11111111111)))

(defun with-ccsds.space-packet.containers (container-list)
  (append
   container-list
   (list
	CCSDS.Space-Packet.Container.Space-Packet
	CCSDS.Space-Packet.Container.User-Data-Field
	CCSDS.Space-Packet.Container.Packet-Data-Field
	CCSDS.Space-Packet.Container.Header.Packet-Identification
	CCSDS.Space-Packet.Container.Header.Packet-Sequence-Control
	CCSDS.Space-Packet.Container.Packet-Primary-Header
	)
  (when (not (equal Space-Packet.Secondary-Header-Length 0))
	CCSDS.Space-Packet.Container.Secondary-Header)))

; Concrete Deframing: Stream + Frame Container -> Frames + Packet Container Ref
; -> Publish on AOS Service -> AOS Service Publishes by VCID

; Abstract Depacketization: AOS-Frame w/ VCID + container ref accepted by Depacketization Service -> Call for Specialized Depacketization Algorithm based on Container-Ref or Ancillary Data -> Use Container Set containing (Packet -> Packet Contents) to create concrete packets.
(defun make-space-packet-container (apid
									name
									entry-list
									&key
									  abstract
									  idle-pattern
									  short-description
									  long-description
									  alias-set
									  (ancillary-data-set (xtce::make-ancillary-data-set))
									  rate-in-stream-set
									  default-rate-in-stream
									  binary-encoding
									  base-container)
  (declare (ignore base-container))
  (with-slots (xtce::data-set) ancillary-data-set
	 (setf (gethash "apid" xtce::data-set) (xtce::make-ancillary-data "apid" apid))
	(make-sequence-container name
							 entry-list
							 :abstract abstract
							 :idle-pattern idle-pattern
							 :short-description short-description
							 :long-description long-description
							 :alias-set alias-set
							 :ancillary-data-set ancillary-data-set
							 :rate-in-stream-set rate-in-stream-set
							 :default-rate-in-stream default-rate-in-stream
							 :binary-encoding binary-encoding
							 :base-container "STC.CCSDS.Space-Packet")))

(defun stc.ccsds.space-packet.is-idle-pattern (apid)
  (with-slots (xtce::idle-pattern) CCSDS.Space-Packet.Container.Space-Packet
	(if (equal xtce::idle-pattern apid)
		t
		nil)))



(defun extract-space-packets-from-mpdu (data first-header-pointer symbol-table alist previous-packet-segment)
  (log:info "Attempting to extract space packets...")
  
  (when (stc::stc.ccsds.mpdu.is-idle-pattern first-header-pointer)
	(log:info "Found idle pattern.")
	(return-from extract-space-packets-from-mpdu nil))
  
  (let ((container stc::CCSDS.Space-Packet.Container.Space-Packet)
		(packet-list nil)
		(data-length (length data)))

	(when (stc::stc.ccsds.mpdu.is-spanning-pattern first-header-pointer)
	  (log:info "Attempting to reconstruct spanning packet.")
	  (if (eq previous-packet-segment #*)
		  (progn
			(log:info "Found spanning packet but we do not have a previous fragmented packet.")
			(return-from extract-space-packets-from-mpdu
			  (values nil
					  (lambda (next-data first-header-pointer symbol-table alist)
						(extract-space-packets-from-mpdu next-data first-header-pointer symbol-table alist nil)))))		  
		  (progn
			(log:info "Packet still fragmented.")
			(return-from extract-space-packets-from-mpdu
			  (values packet-list
					  (lambda (next-data first-header-pointer symbol-table alist)
						(extract-space-packets-from-mpdu next-data first-header-pointer symbol-table alist (concatenate-bit-arrays previous-packet-segment data))))))))
	
	(let* ((rear-fragment (subseq data 0 (* 8 first-header-pointer)))
		   (lead-fragment nil))
	  (unless (equal first-header-pointer 0)
		(log:info "Attempting to reconstruct fragmented packet.")
		(log:info previous-packet-segment)
		(if (equal previous-packet-segment #*)
		  (log:info "Found fragmented packet at the front of the MPDU but we did not see it's lead fragment in the last MPDU.")
		  (progn
			(let* ((reconstructed-packet (concatenate-bit-arrays previous-packet-segment rear-fragment))
				   (decoded-packet (decode reconstructed-packet container symbol-table alist 0)))
			  (log:info reconstructed-packet)
			  (if (stc::stc.ccsds.space-packet.is-idle-pattern
				   (cdr (assoc stc::"STC.CCSDS.Space-Packet.Header.Application-Process-Identifier" decoded-packet)))
				  (log:info "Idle packet restored; Discarding.")
				  (progn 
					(log:info "Restored packet!")
					(push decoded-packet packet-list)))))))
	  
	  (let ((next-pointer (* 8 first-header-pointer)))
		(log:debug "Attempting to extract packets starting from zero pointer.")
		(loop while (< next-pointer data-length)
			  do
				 (handler-case 
					 (multiple-value-bind (res-list bits-consumed) (decode data container symbol-table alist next-pointer)					   
					   (setf next-pointer bits-consumed)
					   (log:debug "Extracted ~A of ~A bytes" bits-consumed data-length)
					   (if (stc::stc.ccsds.space-packet.is-idle-pattern
							(cdr (assoc stc::"STC.CCSDS.Space-Packet.Header.Application-Process-Identifier" res-list)))
						   (log:debug "Found idle Packet!")
						   (push res-list packet-list)))
				   (fragmented-packet-error ()
					 ;; We hit this whenever the packet length tells us to subseq beyond the data frame
					 ;; This is fine, we just take the rest of the frame as a leading fragment
					 (log:info "Fragmented Packet!")
					 (setf lead-fragment (subseq data next-pointer))
					 (return))))
		(log:info "Extracted ~A packets." (length packet-list))
		(log:info (- data-length next-pointer))
		(log:info (length (subseq data next-pointer) ))
		(values packet-list (lambda (next-data first-header-pointer symbol-table alist)
							  (extract-space-packets-from-mpdu next-data first-header-pointer symbol-table alist lead-fragment)))))))


(defun decode-ccsds (data spec bit-offset)
  "Given 'data as a binary vector (e.g. #*0001000), decode data using 'spec, where spec may be a CCSDS container, parameter, or parameter-type.
   This function is intended as a utility for inspecting frame fragments.
   You may specify 'bit-offset in order to skip bits, which is useful when the binary was derived from an integral number of octets, but the 'spec is not (e.g In the CCSDS Space Packet Primary Header, The packet version number is 3 bits but the Packet Identification is 13 bits. We can set bit-offset to 3 and decode #*0000100010000000 derived from #x0880, which begins decoding as if were #*0100010000000).
  (decode-ccsds #*0000100010000000 CCSDS.Space-Packet.Container.Header.Packet-Identification 3)"
  (let ((test-table (xtce::register-keys-in-sequence
					 (funcall 
					 (alexandria:compose #'with-ccsds.space-packet.parameters
										 #'with-ccsds.space-packet.types
										 #'with-ccsds.space-packet.containers
										 #'with-ccsds.mpdu.containers
										 #'with-ccsds.mpdu.types
										 #'with-ccsds.mpdu.parameters
										 #'with-ccsds.aos.containers
										 #'with-ccsds.aos.header.parameters
										 #'with-ccsds.aos.header.types) nil)
					 (xtce::make-filesystem-hash-table)
					 'Test)))
	(xtce-engine::decode data spec test-table '() bit-offset)))


(defun build-apid->container-table (service-def symbol-table)
  (let ((apid-lookup-table (make-hash-table :test 'equalp)))
	(with-slots (reference-set) service-def
		  (progn
			(dolist (reference reference-set)
			  (print reference)
			  (let* ((packet-def (dereference reference symbol-table))
					 (apid (gethash "apid" (xtce::get-ancillary-data packet-def))))
				(setf (gethash apid apid-lookup-table) packet-def)))))
	apid-lookup-table))


(defun space-packet-service (data service-def symbol-table &key apid->container-table)
  (unless apid->container-table
	(setf apid->container-table (build-apid->container-table service-def symbol-table)))

  (let* ((space-packet (xtce-engine::decode data (gethash "STC.CCSDS.Space-Packet.Container.Space-Packet" symbol-table) symbol-table nil 0))
		 (apid (assoc "apid" space-packet))
		 (user-data (assoc "user-data-field" space-packet))
		 (result nil))
	(log:info space-packet)
	;(setf result (xtce-engine::decode user-data (gethash apid apid->container-table) symbol-table nil 0))
	(values result :ok (lambda (data service-def symbol-table)
						 (space-packet-service data service-def symbol-table :apid->container-table apid->container-table)))))



;; (log:config :debug)
;; (decode-ccsds (xtce-engine::hex-string-to-bit-vector "0870 fee6 001a 0000 2d9f 851e 0000 0000 0000 0000 0000 0000 0000 0000 0000 0000 0009 1aed 9e00 4d00 002d 9f8c cc00 0000") stc::CCSDS.Space-Packet.Container.Space-Packet  0)


;; (xtce-engine::u8-array->bit-vector
;;  (xtce-engine::hex-string-to-byte-array "000F"))


;; (xtce-engine::hex-string-to-bit-vector "0009 1aed 9e00 4d00 002d 9f8c cc00 0000")

