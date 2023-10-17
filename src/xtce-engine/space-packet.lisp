(in-package :standard-template-constructs)
(use-package :xtce)

;TODO: Deal with secondary header

(defparameter Space-Packet.Header.Sequence-or-Name 'sequence)

(defparameter Space-Packet.Secondary-Header nil)

(defparameter Space-Packet.Secondary-Header-Type nil)

(defvar Space-Packet.Header.Packet-Name '|STC.CCSDS.Space-Packet.Header.Packet-Name|)

(defvar Space-Packet.Header.Packet-Name-Type '|STC.CCSDS.Space-Packet.Header.Packet-Name-Type|)

(defvar Space-Packet.Header.Packet-Count '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count|)

(defvar Space-Packet.Header.Packet-Name  '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type|)

(defun set-CCSDS.Space-Packet.Header.Sequence-or-Name (a)
  (assert (member (intern (format nil "~A" a) :stc) '(sequence name)) (a) "Option ~a must be one of <'sequence|'name>" a
  (setf Space-Packet.Header.Sequence-or-Name a)))

(defun get-CCSDS.Space-Packet.Header.Sequence-or-Name-Type ()
  (case Space-Packet.Header.Sequence-or-Name
	(sequence
	 CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type)
	(name
	 CCSDS.Space-Packet.Header.Packet-Name-Type)))

(defun get-CCSDS.Space-Packet.Header.Sequence-or-Name-Ref ()
  (assert (member Space-Packet.Header.Sequence-or-Name '(sequence name)) (Space-Packet.Header.Sequence-or-Name)
		  "Invalid Option ~A: Must be 'name or 'sequence"  Space-Packet.Header.Sequence-or-Name)
  (case Space-Packet.Header.Sequence-or-Name
	(sequence
	 Space-Packet.Header.Packet-Count)
	(name
	 Space-Packet.Header.Packet-Name)))

(defun set-CCSDS.Space-Packet.Secondary-Header (parameter)
  (setf Space-Packet.Secondary-Header parameter))

(defun set-CCSDS.Space-Packet.Secondary-Header-Type (parameter-type)
  (setf Space-Packet.Secondary-Header-Type parameter-type))

(defun get-CCSDS.Space-Packet.Header.Sequence-or-Name ()
  (assert (member Space-Packet.Header.Sequence-or-Name '(sequence name)) (Space-Packet.Header.Sequence-or-Name)
		  "Invalid Option ~A: Must be 'name or 'sequence"  Space-Packet.Header.Sequence-or-Name)
  (case Space-Packet.Header.Sequence-or-Name
	(sequence
	 CCSDS.Space-Packet.Header.Packet-Sequence-Count)
	(name
	 CCSDS.Space-Packet.Header.Packet-Name)))

(defvar CCSDS.Space-Packet.Header.Packet-Version-Number-Type
  (make-integer-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Version-Number-Type|
   :short-description "CCSDS Space Packet Header element."
   :long-description (make-long-description "CCSDS Space Packet Header element. Mandatory. 3 bit field fixed to 000.")
   :data-encoding (make-integer-data-encoding :size-in-bits 3)
   ))

(defvar CCSDS.Space-Packet.Header.Packet-Type-Type
  (make-enumerated-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Type-Type|
   :short-description "CCSDS Space Packet Header element."
   :long-description (make-long-description "CCSDS Space Packet Header element. The exact definition of ‘telemetry Packets’ and ‘telecommand Packets’ needs to be established by the project that uses this protocol. Element of Packet-Identification subdivision.")
   :enumeration-list
   (list (make-enumeration 'Telemetry 0) (make-enumeration 'Telecommand 1))
   :data-encoding (make-integer-data-encoding :size-in-bits 1)))

(defvar CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type
  (make-enumerated-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type|
   :short-description "CCSDS Space Packet Header element."
   :long-description (make-long-description "CCSDS Space Packet Header element. Indicates the presence or absence of the Packet Seconday Header within this space packet. This flag shall be static with respect to the APID and managed data path throughout a mission phase. Element of Packet-Identification subdivision.")
   :enumeration-list
   (list (make-enumeration 'Absent 0 :short-description "This packet contains a secondary header.")
		 (make-enumeration 'Present 1 :short-description "This packet does not contain a secondary header."))
   :data-encoding (make-integer-data-encoding :size-in-bits 1)))

(defvar CCSDS.Space-Packet.Header.Application-Process-Identifier-Type
  (make-binary-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier-Type|
   :short-description "CCSDS Space Packet Header element."
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 14)))))

(defvar CCSDS.Space-Packet.Header.Packet-Identification-Type
  (make-binary-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Identification-Type|
   :short-description "CCSDS Space Packet Header element."
   :data-encoding
   (make-binary-data-encoding (make-size-in-bits (make-fixed-value 16)))))

(defvar CCSDS.Space-Packet.Header.Packet-Sequence-Control-Type
  (make-binary-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Control-Type|
   :short-description "CCSDS Space Packet Header element."
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 16)))))

(defvar CCSDS.Space-Packet.Header.Sequence-Flags-Type
  (make-enumerated-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Sequence-Flags-Type|
   :enumeration-list (list
					  (make-enumeration #b00 'Continuation :short-description "Space Packet contains a continuation segment of User Data.")
					  (make-enumeration #b01 'First-Segment :short-description "Space Packet contains the first segment of User Data.")
					  (make-enumeration #b10 'Last-Segment :short-description "Space Packet contains the last segment of User Data.")
					  (make-enumeration #b11 'Unsegmented :short-description "Space Packet is unsegmented."))
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 14)))))

(defvar CCSDS.Space-Packet.Header.Packet-Data-Length-Type
  (make-integer-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Data-Length-Type|
   :size-in-bits 16
   :short-description "CCSDS Space Packet Header Element."
   :long-description (make-long-description "Is one fewer than the length in octets of the Packet Data Field. Described by C=(Total Number of Octets in the Packet Data Field) -1")
   :data-encoding (make-integer-data-encoding :size-in-bits 16)))

(defvar CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type
  (make-integer-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type|
   :size-in-bits 14
   :short-description "CCSDS Space Packet Header Element. Part of Packet Sequence Control container."
   :long-description (make-long-description "The CCSDS spec calls out for either a packet name or packet count.")
  :data-encoding (make-integer-data-encoding :size-in-bits 14)))

(defvar CCSDS.Space-Packet.Header.Packet-Name-Type
  (make-string-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Name|
   :short-description "Part of Packet Sequence Control container."
   :long-description (make-long-description "The CCSDS spec calls out for either a packet name or packet.")
   :data-encoding (make-string-data-encoding (make-size-in-bits (make-fixed (make-fixed-value 14))))))

(defvar CCSDS.Space-Packet.User-Data-Field-Type
  (make-string-parameter-type
   '|STC.CCSDS.Space-Packet.User-Data-Field-Type|
   :short-description "Bytes containing the packet payload"
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-dynamic-value (make-parameter-instance-ref '|STC.CCSDS.Space-Packet.Header.Packet-Data-Length|))))))

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
		   CCSDS.Space-Packet.User-Data-Field-Type
		   (get-CCSDS.Space-Packet.Header.Sequence-or-Name-Type))))

(defun with-ccsds.space-packet.types (type-list)
  (with-ccsds.space-packet.header.types type-list))

(defvar CCSDS.Space-Packet.Header.Packet-Transfer-Frame-Version-Number
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Transfer-Frame-Version-Number| '|STC.CCSDS.Space-Packet.Header.Packet-Version-Number-Type|))

(defvar CCSDS.Space-Packet.Header.Packet-Type
	(make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Type| '|STC.CCSDS.Space-Packet.Header.Packet-Type-Type|))

(defvar CCSDS.Space-Packet.Header.Secondary-Header-Flag
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag| '|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type|))

(defvar CCSDS.Space-Packet.Header.Application-Process-Identifier
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier| '|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier-Type|))

(defvar CCSDS.Space-Packet.Header.Packet-Identification
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Identification| '|STC.CCSDS.Space-Packet.Header.Packet-Identification-Type|))

(defvar CCSDS.Space-Packet.Header.Packet-Sequence-Control
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Control| '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Control-Type|))

(defvar CCSDS.Space-Packet.Header.Sequence-Flags
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Sequence-Flags| '|STC.CCSDS.Space-Packet.Header.Sequence-Flags-Type|))

(defvar CCSDS.Space-Packet.Header.Packet-Data-Length
	(make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Data-Length| '|STC.CCSDS.Space-Packet.Header.Packet-Data-Length-Type|))

(defvar CCSDS.Space-Packet.User-Data-Field
	(make-parameter '|STC.CCSDS.Space-Packet.User-Data-Field| '|STC.CCSDS.Space-Packet.User-Data-Field-Type|))

(defvar CCSDS.Space-Packet.Header.Packet-Sequence-Count
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count| '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type|))

(defvar CCSDS.Space-Packet.Header.Packet-Name
  (make-parameter Space-Packet.Header.Packet-Name Space-Packet.Header.Packet-Name-Type))

(defun with-ccsds.space-packet.header.parameters (parameter-list)
  (append
   parameter-list
   (list
	CCSDS.Space-Packet.Header.Packet-Transfer-Frame-Version-Number
	CCSDS.Space-Packet.Header.Packet-Type
	CCSDS.Space-Packet.Header.Secondary-Header-Flag
	CCSDS.Space-Packet.Header.Application-Process-Identifier
	CCSDS.Space-Packet.Header.Packet-Identification
	CCSDS.Space-Packet.Header.Packet-Sequence-Control
	CCSDS.Space-Packet.Header.Sequence-Flags
	CCSDS.Space-Packet.Header.Packet-Data-Length
	(get-CCSDS.Space-Packet.Header.Sequence-or-Name))))

(defun with-ccsds.space-packet.parameters (parameter-list)
  (with-ccsds.space-packet.header.parameters parameter-list)
  )

(defvar CCSDS.Space-Packet.Container.Packet-Primary-Header
  (make-sequence-container
   '|STC.CCSDS.Space-Packet.Container.Packet-Primary-Header|
   (list
	(make-parameter-ref-entry '|STC.CCSDS.Space-Packet.Header.Packet-Version-Number|)
	(make-container-ref-entry '|STC.CCSDS.Space-Packet.Container.Header.Packet-Identification|)
	(make-container-ref-entry '|STC.CCSDS.Space-Packet.Container.Packet-Sequence-Control|)
	(make-parameter-ref-entry '|STC.CCSDS.Space-Packet.Header.Packet-Data-Length|))))

(defvar CCSDS.Space-Packet.Container.Header.Packet-Identification
  (make-sequence-container
   '|STC.CCSDS.Space-Packet.Container.Header.Packet-Identification|
   (list
	(make-parameter-ref-entry '|STC.CCSDS.Space-Packet.Header.Packet-Type|)
	(make-parameter-ref-entry '|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag|)
	(make-parameter-ref-entry '|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier|))))

(defvar CCSDS.Space-Packet.Container.Header.Packet-Sequence-Control
  (make-sequence-container
   '|STC.CCSDS.Space-Packet.Container.Header.Packet-Sequence-Control|
   (list
	(make-parameter-ref-entry '|STC.CCSDS.Space-Packet.Header.Sequence-Flags|)
	(make-parameter-ref-entry (get-CCSDS.Space-Packet.Header.Sequence-or-Name-Ref)))))

(defvar CCSDS.Space-Packet.Container.Packet-Data-Field
										;TODO: Consider feature flag for secondary header?
  (let ((seq '()))
	(when Space-Packet.Secondary-Header
	  (push (make-container-ref-entry '|STC.CCSDS.Space-Packet.Packet-Data-Field.Container.Secondary-Header|)
			seq))
	(push (make-parameter-ref-entry '|STC.CCSDS.Space-Packet.Packet-Data-Field.User-Data-Field|)
		  seq)
	(make-sequence-container
	 '|STC.CCSDS.Space-Packet.Container.Packet-Data-Field|
	 seq)))

(defvar CCSDS.Space-Packet.Container.Secondary-Header ())

(defvar CCSDS.Space-Packet.Container.User-Data-Field
  (make-sequence-container
   '|CCSDS.Space-Packet.Container.User-Data-Field|
   (list
	(make-parameter-ref-entry '|CCSDS.Space-Packet.User-Data-Field|))))

(defvar CCSDS.Space-Packet.Container.Space-Packet
  (let ((entry-list
		  (append
		   (list (make-container-ref-entry '|STC.CCSDS.Space-Packet.Container.Packet-Primary-Header|))
		   (when Space-Packet.Secondary-Header nil) ;TODO: UGH, also we should check SANA
		   (list (make-container-ref-entry '|STC.CCSDS.Space-Packet.Container.Packet-Data-Field|)))))
	(make-sequence-container
	 '|STC.CCSDS.Space-Packet.Container.Space-Packet|
	 entry-list)))

(defun with-ccsds.space-packet.containers (container-list)
  (append
   container-list
   (list
	CCSDS.Space-Packet.Container.Space-Packet
	CCSDS.Space-Packet.Container.User-Data-Field
	CCSDS.Space-Packet.Container.Packet-Data-Field
	CCSDS.Space-Packet.Container.Header.Packet-Identification
	CCSDS.Space-Packet.Container.Header.Packet-Sequence-Control)
  (when Space-Packet.Secondary-Header
	CCSDS.Space-Packet.Container.Secondary-Header)))

; Concrete Deframing: Stream + Frame Container -> Frames + Packet Container Ref
; -> Publish on AOS Service -> AOS Service Publishes by VCID

; Abstract Depacketization: AOS-Frame w/ VCID + container ref accepted by Depacketization Service -> Call for Specialized Depacketization Algorithm based on Container-Ref or Ancillary Data -> Use Container Set containing (Packet -> Packet Contents) to create concrete packets.
(defun make-space-packet-container (name
									apid
									entry-list
									&key
									  abstract
									  idle-pattern
									  short-description
									  long-description
									  alias-set
									  ancillary-data-set
									  rate-in-stream-set
									  default-rate-in-stream
									  binary-encoding
									  base-container)
  (declare (ignore base-container))
  (let ((apid-data (list (make-ancillary-data '|apid| apid))))
	(make-sequence-container name
							 entry-list
							 :abstract abstract
							 :idle-pattern idle-pattern
							 :short-description short-description
							 :long-description long-description
							 :alias-set alias-set
							 :ancillary-data-set (append ancillary-data-set apid-data)
							 :rate-in-stream-set rate-in-stream-set
							 :default-rate-in-stream default-rate-in-stream
							 :binary-encoding binary-encoding
							 :base-container '|STC.CCSDS.Space-Packet|)))

