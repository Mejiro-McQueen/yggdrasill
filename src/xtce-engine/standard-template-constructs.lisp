(in-package :standard-template-constructs)
(use-package :xtce)

(defun CCSDS.Space-Packet.Header.Packet-Version-Number-Type ()
  (make-integer-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Version-Number-Type|
   :short-description "CCSDS Space Packet Header element."
   :long-description (make-long-description "CCSDS Space Packet Header element. Mandatory. 3 bit field fixed to 000.")
   :size-in-bits 3))

(defun CCSDS.Space-Packet.Header.Packet-Type-Type ()
  (make-enumerated-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Type-Type|
   :short-description "CCSDS Space Packet Header element."
   :long-description (make-long-description "CCSDS Space Packet Header element. The exact definition of ‘telemetry Packets’ and ‘telecommand Packets’ needs to be established by the project that uses this protocol. Element of Packet-Identification subdivision.")
   :enumeration-list
   (list (make-enumeration 'Telemetry 0) (make-enumeration 'Telecommand 1))))

(defun CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type ()
  (make-enumerated-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type|
   :short-description "CCSDS Space Packet Header element."
   :long-description (make-long-description "CCSDS Space Packet Header element. Indicates the presence or absence of the Packet Seconday Header within this space packet. This flag shall be static with respect to the APID and managed data path throughout a mission phase. Element of Packet-Identification subdivision.")
   :enumeration-list
   (list (make-enumeration 'Absent 0 :short-description "This packet contains a secondary header.")
		 (make-enumeration 'Present 1 :short-description "This packet does not contain a secondary header."))))

(defun CCSDS.Space-Packet.Header.Application-Process-Identifier-Type ()
  (make-binary-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier-Type|
   :short-description "CCSDS Space Packet Header element."))

(defun CCSDS.Space-Packet.Header.Packet-Identification-Type ()
  (make-binary-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Identification-Type|
   :short-description "CCSDS Space Packet Header element."))

(defun CCSDS.Space-Packet.Header.Packet-Sequence-Control-Type ()
  (make-binary-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Control-Type|
   :short-description "CCSDS Space Packet Header element."))

(defun CCSDS.Space-Packet.Header.Sequence-Flags-Type ()
  (make-enumerated-parameter-type
   '|STC:CCSDS.Space-Packet.Header.Sequence-Flags-Type|
   :enumeration-list (list
					  (make-enumeration #b00 'Continuation :short-description "Space Packet contains a continuation segment of User Data.")
					  (make-enumeration #b01 'First-Segment :short-description "Space Packet contains the first segment of User Data.")
					  (make-enumeration #b10 'Last-Segment :short-description "Space Packet contains the last segment of User Data.")
					  (make-enumeration #b11 'Unsegmented :short-description "Space Packet is unsegmented."))))

(defun CCSDS.Space-Packet.Header.Packet-Data-Length-Type ()
  (make-integer-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Data-Length-Type|
   :size-in-bits 16
   :short-description "CCSDS Space Packet Header Element."
   :long-description (make-long-description "Is one fewer than the length in octets of the Packet Data Field. Described by C=(Total Number of Octets in the Packet Data Field) -1")))

(defun CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type ()
  (make-integer-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type|
   :size-in-bits 14
   :short-description "CCSDS Space Packet Header Element. Part of Packet Sequence Control subdivision."
   :long-description (make-long-description "The CCSDS spec calls out for either a packet name or packet count.")))

(defun CCSDS.Space-Packet.Header.Packet-Name-Type ()
  (make-string-parameter-type
   '|STC.CCSDS.Space-Packet.Header.Packet-Name-Type|
   :short-description "Do not Use: I have no idea how it's supposed to work. CCSDS Space Packet Header Element. Part of Packet Sequence Control subdivision."
   :long-description (make-long-description "The CCSDS spec calls out for either a packet name or packet.")))

(defun with-ccsds.space-packet.header.types (type-list)
  (append type-list
		  (list
		   (CCSDS.Space-Packet.Header.Packet-Identification-Type)
		   (CCSDS.Space-Packet.Header.Packet-Sequence-Control-Type)
		   (CCSDS.Space-Packet.Header.Packet-Version-Number-Type)
		   (CCSDS.Space-Packet.Header.Packet-Type-Type)
		   (CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type)
		   (CCSDS.Space-Packet.Header.Application-Process-Identifier-Type)
		   (CCSDS.Space-Packet.Header.Sequence-Flags-Type)
		   (CCSDS.Space-Packet.Header.Packet-Data-Length-Type)
		   (CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type)
		   (CCSDS.Space-Packet.Header.Packet-Name-Type))))

(defun CCSDS.Space-Packet.Header.Packet-Transfer-Frame-Version-Number ()
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Transfer-Frame-Version-Number| '|STC.CCSDS.Space-Packet.Header.Packet-Version-Number-Type|))

(defun CCSDS.Space-Packet.Header.Packet-Type ()
	(make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Type| '|STC.CCSDS.Space-Packet.Header.Packet-Type-Type|))

(defun CCSDS.Space-Packet.Header.Secondary-Header-Flag ()
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag| '|STC.CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type|))

(defun CCSDS.Space-Packet.Header.Application-Process-Identifier ()
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier| '|STC.CCSDS.Space-Packet.Header.Application-Process-Identifier|))

(defun CCSDS.Space-Packet.Header.Packet-Identification ()
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Identification| '|STC.CCSDS.Space-Packet.Header.Packet-Identification-Type|))

(defun CCSDS.Space-Packet.Header.Sequence-Control ()
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Sequence-Control| '|STC.CCSDS.Space-Packet.Header.Sequence-Control-Type|))

(defun CCSDS.Space-Packet.Header.Sequence-Flags ()
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Sequence-Flags| '|STC.CCSDS.Space-Packet.Header.Sequence-Flags-Type|))

(defun CCSDS.Space-Packet.Header.Packet-Data-Length ()
	(make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Data-Length| '|STC.CCSDS.Space-Packet.Header.Packet-Data-Length-Type|))

(defun CCSDS.Space-Packet.Header.Packet-Sequence-Count ()
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count| '|STC.CCSDS.Space-Packet.Header.Packet-Sequence-Count-Type|))

(defun CCSDS.Space-Packet.Header.Packet-Name ()
  (make-parameter '|STC.CCSDS.Space-Packet.Header.Packet-Name| '|STC.CCSDS.Space-Packet.Header.Packet-Name-Type|))

(defun with-ccsds.space-packet.header.parameters (parameter-list)
  (append
   parameter-list
   (list
	(CCSDS.Space-Packet.Header.Packet-Transfer-Frame-Version-Number)
	(CCSDS.Space-Packet.Header.Packet-Type)
	(CCSDS.Space-Packet.Header.Secondary-Header-Flag)
	(CCSDS.Space-Packet.Header.Application-Process-Identifier)
	(CCSDS.Space-Packet.Header.Packet-Identification)
	(CCSDS.Space-Packet.Header.Sequence-Control)
	(CCSDS.Space-Packet.Header.Sequence-Flags)
	(CCSDS.Space-Packet.Header.Packet-Data-Length)
	(CCSDS.Space-Packet.Header.Packet-Sequence-Count)
	(CCSDS.Space-Packet.Header.Packet-Name))))

(defun CCSDS.AOS.Header.Master-Channel-ID-Type ()
  (make-binary-parameter-type '|STC.CCSDS.AOS.Header.Master-Channel-ID-Type|))

(defun CCSDS.AOS.Header.Signaling-Field-Type ()
  (make-binary-parameter-type '|STC.CCSDS.AOS.Header.Signaling-Field-Type|))

(defun CCSDS.AOS.Header.Version-Number-Type ()
  (make-binary-parameter-type
   '|STC.CCSDS.AOS.Header.Frame-Version-Number-Type|
   :short-description "2 bits fixed to 01"))

(defun CCSDS.AOS.Header.Spacecraft-Identifier-Type ()
  (make-binary-parameter-type '|STC.CCSDS.AOS.Header.Spacecraft-Identifier-Type| :short-description "8 bit SCID"))

(defun CCSDS.AOS.Header.Virtual-Channel-ID-Type ()
  (make-binary-parameter-type '|STC.CCSDS.AOS.Header.Virtual-Channel-ID-Type| :short-description "8 bit SCID"))

(defun CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Type ()
  (make-integer-parameter-type '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Type| :size-in-bits 24 :short-description "24 bit SCID"))

(defun CCSDS.AOS.Header.Replay-Flag-Type ()
  (make-binary-parameter-type '|STC.CCSDS.AOS.Header.Replay-Flag-Type| :short-description "boolean flag"))

(defun CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag-Type ()
  (make-binary-parameter-type '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag-Type| :short-description "boolean flag"))

(defun CCSDS.AOS.Header.Reserved-Spare-Type ()
  (make-binary-parameter-type '|STC.CCSDS.AOS.Header.Reserved-Spare-Type| :short-description "boolean flag"))

(defun CCSDS.AOS.Header.Frame-Count-Cycle-Type ()
  (make-binary-parameter-type '|STC.CCSDS.AOS.Header.Frame-Count-Cycle-Type|
   :short-description "Set to all zeros if not used. Otherwise, increments whenever the Frame Count rolls over, effectively extending it to 28 bits."))

(defun CCSDS.AOS.Header.Frame-Header-Error-Control-Type ()
  (make-binary-parameter-type '|STC.CCSDS.AOS.Header.Frame-Header-Error-Control-Type|
  :short-description "Optional. Reed-Solomon Protecting Mater Channel Identifier and Virtual Channel Identifier. 16 bits."))

(defun CCSDS.AOS.Header.Insert-Zone-Type ()
  (make-binary-parameter-type '|STC.CCSDS.AOS.Header.Insert-Zone-Type|
  :short-description "Optional."))

(defun CCSDS.AOS.Data-Field-Type ()
  (make-binary-parameter-type '|STC.CCSDS.AOS.Data-Field-Type|
  :short-description "M_PDU or B_PDU or VCA_SDU or Idle Data")
  :long-description (make-long-description "4.1.4.1.3 The Transfer Frame Data Field shall contain one Multiplexing Protocol Data Unit
(M_PDU), one Bitstream Protocol Data Unit (B_PDU), one Virtual Channel Access Service
Data Unit (VCA_SDU), or Idle Data. \n M_PDUs, B_PDUs, VCA_SDUs, and Idle Data shall not be mixed in a Virtual
Channel (i.e., if a Virtual Channel transfers M_PDUs, every Transfer Frame of that Virtual
Channel shall contain an M_PDU). Management shall decide whether M_PDUs, B_PDUs or
VCA_SDUs are transferred on a particular Virtual Channel, and this decision shall remain
static throughout a Mission Phase."))

(defun with-ccsds.aos.header.types (type-list)
  (append type-list
		  (list
		   (CCSDS.AOS.Header.Frame-Count-Cycle-Type)
		   (CCSDS.AOS.Header.Frame-Header-Error-Control-Type)
		   (CCSDS.AOS.Header.Insert-Zone-Type)
		   (CCSDS.AOS.Header.Master-Channel-ID-Type)
		   (CCSDS.AOS.Header.Replay-Flag-Type)
		   (CCSDS.AOS.Header.Reserved-Spare-Type)
		   (CCSDS.AOS.Header.Signaling-Field-Type)
		   (CCSDS.AOS.Header.Spacecraft-Identifier-Type)
		   (CCSDS.AOS.Header.Version-Number-Type)
		   (CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Type)
		   (CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag-Type)
		   (CCSDS.AOS.Header.Virtual-Channel-ID-Type)
		   )))


(defun CCSDS.AOS.Header.Master-Channel-ID ()
  (make-parameter '|STC.CCSDS.AOS.Header.Master-Channel-ID| '|STC.CCSDS.AOS.Header.Master-Channel-ID-Type|))

(defun CCSDS.AOS.Header.Signaling-Field ()
  (make-parameter '|STC.CCSDS.AOS.Header.Signaling-Field| '|STC.CCSDS.AOS.Header.Signaling-Field-Type|))

(defun CCSDS.AOS.Header.Virtual-Channel-ID ()
  (make-parameter '|STC.CCSDS.AOS.Header.Virtual-Channel-ID| '|STC.CCSDS.AOS.Header.Virtual-Channel-ID-Type|))

(defun CCSDS.AOS.Header.Virtual-Channel-Frame-Count ()
  (make-parameter '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count| '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Type|))

(defun CCSDS.AOS.Header.Replay-Flag ()
  (make-parameter '|STC.CCSDS.AOS.Header.Replay-Flag| '|STC.CCSDS.AOS.Header.Replay-Flag|))

(defun CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag ()
  (make-parameter '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag| '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag-Type|))

(defun CCSDS.AOS.Header.Reserved-Spare ()
  (make-parameter '|STC.CCSDS.AOS.Header.Reserved-Spare| '|STC.CCSDS.AOS.Header.Reserved-Spare-Type|))

(defun CCSDS.AOS.Header.Frame-Count-Cycle ()
  (make-parameter '|STC.CCSDS.AOS.Header.Frame-Count-Cycle| '|STC.CCSDS.AOS.Header.Frame-Count-Cycle-Type|))

(defun CCSDS.AOS.Header.Frame-Header-Error-Control ()
  (make-parameter '|STC.CCSDS.AOS.Header.Frame-Header-Error-Control| '|STC.CCSDS.AOS.Header.Frame-Header-Error-Control-Type|))

(defun CCSDS.AOS.Header.Insert-Zone ()
  (make-parameter '|STC.CCSDS.AOS.Header.Insert-Zone| '|STC.CCSDS.AOS.Header.Insert-Zone-Type|))

(defun CCSDS.AOS.Header.Data-Field ()
  (make-parameter '|STC.CCSDS.AOS.Header.Data-Field| '|STC.CCSDS.AOS.Header.Data-Field-Type|))

(defun with-ccsds.aos.header.parameters (parameter-list)
  (append parameter-list
		  (list
		   (CCSDS.AOS.Header.Master-Channel-ID)
		   (CCSDS.AOS.Header.Signaling-Field)
		   (CCSDS.AOS.Header.Virtual-Channel-ID)
		   (CCSDS.AOS.Header.Virtual-Channel-Frame-Count)
		   (CCSDS.AOS.Header.Replay-Flag)
		   (CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag)
		   (CCSDS.AOS.Header.Reserved-Spare)
		   (CCSDS.AOS.Header.Frame-Count-Cycle)
		   (CCSDS.AOS.Header.Frame-Header-Error-Control)
		   (CCSDS.AOS.Header.Insert-Zone)
		   (CCSDS.AOS.Header.Data-Field))))

(defun CCSDS.MPDU.Header.Reserved-Spare-Type ()
  (make-binary-parameter-type '|CCSDS.MPDU.Header.Reserved-Spare-Type|
  :short-description "5 bits. All zero."))

(defun CCSDS.MPDU.Header.First-Header-Pointer-Type ()
  (make-binary-parameter-type '|CCSDS.MPDU.Header.First-Header-Pointer-Type|
  :short-description "11 bit Integer position of first octet."))

(defun CCSDS.MPDU.Packet-Zone-Type ()
  (make-binary-parameter-type '|CCSDS.MPDU.Packet-Zone-Type|
  :short-description "Contains a series of MPDU"))
 
(defun with-ccsds.mpdu.header.types (type-list)
  (append
   type-list
   (list
	(CCSDS.MPDU.Header.Reserved-Spare-Type)
	(CCSDS.MPDU.Header.First-Header-Pointer-Type)
	(CCSDS.MPDU.Packet-Zone-Type))))

(defun CCSDS.MPDU.Header.Reserved-Spare ()
  (make-parameter '|STC.CCSDS.MPDU.Header.Reserved-Spare| '|STC.CCSDS.MPDU.Header.Reserved-Spare-Type|))

(defun CCSDS.MPDU.Header.First-Header-Pointer ()
  (make-parameter '|STC.CCSDS.MPDU.Header.First-Header-Pointer| '|STC.CCSDS.MPDU.Header.First-Header-Pointer-Type|))
  
(defun CCSDS.MPDU.Header.Packet-Zone ()
  (make-parameter '|STC.CCSDS.MPDU.Header.Packet-Zone| '|STC.CCSDS.MPDU.Header.Packet-Zone-Type|))

(defun with-ccsds.mpdu.header.parameters (parameter-list)
  (append parameter-list
		  (list
		   (CCSDS.MPDU.Header.Reserved-Spare)
		   (CCSDS.MPDU.Header.First-Header-Pointer)
		   (CCSDS.MPDU.Header.Packet-Zone))))

