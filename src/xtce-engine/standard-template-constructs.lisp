(in-package :standard-template-constructs)
(use-package :xtce)

(defun CCSDS.Space-Packet.Header.Packet-Version-Number-Type ()
  `(make-integer-parameter-type
	'|STC.CCSDS.Packet-Version-Number-Type|
	:short-description "CCSDS Space Packet Header element."
	:long-description (make-long-description "CCSDS Space Packet Header element. Mandatory. 3 bit field fixed to 000.")
	:size-in-bits 3))

(defun CCSDS.Space-Packet.Header.Packet-Type ()
  `(make-enumerated-parameter-type
	'|STC.CCSDS.Packet-Type|
	:short-description "CCSDS Space Packet Header element."
	:long-description (make-long-description "CCSDS Space Packet Header element. The exact definition of ‘telemetry Packets’ and ‘telecommand Packets’ needs to be established by the project that uses this protocol. Element of Packet-Identification subdivision.")
	:enumeration-list
	(make-enumeration-list (make-enumeration 'Telemetry 0) (make-enumeration 'Telecommand 1))))

(defun CCSDS.Space-Packet.Header.Secondary-Header-Flag-Type ()
  `(make-enumerated-parameter-type
	'|STC.CCSDS.Secondary-Header-Flag-Type|
	:short-description "CCSDS Space Packet Header element."
	:long-description (make-long-description "CCSDS Space Packet Header element. Indicates the presence or absence of the Packet Seconday Header within this space packet. This flag shall be static with respect to the APID and managed data path throughout a mission phase. Element of Packet-Identification subdivision.")
	:enumeration-list
	(make-enumeration-list (make-enumeration 'Absent 0 :short-description "This packet contains a secondary header.")
						   (make-enumeration 'Present 1 :short-description "This packet does not contain a secondary header."))))

(defun CCSDS.Space-Packet.Header.Applicaiton-Process-Identifier-Type ()
  `(make-binary-parameter-type
	'|STC.CCSDS.Application-Process-Identifier-Type|
	:short-description "CCSDS Space Packet Header element."))

(defun CCSDS.Space-Packet.Header.Sequence-Flags-Type ()
  (make-enumerated-parameter-type
   '|STC:CCSDS.Sequence-Flags-Type|
   :enumeration-list (make-enumeration-list
					  (make-enumeration #b00 'Continuation :short-description "Space Packet contains a continuation segment of User Data.")
					  (make-enumeration #b01 'First-Segment :short-description "Space Packet contains the first segment of User Data.")
					  (make-enumeration #b10 'Last-Segment :short-description "Space Packet contains the last segment of User Data.")
					  (make-enumeration #b11 'Unsegmented :short-description "Space Packet is unsegmented."))))

(defun CCSDS.Space-Packet.Header.Packet-Data-Length-Type ()
  `(make-integer-parameter-type
	'|STC.CCSDS.Packet-Data-Length-Type|
	:size-in-bits 16
	:short-description "CCSDS Space Packet Header Element."
	:long-description (make-long-description "Is one fewer than the length in octets of the Packet Data Field. Described by C=(Total Number of Octets in the Packet Data Field) -1")))

(defun CCSDS.Space-Packet.Header.Packet-Sequence-Count ()
  `(make-integer-parameter-type
	'|STC.CCSDS.Packet-Sequence-Count|
	:size-in-bits 14
	:short-description "CCSDS Space Packet Header Element. Part of Packet Sequence Control subdivision."
	:long-description (make-long-description "The CCSDS spec calls out for either a packet name or packet count.")))

(defun CCSDS.Space-Packet.Header.Packet-Name ()
  `(make-string-parameter-type
	'|STC.CCSDS.Packet-Name|
	:short-description "Do not Use: I have no idea how it's supposed to work. CCSDS Space Packet Header Element. Part of Packet Sequence Control subdivision."
	:long-description (make-long-description "The CCSDS spec calls out for either a packet name or packet.")))

(defun CCSDS.Space-Packet.Header.Types ()
  `(CCSDS.Space-Packet.Header.Packet-Version-Number-Type))
