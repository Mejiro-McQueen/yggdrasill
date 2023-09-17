(in-package :standard-template-constructs)
(use-package :xtce)

(defvar CCSDS.MPDU.Header.Reserved-Spare-Type
  (make-binary-parameter-type
   '|STC.CCSDS.MPDU.Header.Reserved-Spare-Type|
   :short-description "5 bits. All zero."
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 5)))))

(defvar CCSDS.MPDU.Header.First-Header-Pointer-Type
  (make-binary-parameter-type '|STC.CCSDS.MPDU.Header.First-Header-Pointer-Type|
  :short-description "11 bit Integer position of first octet."
  :data-encoding (make-integer-data-encoding :size-in-bits 11)))

(defvar CCSDS.MPDU.Packet-Zone-Type
  (make-binary-parameter-type '|STC.CCSDS.MPDU.Packet-Zone-Type|
  :short-description "Contains a series of MPDU"))

(defvar CCSDS.MPDU.Packet-Type
  (make-binary-parameter-type '|STC.CCSDS.MPDU.Packet-Type|
  :short-description "Contains a series of MPDU"))

(defun with-ccsds.mpdu.header.types (type-list)
  (append
   type-list
   (list
	CCSDS.MPDU.Header.Reserved-Spare-Type
	CCSDS.MPDU.Header.First-Header-Pointer-Type
	CCSDS.MPDU.Packet-Zone-Type
	CCSDS.MPDU.Packet-Type
	)))

(defvar CCSDS.MPDU.Header.Reserved-Spare
  (make-parameter '|STC.CCSDS.MPDU.Header.Reserved-Spare| '|STC.CCSDS.MPDU.Header.Reserved-Spare-Type|))

(defvar CCSDS.MPDU.Header.First-Header-Pointer
  (make-parameter '|STC.CCSDS.MPDU.Header.First-Header-Pointer| '|STC.CCSDS.MPDU.Header.First-Header-Pointer-Type|))
  
(defvar CCSDS.MPDU.Packet-Zone
  (make-parameter '|STC.CCSDS.MPDU.Packet-Zone| '|STC.CCSDS.MPDU.Packet-Zone-Type|))

(defvar CCSDS.MPDU.Packet
  (make-parameter '|STC.CCSDS.MPDU.Packet| '|STC.CCSDS.MPDU.Packet-Type|))

(defun with-ccsds.mpdu.header.parameters (parameter-list)
  (append parameter-list
		  (list
		   CCSDS.MPDU.Header.Reserved-Spare
		   CCSDS.MPDU.Header.First-Header-Pointer
		   CCSDS.MPDU.Packet-Zone)))

(defvar CCSDS.MPDU
  (make-sequence-container
   '|STC.CCSDS.MPDU|
   (list
	(make-parameter-ref-entry '|STC.CCSDS.MPDU.Header.Reserved-Spare|)
	(make-parameter-ref-entry '|STC.CCSDS.MPDU.Header.First-Header-Pointer|)
	(make-container-ref-entry '|STC.CCSDS.MPDU.Packet-Zone|))))

(defvar CCSDS.MPDU-Packet-Zone
  (make-sequence-container
   '|STC.CCSDS.MPDU.Packet-Zone|
   (list
	(make-container-ref-entry '|STC.CCSDS.Space-Packet| :short-description "Request Auto MPDU Depacketize"))))

;Use binary sequence encoding on container to turn m_pdu packet zone to packets, then reference the packet container
