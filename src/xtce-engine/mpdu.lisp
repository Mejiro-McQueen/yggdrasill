(in-package :standard-template-constructs)
(use-package :xtce)

(defvar CCSDS.MPDU.Transfer-Zone.Packet-Zone-Length
  (- AOS.Transfer-Frame-Data-Field-Length 16))

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
  :short-description "Contains a series of MPDU"
  :data-encoding (make-binary-data-encoding
				  (make-size-in-bits
				   (make-fixed-value CCSDS.MPDU.Transfer-Zone.Packet-Zone-Length)))))

(defvar CCSDS.MPDU.Packet-Type
  (make-binary-parameter-type '|STC.CCSDS.MPDU.Packet-Type|
  :short-description "Contains a series of MPDU"))

(defun with-ccsds.mpdu.types (type-list)
  (append
   type-list
   (list
	CCSDS.MPDU.Header.Reserved-Spare-Type
	CCSDS.MPDU.Header.First-Header-Pointer-Type
	CCSDS.MPDU.Packet-Zone-Type
	CCSDS.MPDU.Packet-Type)))

(defvar CCSDS.MPDU.Header.Reserved-Spare
  (make-parameter '|STC.CCSDS.MPDU.Header.Reserved-Spare| '|STC.CCSDS.MPDU.Header.Reserved-Spare-Type|))

(defvar CCSDS.MPDU.Header.First-Header-Pointer
  (make-parameter '|STC.CCSDS.MPDU.Header.First-Header-Pointer| '|STC.CCSDS.MPDU.Header.First-Header-Pointer-Type|))
  
(defvar CCSDS.MPDU.Packet-Zone
  (make-parameter
   '|STC.CCSDS.MPDU.Packet-Zone| '|STC.CCSDS.MPDU.Packet-Zone-Type|))

(defvar CCSDS.MPDU.Packet
  (make-parameter '|STC.CCSDS.MPDU.Packet| '|STC.CCSDS.MPDU.Packet-Type|))

(defun with-ccsds.mpdu.parameters (parameter-list)
  (append parameter-list
		  (list
		   CCSDS.MPDU.Header.Reserved-Spare
		   CCSDS.MPDU.Header.First-Header-Pointer
		   CCSDS.MPDU.Packet-Zone)))

(defvar CCSDS.MPDU.Container.Header
  (make-sequence-container
   '|STC.CCSDS.MPDU.Container.Header|
   (list
	(make-parameter-ref-entry '|STC.CCSDS.MPDU.Header.Reserved-Spare|)
	(make-parameter-ref-entry '|STC.CCSDS.MPDU.Header.First-Header-Pointer|))))

(defvar CCSDS.MPDU.Container.Packet-Zone
  (make-sequence-container
   '|STC.CCSDS.MPDU.Container.Packet-Zone|
   (list
	(make-parameter-ref-entry '|STC.CCSDS.MPDU.Packet-Zone|))))

(defvar CCSDS.MPDU.Container.MPDU
  (make-sequence-container
   '|STC.CCSDS.MPDU.Container.MPDU|
   (list
	(make-container-ref-entry '|STC.CCSDS.MPDU.Container.Header|)
	(make-container-ref-entry '|STC.CCSDS.MPDU.Container.Packet-Zone|))))

;Use binary sequence encoding on container to turn m_pdu packet zone to packets, then reference the packet container

(defun with-ccsds.mpdu.containers (parameter-list)
  (append parameter-list
		  (list
		   CCSDS.MPDU.Container.MPDU
		   CCSDS.MPDU.Container.Header
		   CCSDS.MPDU.Container.Packet-Zone)))

(defun stc.ccsds.mpdu.is-spanning-pattern (first-header-pointer)
	(if (eq first-header-pointer #b11111111111)
		t
		nil))

(defun stc.ccsds.mpdu.is-idle-pattern (first-header-pointer)
  (if (eq first-header-pointer #b11111111110) ;equiv to (- #b11111111111 1)
		t
		nil))
