(in-package :standard-template-constructs)
(use-package :xtce)

(defparameter use-AOS.Operational-Control-Field nil)

(defparameter AOS.Insert-Zone-Length 0)

(defparameter use-AOS.Frame-Error-Control-Field nil)

(defparameter use-AOS.Header.Frame-Header-Error-Control-Field nil)

(defun set-CCSDS.AOS.Set-Insert-Zone-Length (n)
  (setf AOS.Insert-Zone-Length n))

(defun set-CCSDS.AOS.Frame-Error-Control-Field ())

(defun set-CCSDS.AOS.Operational-Control-Field ())

(defvar CCSDS.AOS.Header.Master-Channel-ID-Type
  (make-binary-parameter-type
   '|STC.CCSDS.AOS.Header.Master-Channel-ID-Type|
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 16)))))

(defvar CCSDS.AOS.Header.Signaling-Field-Type
  (make-binary-parameter-type
   '|STC.CCSDS.AOS.Header.Signaling-Field-Type|
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 8)))))

(defvar CCSDS.AOS.Header.Version-Number-Type
  (make-binary-parameter-type
   '|STC.CCSDS.AOS.Header.Frame-Version-Number-Type|
   :short-description "2 bits fixed to 01"
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 2)))))

(defvar CCSDS.AOS.Header.Spacecraft-Identifier-Type
  (make-binary-parameter-type
   '|STC.CCSDS.AOS.Header.Spacecraft-Identifier-Type|
   :short-description "8 bit SCID"
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 8)))))

(defvar CCSDS.AOS.Header.Virtual-Channel-ID-Type
  (make-binary-parameter-type
   '|STC.CCSDS.AOS.Header.Virtual-Channel-ID-Type|
   :short-description "6 channel ID "
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 6)))))

(defvar CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Type
  (make-integer-parameter-type
   '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Type|
   :short-description "24 bit positive integer counter"
   :data-encoding (make-integer-data-encoding :size-in-bits 24)))

(defvar CCSDS.AOS.Header.Replay-Flag-Type
  (make-enumerated-parameter-type
   '|STC.CCSDS.AOS.Header.Replay-Flag-Type|
   :short-description "boolean flag"
   :enumeration-list (list (make-enumeration #b0 '|Realtime-Transfer-Frame|)
						   (make-enumeration #b1 '|Replay-Transfer-Frame|))
   :data-encoding (boolean-flag)))

(defvar CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag-Type
  (make-enumerated-parameter-type
   '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag-Type|
   :short-description "boolean flag"
   :enumeration-list (list (make-enumeration #b0 '|Ignored|)
						   (make-enumeration #b1 '|Interpreted|))
   :data-encoding (boolean-flag)))

(defvar CCSDS.AOS.Header.Reserved-Spare-Type
  (make-binary-parameter-type
   '|STC.CCSDS.AOS.Header.Reserved-Spare-Type|
   :short-description "CCSDS Reserved 2 bits, always 00"
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 2)))))

(defvar CCSDS.AOS.Header.Frame-Count-Cycle-Type
  (make-binary-parameter-type
   '|STC.CCSDS.AOS.Header.Frame-Count-Cycle-Type|
   :short-description "Set to all zeros if not used. Otherwise, increments whenever the Frame Count rolls over, effectively extending it to 28 bits."
   :data-encoding (make-integer-data-encoding :size-in-bits 4)))

;TODO: Figure out how to describe that it should be reed solomon checked
(defvar CCSDS.AOS.Header.Frame-Header-Error-Control-Type
  (make-binary-parameter-type
   '|STC.CCSDS.AOS.Header.Frame-Header-Error-Control-Type|
   :short-description "Optional. Reed-Solomon Protecting Mater Channel Identifier and Virtual Channel Identifier. 16 bits."
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 16)))))

;TODO: Maybe specify an auxillary value we can use to specify this.
(defvar CCSDS.AOS.Insert-Zone-Type
  (make-binary-parameter-type '|STC.CCSDS.AOS.Insert-Zone-Type|
  :short-description "Optional."
  :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value AOS.Insert-Zone-Length)))))

(defvar CCSDS.AOS.Data-Field-Type
  (make-binary-parameter-type '|STC.CCSDS.AOS.Data-Field-Type|
  :short-description "M_PDU or B_PDU or VCA_SDU or Idle Data"
  :long-description (make-long-description "4.1.4.1.3 The Transfer Frame Data Field shall contain one Multiplexing Protocol Data Unit
(M_PDU), one Bitstream Protocol Data Unit (B_PDU), one Virtual Channel Access Service
Data Unit (VCA_SDU), or Idle Data. \n M_PDUs, B_PDUs, VCA_SDUs, and Idle Data shall not be mixed in a Virtual
Channel (i.e., if a Virtual Channel transfers M_PDUs, every Transfer Frame of that Virtual
Channel shall contain an M_PDU). Management shall decide whether M_PDUs, B_PDUs or
VCA_SDUs are transferred on a particular Virtual Channel, and this decision shall remain
static throughout a Mission Phase.")))

(defun with-ccsds.aos.header.types (type-list)
  (append type-list
		  (list
		   CCSDS.AOS.Data-Field-Type
		   CCSDS.AOS.Header.Frame-Count-Cycle-Type
		   CCSDS.AOS.Header.Frame-Header-Error-Control-Type
		   CCSDS.AOS.Insert-Zone-Type
		   CCSDS.AOS.Header.Master-Channel-ID-Type
		   CCSDS.AOS.Header.Replay-Flag-Type
		   CCSDS.AOS.Header.Reserved-Spare-Type
		   CCSDS.AOS.Header.Signaling-Field-Type
		   CCSDS.AOS.Header.Spacecraft-Identifier-Type
		   CCSDS.AOS.Header.Version-Number-Type
		   CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Type
		   CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag-Type
		   CCSDS.AOS.Header.Virtual-Channel-ID-Type)))

(defvar CCSDS.AOS.Header.Master-Channel-ID
  (make-parameter '|STC.CCSDS.AOS.Header.Master-Channel-ID| '|STC.CCSDS.AOS.Header.Master-Channel-ID-Type|))

(defvar CCSDS.AOS.Header.Signaling-Field
  (make-parameter '|STC.CCSDS.AOS.Header.Signaling-Field| '|STC.CCSDS.AOS.Header.Signaling-Field-Type|))

(defvar CCSDS.AOS.Header.Virtual-Channel-ID
  (make-parameter '|STC.CCSDS.AOS.Header.Virtual-Channel-ID| '|STC.CCSDS.AOS.Header.Virtual-Channel-ID-Type|))

(defvar CCSDS.AOS.Header.Virtual-Channel-Frame-Count
  (make-parameter '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count| '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Type|))

(defvar CCSDS.AOS.Header.Replay-Flag
  (make-parameter '|STC.CCSDS.AOS.Header.Replay-Flag| '|STC.CCSDS.AOS.Header.Replay-Flag|))

(defvar CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag
  (make-parameter '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag| '|STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag-Type|))

(defvar CCSDS.AOS.Header.Reserved-Spare
  (make-parameter '|STC.CCSDS.AOS.Header.Reserved-Spare| '|STC.CCSDS.AOS.Header.Reserved-Spare-Type|))

(defvar CCSDS.AOS.Header.Frame-Count-Cycle
  (make-parameter '|STC.CCSDS.AOS.Header.Frame-Count-Cycle| '|STC.CCSDS.AOS.Header.Frame-Count-Cycle-Type|))

(defvar CCSDS.AOS.Header.Frame-Header-Error-Control
  (make-parameter '|STC.CCSDS.AOS.Header.Frame-Header-Error-Control| '|STC.CCSDS.AOS.Header.Frame-Header-Error-Control-Type|))

(defvar CCSDS.AOS.Header.Insert-Zone
  (make-parameter '|STC.CCSDS.AOS.Insert-Zone| '|STC.CCSDS.AOS.Insert-Zone-Type|))

(defvar CCSDS.AOS.Header.Data-Field
  (make-parameter '|STC.CCSDS.AOS.Header.Data-Field| '|STC.CCSDS.AOS.Header.Data-Field-Type|))

(defun with-ccsds.aos.header.parameters (parameter-list)
  (append parameter-list
		  (list
		   CCSDS.AOS.Header.Master-Channel-ID
		   CCSDS.AOS.Header.Signaling-Field
		   CCSDS.AOS.Header.Virtual-Channel-ID
		   CCSDS.AOS.Header.Virtual-Channel-Frame-Count
		   CCSDS.AOS.Header.Replay-Flag
		   CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag
		   CCSDS.AOS.Header.Reserved-Spare
		   CCSDS.AOS.Header.Frame-Count-Cycle
		   CCSDS.AOS.Header.Frame-Header-Error-Control
		   CCSDS.AOS.Header.Data-Field)
		  (if AOS.Insert-Zone-Length
			  (list CCSDS.AOS.Header.Insert-Zone))))

(defun with-ccsds.aos.stream (frame-length-in-bits stream-list)
  (append stream-list
		  (list (make-fixed-frame-stream
				 '|STC.CCSDS.AOS.Stream|
				 frame-length-in-bits
				 (make-container-ref '|STC.CCSDS.AOS-Frame|)
				 (make-sync-strategy (make-sync-pattern))
				 :short-description "CCSDS AOS Stream"))))
