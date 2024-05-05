(in-package :standard-template-constructs)
(use-package :xtce)

(defvar CCSDS.MPDU.Transfer-Zone.Packet-Zone-Length
  (- AOS.Transfer-Frame-Data-Field-Length 16))

(defvar CCSDS.MPDU.Header.Reserved-Spare-Type
  (make-binary-parameter-type
   "STC.CCSDS.MPDU.Header.Reserved-Spare-Type"
   :short-description "5 bits. All zero."
   :data-encoding (make-binary-data-encoding (make-size-in-bits (make-fixed-value 5)))))

(defvar CCSDS.MPDU.Header.First-Header-Pointer-Type
  (make-binary-parameter-type "STC.CCSDS.MPDU.Header.First-Header-Pointer-Type"
  :short-description "11 bit Integer position of first octet."
  :data-encoding (make-integer-data-encoding :size-in-bits 11)))

(defvar CCSDS.MPDU.Packet-Zone-Type
  (make-binary-parameter-type "STC.CCSDS.MPDU.Packet-Zone-Type"
  :short-description "Contains a series of MPDU"
  :data-encoding (make-binary-data-encoding
				  (make-size-in-bits
				   (make-fixed-value CCSDS.MPDU.Transfer-Zone.Packet-Zone-Length)))))

(defvar CCSDS.MPDU.Packet-Type
  (make-binary-parameter-type "STC.CCSDS.MPDU.Packet-Type"
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
  (make-parameter "STC.CCSDS.MPDU.Header.Reserved-Spare" "STC.CCSDS.MPDU.Header.Reserved-Spare-Type"))

(defvar CCSDS.MPDU.Header.First-Header-Pointer
  (make-parameter "STC.CCSDS.MPDU.Header.First-Header-Pointer" "STC.CCSDS.MPDU.Header.First-Header-Pointer-Type"))
  
(defvar CCSDS.MPDU.Packet-Zone
  (make-parameter
   "STC.CCSDS.MPDU.Packet-Zone" "STC.CCSDS.MPDU.Packet-Zone-Type"))

(defvar CCSDS.MPDU.Packet
  (make-parameter "STC.CCSDS.MPDU.Packet" "STC.CCSDS.MPDU.Packet-Type"))

(defun with-ccsds.mpdu.parameters (parameter-list)
  (append parameter-list
		  (list
		   CCSDS.MPDU.Header.Reserved-Spare
		   CCSDS.MPDU.Header.First-Header-Pointer
		   CCSDS.MPDU.Packet-Zone)))

(defvar CCSDS.MPDU.Container.Header
  (make-sequence-container
   "STC.CCSDS.MPDU.Container.Header"
   (list
	(make-parameter-ref-entry "STC.CCSDS.MPDU.Header.Reserved-Spare")
	(make-parameter-ref-entry "STC.CCSDS.MPDU.Header.First-Header-Pointer"))))

(defvar CCSDS.MPDU.Container.Packet-Zone
  (make-sequence-container
   "STC.CCSDS.MPDU.Container.Packet-Zone"
   (list
	(make-parameter-ref-entry "STC.CCSDS.MPDU.Packet-Zone"))))

(defvar CCSDS.MPDU.Container.MPDU
  (make-sequence-container
   "STC.CCSDS.MPDU.Container.MPDU"
   (list
	(make-container-ref-entry "STC.CCSDS.MPDU.Container.Header")
	(make-container-ref-entry "STC.CCSDS.MPDU.Container.Packet-Zone"))))

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

;;Might be nicer to build an alist we can concatenate at the end
(defun make-mpdu-header (packet-header-in-bits maxmimum-packet-size)
  (let ((first-header-pointer-in-bytes
		  (if (<= packet-header-in-bits maxmimum-packet-size)
			  (uint->bit-vector (/ packet-header-in-bits 8) 11)
			  #*11111111111)))
	;(log:info packet-header-in-bits)
	(alist->bit-vector
	 (list (cons 'spare #*00000)
		   (cons 'first-header-pointer first-header-pointer-in-bytes)))))

(defun monad (frame symbol-table &key (packet-extractor (lambda (data first-header-pointer symbol-table alist)
						   (extract-space-packets data first-header-pointer symbol-table alist #*))))
  (log:info "STARTING CYCLE")
  (let* ((frame-alist (decode frame (gethash "STC.CCSDS.AOS.Container.Frame" symbol-table) symbol-table '() 0))
		 (frame-data-field (cdr (assoc stc::"STC.CCSDS.AOS.Transfer-Frame-Data-Field" frame-alist)))
		 (container (gethash "STC.CCSDS.MPDU.Container.MPDU" symbol-table))
		 (mpdu (decode frame-data-field container symbol-table '() 0))
		 (packet-zone (cdr (assoc stc::"STC.CCSDS.MPDU.Packet-Zone" mpdu)))
		 (first-header-pointer (cdr (assoc stc::"STC.CCSDS.MPDU.Header.First-Header-Pointer" mpdu))))

	(log:info first-header-pointer)
	(multiple-value-bind (alist next-extractor)
		(funcall packet-extractor packet-zone first-header-pointer symbol-table mpdu)
	  (values alist (lambda (frame symbol-table) (monad frame symbol-table :packet-extractor next-extractor))))))


(defun decode-mpdu-service (frame-alist service-definition symbol-table &key (packet-extractor (lambda (data first-header-pointer symbol-table alist)
																	(extract-space-packets data first-header-pointer symbol-table alist #*))))
  (let* ((frame-data-field (cdr (assoc stc::"STC.CCSDS.AOS.Transfer-Frame-Data-Field" frame-alist)))
		 (container (gethash "STC.CCSDS.MPDU.Container.MPDU" symbol-table))
		 (mpdu (xtce-engine::decode frame-data-field container symbol-table '() 0))
		 (packet-zone (cdr (assoc stc::"STC.CCSDS.MPDU.Packet-Zone" mpdu)))
		 (first-header-pointer (cdr (assoc stc::"STC.CCSDS.MPDU.Header.First-Header-Pointer" mpdu))))

	(log:info first-header-pointer)
	(multiple-value-bind (alist next-extractor)
		(funcall packet-extractor packet-zone first-header-pointer symbol-table mpdu)
	  (values alist :OK (lambda (frame-alist service-definition symbol-table) (decode-mpdu-service frame-alist service-definition symbol-table :packet-extractor next-extractor))))))
