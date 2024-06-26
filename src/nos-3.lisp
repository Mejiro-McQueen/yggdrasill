(ql:quickload "bifrost-yggdrasill")
(in-package :nasa-cfs)

(defvar NASA-cFS
  (make-space-system
   'NASA-CFS
   :root t
   :short-description "Test system for NASA CFS for NOS-3"
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-type-set
	(list (make-integer-parameter-type 'UINT_16_Type :size-in-bits 16 :signed nil)
		  (make-integer-parameter-type 'SPARE_32_Type :size-in-bits 32 :signed nil)
		  (make-binary-parameter-type 'NOOP_Type :short-description "Generic No Operation"
												 :data-encoding (make-binary-data-encoding 0))))
   :space-system-list
   (list CFS)))

(defvar CFS
  (make-space-system
   'CFS :parent-system 'NASA-CFS
		:space-system-list (list TO)))

(defvar TO
  (make-space-system
   '|Telemetry_Output| :parent-system 'CFS
   :short-description "Telemetry Output"
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-set (list (make-parameter '|Command_Counter| '|/UINT_16_Type|)
						 (make-parameter '|Error_Counter| '|/UINT_16_Type|)
						 (make-parameter '|Message_Sub_Counter| '|/UINT_16_Type|)
						 (make-parameter '|Message_Sub_Error_Counter| '|/UINT_16_Type|)
						 (make-parameter '|Table_Update_Counter| '|/UINT_16_Type|)
						 (make-parameter '|Table_Error_Counter| '|/UINT_16_Type|)
						 (make-parameter '|Configured_Routes| '|/UINT_16_Type|)
						 (make-parameter '|Enabled_Routes| '|/UINT_16_Type|))
	:container-set
	(list
	 (make-space-packet-container 1 'House_Keeping_Packet
								  (mapcar #'make-parameter-ref-entry '('|Command_Counter|
																	   '|Error_Counter|
																	   '|Message_Sub_Counter|
																	   '|Table_Update_Counter|
																	   '|Table_Error_Counter|
																	   '|Config_Routes|
																	   '|Enabled_Routes|))
								  :short-description "Housekeeping Packet Structure"))
	)))




(log:info (dump-xml NASA-cFS))

(stc::decode-ccsds #*0000100010000000 stc::CCSDS.Space-Packet.Container.Header.Packet-Identification 3)
