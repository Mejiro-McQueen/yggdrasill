(ql:quickload "bifrost-yggdrasill")

(declaim (optimize (speed 0) (space 0) (debug 3)))
(defvar debug-mode t)

(in-package :xtce-engine)
(defparameter *TEST* (make-enumerated-parameter-type
 '|STC:CCSDS:Sequence-Flags-Type|
 :enumeration-list (make-enumeration-list
					(make-enumeration #b00 'Continuation :short-description "Space Packet contains a continuation segment of User Data.")
					(make-enumeration #b01 'First-Segment :short-description "Space Packet contains the first segment of User Data.")
					(make-enumeration #b10 'Last-Segment :short-description "Space Packet contains the last segment of User Data.")
					(make-enumeration #b11 'Unsegmented :short-description "Space Packet is unsegmented."))))

;; (make-space-system
;;  '|SpaceVechicle|
 
;; :telemetry-metadata
;;  (make-telemetry-metadata
  
;;   :parameter-type-set
;;   (make-parameter-type-set
   
;;    (make-integer-parameter-typoe
;;     '|IDType|
;;     :data-encoding
;;     (make-integer-data-encoding)
;; 	:signed nil)
   
;;    (make-integer-parameter-type
;; 	'|SecHType|
;; 	:signed nil
;; 	:data-encoding
;; 	(make-integer-data-encoding :size-in-bits 1))
   
;;    (make-integer-parameter-type
;; 	'|TypeType|
;; 	:signed nil
;; 	:data-encoding
;; 	(make-integer-data-encoding :size-in-bits 1))
   
;;    (make-integer-parameter-type
;; 	'|LengthType|
;; 	:signed nil
;; 	:data-encoding
;; 	(make-integer-data-encoding :size-in-bits 16))

;;    (make-enumerated-parameter-type
;; 	'|PSWHLTIMFLGType|
;; 	:data-encoding
;; 	(make-integer-data-encoding :size-in-bits 16)
;; 	:enumeration-list
;; 	(make-enumeration-list
;; 	 (make-enumeration 'TIMER_OFF 0)
;; 	 (make-enumeration 'TIMER_ON 1)
;; 	 (make-enumeration 'TIMER_COMPLETED 2)))
   
;;    (make-float-parameter-type
;; 	'|PBATMTEMPType|
;; 	:size-in-bits 64
;; 	:unit-set
;; 	(make-unit-set
;; 	 (make-unit
;; 	  :description "Bq"
;; 	  :form "units:Becquerel"))
;; 	:data-encoding
;; 	(make-integer-data-encoding
;; 	 :size-in-bits 16
;; 	 :encoding 'twos-complement
;; 	 :default-calibrator
;; 	 (make-polynomial-calibrator
;; 	  :term-list
;; 	  (make-term-list 
;; 	   (make-term :coefficient -7459.23273708 :exponent 0)
;; 			 (make-term :coefficient 8.23643519148 :exponent 1)
;; 			 (make-term :coefficient -3.02185061876e3 :exponent 2)
;; 			 (make-term :coefficient 2.33422429056e-7 :exponent 3)
;; 			 (make-term :coefficient 5.67189556173e11 :exponent 4)))))
   
;;    (make-absolute-time-parameter
;; 	'|MissionTimeType|
;; 	:reference-time
;; 	(make-reference-time
;; 	 (make-offset-from '|Seconds|)))
   
;;    (make-absolute-time-parameter
;; 	'|SecondsType|
;; 	:encoding
;; 	(make-encoding
;; 	 :units 'seconds
;; 	 :data-encoding
;; 	 (make-integer-data-encoding :size-in-bits 32))
;; 	:reference-time
;; 	(make-reference-time
;; 	 (make-offset-from '|Milliseconds|)))
   
;;    (make-absolute-time-parameter
;; 	'|MillisecondsType|
;; 	:encoding
;; 	(make-encoding
;; 	 :units '|seconds|
;; 	 :scale 0.001
;; 	 :data-encoding
;; 	 (make-integer-data-encoding :size-in-bits 16))
;; 	:reference-time
;; 	(make-reference-time
;; 	 (make-epoch 'TAI))))
 
;;   :parameter-set
;;   (make-parameter-set
;;    (make-parameter '|SecH| '|SecHType|)
;;    (make-parameter '|Type| '|TypeType|)
;;    (make-parameter '|ID| '|IDType|)
;;    (make-parameter '|Length| '|LengthType|)
;;    (make-parameter '|Seconds| '|SecondsType|)
;;    (make-parameter '|Milliseconds| '|MillisecondsType|)
;;    (make-parameter '|PBATMTEMP| '|PBATMTEMPType|)
;;    (make-parameter '|PSWHLTIMFLG| '|PSWHLTIMFLGType|)
;;    (make-parameter '|MissionTime| '|MissionTimeType|
;; 				   :parameter-properties
;; 				   (make-parameter-properties :data-source "derived")))

;;   :container-set
;;   (make-container-set
;;    (make-sequence-container
;; 	'|Header|
;; 	(make-entry-list
;; 	 (make-parameter-ref-entry '|ID|)
;; 	 (make-parameter-ref-entry '|SecH|)
;; 	 (make-parameter-ref-entry '|Type|)
;; 	 (make-parameter-ref-entry '|Length|)
;; 	 (make-parameter-ref-entry '|SecondaryHeader|
;; 							   :include-condition
;; 							   (make-include-condition (make-comparison '|SecH| 1))))))))


;; (time (dump-space-system-xml (symbol-value 'SPACEVECHICLE)))


; Generate and store speculative container match
; When container is called again, check for speculative match, then check against restriction criteria.
; When the full container match occurs, you win





;; (progn 
;;   (defparameter *TEST* (process-fixed-frame 0 6 'LOCK (make-sync-strategy) (make-sync-pattern) #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF :aperture 0))
;;   (print *TEST*))

;; (process-fixed-frame 0 8 'LOCK (make-sync-strategy) (make-sync-pattern) #x1acffc1eFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

;; (print-hex (second (process-fixed-frame 0 1 'SEARCH (make-sync-strategy) (make-sync-pattern) #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))

; (lambda (frame aperture) (process-fixed-frame 0 0 'SEARCH (make-sync-strategy) (make-sync-pattern) frame :aperture aperture)))))))

;Fixed frames do not span, immediately move to next level
;Variable sized frames may span, need to move to accumulator (e.g. simulators)
