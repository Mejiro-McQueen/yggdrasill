(ql:quickload "filesystem-hash-table")
(ql:quickload "uiop")
(ql:quickload "alexandria")
(ql:quickload "cxml")
(ql:quickload "bifrost-yggdrasill")

(declaim (optimize (speed 0) (space 0) (debug 3)))
(defvar debug-mode t)

(in-package :xtce)

(defun print-bin (n)
  (let ((pad (hex-length-in-bits n)))
	(format nil "~v,'0b" pad n)))

(defun print-hex (n)
  (format nil "~X" n))

(defun hex-length-in-bits (hex-val)
  "Use to count how large a frame is in bits"
  ;integer-length only counts significant bits (i.e. #x5555 = 010101... resulting in 15)  
  (let ((number-of-hex-characters (length (format nil "~X" hex-val)))
		(bits-per-hex 4))
	(if hex-val
		(* bits-per-hex number-of-hex-characters)
		0)))

(defun truncate-from-left (data bits)
  (let ((length-from-lsb (- (integer-length data) bits)))
	(ldb (byte length-from-lsb 0) data)))

(defun truncate-from-left-to-size (data bits)
  (truncate-from-left data (- (integer-length data) bits)))

(defun ldb-left (size position integer)
  "Extract (size) bits starting at MSB bit (position) from integer
   Example: 
     ldb-msb (16 0 #xABCD) = #xABCD
     ldb-msb (4 2) = #xA
     ldb-msb (4 4) = #xC
"
  (let* ((msb (hex-length-in-bits integer))
		 (msb-pos (- msb position size)))
	(ldb (byte size msb-pos) integer)))

(defun hamming-distance (integer-1 integer-2)
  (logcount (logxor integer-1 integer-2)))

(make-space-system
 '|SpaceVechicle|
 
:telemetry-metadata
 (make-telemetry-metadata
  
  :parameter-type-set
  (make-parameter-type-set
   
   (make-integer-parameter-type
    '|IDType|
    :data-encoding
    (make-integer-data-encoding)
	:signed nil)
   
   (make-integer-parameter-type
	'|SecHType|
	:signed nil
	:data-encoding
	(make-integer-data-encoding :size-in-bits 1))
   
   (make-integer-parameter-type
	'|TypeType|
	:signed nil
	:data-encoding
	(make-integer-data-encoding :size-in-bits 1))
   
   (make-integer-parameter-type
	'|LengthType|
	:signed nil
	:data-encoding
	(make-integer-data-encoding :size-in-bits 16))

   (make-enumerated-parameter-type
	'|PSWHLTIMFLGType|
	:data-encoding
	(make-integer-data-encoding :size-in-bits 16)
	:enumeration-list
	(make-enumeration-list
	 (make-enumeration 'TIMER_OFF 0)
	 (make-enumeration 'TIMER_ON 1)
	 (make-enumeration 'TIMER_COMPLETED 2)))
   
   (make-float-parameter-type
	'|PBATMTEMPType|
	:size-in-bits 64
	:unit-set
	(make-unit-set
	 (make-unit
	  :description "Bq"
	  :form "units:Becquerel"))
	:data-encoding
	(make-integer-data-encoding
	 :size-in-bits 16
	 :encoding 'twos-complement
	 :default-calibrator
	 (make-polynomial-calibrator
	  :term-list
	  (make-term-list 
	   (make-term :coefficient -7459.23273708 :exponent 0)
			 (make-term :coefficient 8.23643519148 :exponent 1)
			 (make-term :coefficient -3.02185061876e3 :exponent 2)
			 (make-term :coefficient 2.33422429056e-7 :exponent 3)
			 (make-term :coefficient 5.67189556173e11 :exponent 4)))))
   
   (make-absolute-time-parameter
	'|MissionTimeType|
	:reference-time
	(make-reference-time
	 (make-offset-from '|Seconds|)))
   
   (make-absolute-time-parameter
	'|SecondsType|
	:encoding
	(make-encoding
	 :units 'seconds
	 :data-encoding
	 (make-integer-data-encoding :size-in-bits 32))
	:reference-time
	(make-reference-time
	 (make-offset-from '|Milliseconds|)))
   
   (make-absolute-time-parameter
	'|MillisecondsType|
	:encoding
	(make-encoding
	 :units '|seconds|
	 :scale 0.001
	 :data-encoding
	 (make-integer-data-encoding :size-in-bits 16))
	:reference-time
	(make-reference-time
	 (make-epoch 'TAI))))
 
  :parameter-set
  (make-parameter-set
   (make-parameter '|SecH| '|SecHType|)
   (make-parameter '|Type| '|TypeType|)
   (make-parameter '|ID| '|IDType|)
   (make-parameter '|Length| '|LengthType|)
   (make-parameter '|Seconds| '|SecondsType|)
   (make-parameter '|Milliseconds| '|MillisecondsType|)
   (make-parameter '|PBATMTEMP| '|PBATMTEMPType|)
   (make-parameter '|PSWHLTIMFLG| '|PSWHLTIMFLGType|)
   (make-parameter '|MissionTime| '|MissionTimeType|
				   :parameter-properties
				   (make-parameter-properties :data-source "derived")))

  :container-set
  (make-container-set
   (make-sequence-container
	'|Header|
	(make-entry-list
	 (make-parameter-ref-entry '|ID|)
	 (make-parameter-ref-entry '|SecH|)
	 (make-parameter-ref-entry '|Type|)
	 (make-parameter-ref-entry '|Length|)
	 (make-parameter-ref-entry '|SecondaryHeader|
							   :include-condition
							   (make-include-condition (make-comparison '|SecH| 1))))))))


(time (dump-space-system-xml (symbol-value 'SPACEVECHICLE)))


(defparameter *TEST* (make-container-set 
 (make-sequence-container
  '|MyFormatHeader|
  (make-entry-list
   (make-parameter-ref-entry '|Version|)
   (make-parameter-ref-entry '|Type|)
   (make-parameter-ref-entry '|ID|)
   (make-parameter-ref-entry '|Length|))
   :abstract t)
 
 (make-sequence-container
  '|MyTimeStampHeader|
  (make-entry-list
   (make-parameter-ref-entry '|TimeStamp|))
  :abstract t
  :base-container
  (make-base-container
   '|MyFormatHeader|
   :restriction-criteria
   (make-restriction-criteria
	(make-comparison-list
	 (make-comparison '|Version| 1)
	 (make-comparison '|Type| 1)))))

 (make-sequence-container
  '|MyConcretePacket|
  (make-entry-list
   (make-parameter-ref-entry '|P1|)
   (make-parameter-ref-entry '|P2|)
   (make-parameter-ref-entry '|P3|))
  :base-container
  (make-base-container
   '|MyTimeStampHeader|
   :restriction-criteria
   (make-restriction-criteria
	(make-comparison '|ID| 100))))))


(defparameter *HASH* (make-hash-table))

(dolist (container (slot-value *TEST* 'items))
  (let ((container-name (slot-value container 'name)))
  (setf (gethash container-name *HASH*) container)))

(defun find-resolvable-containers (containers-map)
  (let ((resolveable-containers '()))
	(maphash (lambda (key container)
			   (declare (ignore key))
			   (when (resolveable-container-p container)
				 (push container resolveable-containers)))
			 containers-map)
  resolveable-containers))

(defun abstract-container-p (container)
  (when container
	(slot-value container 'abstract)))

(defun concrete-container-p (container)
  (when container
  (not (abstract-container-p container))))
		  
(defun resolveable-container-p (container)
  (when container
	(and (concrete-container-p container)
		 (heir-container-p container))))

(defun heir-container-p (container)
  (when container
	(if (slot-value container 'base-container)
		t)))

(defun unresolvable-container-p (container)
  (not (resolveable-container-p container)))

;; (dump-xml (first (resolve-containers *HASH*)))

(defun resolve (func container container-map)
  (let* ((base-container (if container (slot-value container 'base-container)))
		 (parent-container-ref (if base-container (slot-value base-container 'container-ref)))
		 (parent-container (gethash parent-container-ref container-map)))
	(if (heir-container-p parent-container)
		(funcall func container (resolve func parent-container container-map))
		(funcall func container parent-container))))

(defun resolve-containers (f container-map)
  "Map a function on all concrete containers, descending recurs down their inheritance chain.

   Args:
   function: (container container-parent container-jump-table)
   container-map: jump table for containers."
  (let ((resolvable-containers (find-resolvable-containers container-map)))
		(mapcar #'(lambda (container) (resolve f container container-map)) resolvable-containers)))

(defmethod deep-inherit ((child sequence-container) (parent sequence-container))
  ;NOTE: It looks like nextContainer just means that container's restriction's must pass for this container to take effect.
  ;See: 4.3.4.9.4 NextContainer Element
  ;5.5 DYNAMIC CONTAINER MATCHING seems to take effect in order to dynamically resolve that container if it is abstract
  (let* ((name (slot-value child 'name))
		 (abstract (slot-value child 'abstract))
		 (idle-pattern (slot-value child 'idle-pattern))
		 (short-description (slot-value child 'short-description))
		 (long-description (slot-value child 'long-description))
		 (alias-set (slot-value child 'long-description))
		 (ancillary-data-set (append (slot-value child 'ancillary-data-set) (slot-value parent 'ancillary-data-set)))
		 (default-rate-in-stream (if (slot-value child 'default-rate-in-stream) (slot-value parent 'default-rate-in-stream)))
		 (child-rate-in-stream-set (slot-value child 'rate-in-stream-set))
		 (parent-rate-in-stream-set (slot-value parent 'rate-in-stream-set))
		 (rate-in-stream-set (apply #' make-rate-in-stream-set
							  (union (if child-rate-in-stream-set
										 (items child-rate-in-stream-set)
										 nil)
									 (if parent-rate-in-stream-set
										 (items parent-rate-in-stream-set)
										 nil) :key 'stream-ref)))
		 
		 (binary-encoding (if (slot-value child 'binary-encoding) (slot-value parent 'binary-encoding)))
		 (parent-entry-list (slot-value parent 'entry-list))
		 (child-entry-list (slot-value child 'entry-list))
		 (entry-list (apply #'make-entry-list
							(append
							 (if parent-entry-list
								 (items parent-entry-list)
								 nil)
							 (if child-entry-list
								 (items child-entry-list)
								 nil)
							 )))
		 ;I'm keeping the base container so that we can neatly check the restriction criteria.
		 ;I don't know how I feel about making a non XTCE construct to hold it at the top level.
		 ;Descending the inheritance tree shouldn't bee too expensive.
		 (base-container nil) ;(slot-value child 'base-container))
		 (new (make-sequence-container name
									   entry-list
									   :abstract abstract
									   :idle-pattern idle-pattern
									   :short-description short-description
									   :long-description long-description
									   :alias-set alias-set
									   :ancillary-data-set ancillary-data-set
									   :rate-in-stream-set rate-in-stream-set
									   :base-container base-container
									   :default-rate-in-stream default-rate-in-stream
									   :binary-encoding binary-encoding)))
	new))

;(mapcar 'describe (resolve-containers *HASH*))

(resolve-containers #'deep-inherit *HASH*)
(dump-xml (first (resolve-containers #'deep-inherit *HASH*)))

;; (step (resolve-containers *HASH*))

;; (trace resolve)
;; (trace resolve-containers)
;(find-resolvable-containers *HASH*)


; Generate and store speculative container match
; When container is called again, check for speculative match, then check against restriction criteria.
; When the full container match occurs, you win

(defclass data-stream () ())

(defclass stream-set (xtce-set) ())


(defun make-data-stream-set (&rest items)
  (make-xtce-set 'data-stream "StreamSet" items))

(defclass variable-frame-stream (data-stream) ())

(defclass custom-stream (data-stream) ())

(defclass fixed-frame-stream (data-stream)
  ((name :initarg :name :type string)
   (short-description :initarg :short-description :type short-description)
   (long-description :initarg :long-description :type string)
   (alias-set :initarg :alias-set :type alias-set)
   (ancillary-data-set :initarg :ancillary-data-set :type ancillary-data-set)
   (bit-rate-in-bps :initarg :bit-rate-in-bips)
   (pcm-type :initarg :pcm-type)
   (inverted :initarg :inverted :type boole)
   (sync-apeture-in-bits :initarg :sync-apeture-in-bits :type sync-apeture-in-bits)
   (frame-length-in-bits :initarg :frame-length-in-bits :type positive-integer)
   (reference-type :initarg :reference-type :type reference-type)
   (stream-ref :initarg :stream-ref :type string)
   (sync-strategy :initarg :sync-strategy :type sync-strategy)))

(defun make-fixed-frame-stream (sync-strategy frame-length-in-bits &key sync-apeture-in-bits)
  "For streams that contain a series of frames with a fixed frame length where the frames are found by looking for a marker in the data. This marker is sometimes called the frame sync pattern and sometimes the Asynchronous Sync Marker (ASM). This marker need not be contiguous although it usually is."
  (make-instance 'fixed-frame-stream
				 :sync-strategy sync-strategy
				 :sync-apeture-in-bits sync-apeture-in-bits
				 :frame-length-in-bits frame-length-in-bits))


(defclass sync-strategy () ((auto-invert :initarg :auto-invert :type auto-invert)
							(sync-pattern :initarg :sync-pattern :type sync-pattern)
							(verify-to-lock-good-frames :initarg :verify-to-lock-good-frames :type postiive-integer)
							(check-to-lock-good-frames :initarg :check-to-lock-good-frames :type postiive-integer)
							(max-bit-errors-in-sync-pattern :initarg :max-bit-errors-in-sync-pattern :type postiive-integer)))

(defun make-sync-strategy (&key (verify-to-lock-good-frames 4) (check-to-lock-good-frames 1) (max-bit-errors-in-sync-pattern 0))
  "CCSDS: A Sync Strategy specifies the strategy on how to find frames within a stream of PCM data. The sync strategy is based upon a state machine that begins in the 'Search' state until the first sync marker is found. Then it goes into the 'Verify' state until a specified number of successive good sync markers are found. Then, the state machine goes into the 'Lock' state, in the 'Lock' state frames are considered good. Should a sync marker be missed in the 'Lock' state, the state machine will transition into the 'Check' state, if the next sync marker is where it's expected within a specified number of frames, then the state machine will transition back to the 'Lock' state, it not it will transition back to 'Search'"
  (make-instance 'sync-strategy
				 :verify-to-lock-good-frames verify-to-lock-good-frames
				 :check-to-lock-good-frames check-to-lock-good-frames
				 :max-bit-errors-in-sync-pattern max-bit-errors-in-sync-pattern))

(defclass sync-pattern ()
  ((pattern :initarg :pattern
			:documentation "Hexadecimal pattern to match against a potential synchronization marker. e.g. CCSDS ASM for non-turbocoded frames is #x1acffc1d")
   (pattern-length-in-bits :initarg :pattern-length-in-bits
						   :type positive-integer
						   :documentation "Truncate the pattern from the left so that the pattern is exactly this many bits.")
   (bit-location-from-start :initarg :bit-location-from-start
							:type positive-integer
							:documentation "After the synchronization marker is truncated, truncate this amount more so that the left most bit of the frame corresponds to the start of the container.")
   (mask :initarg :mask
		 :documentation "Apply this mask (e.g. #x0x29a) to the potential synchronization marker before checking against the pattern.")
   (mask-length-in-bits :initarg :mask-length-in-bits
						:type postive-integer
						:documentation "Truncate the mask from the left so that the pattern is exactly this many bits.")))

(defun make-sync-pattern (&key (pattern #x1acffc1d) (pattern-length-bits (hex-length-in-bits pattern)) mask mask-length-bits (bit-location-from-start 0))
  "CCSDS: The pattern of bits used to look for frame synchronization. See SyncPatternType.
   Bifrost: Define a synchronization pattern and masks. Used as metadata to search the synchronization markers of fixed frames."
  (make-instance 'sync-pattern
				 :pattern pattern
				 :pattern-length-in-bits pattern-length-bits
				 :bit-location-from-start bit-location-from-start
				 :mask mask
				 :mask-length-in-bits mask-length-bits))

(defun find-sync-pattern (frame sync-pattern &key (max-bit-errors 0) (aperture 0))
  ; Need to double check if aperture does what we think it does
  "Use to check for a synchronized frame.

   Args:
     frame (hex): the frame to check for synchronization pattern.
     sync-pattern (sync-pattern): sync-pattern type.
     max-errors (positive-integer): Maximum number of bit errors (inclusive) for a match to occur.
     
  Returns:
    hex: Frame truncated from the left up to the start of the container (i.e. truncate synchronization marker + start of container)
    nil: No synchronization marker was found. "

  (with-slots (pattern pattern-length-in-bits bit-location-from-start mask mask-length-bits) sync-pattern
	(let* ((truncated-mask (if (and mask mask-length-bits)
							   (truncate-from-left-to-size mask mask-length-bits)
							   pattern))
		   
		   (truncated-pattern (if pattern-length-in-bits
								  (truncate-from-left-to-size pattern pattern-length-in-bits)
								  pattern))
		   (speculative-match (ldb-left pattern-length-in-bits aperture frame))
		   
		   (match? (logand speculative-match truncated-mask))
		   (error-count (hamming-distance truncated-pattern match?))
		   (frame-truncation (+ 1 bit-location-from-start pattern-length-in-bits aperture)))
	  (when (<= error-count max-bit-errors)
		(truncate-from-left frame frame-truncation)))))

(defun process-fixed-frame (state-check-counter verify-counter state-symbol sync-strategy sync-pattern frame &key (aperture 0))
  (let ((sync-result (find-sync-pattern frame sync-pattern :aperture aperture))
		(state-result nil))
	
	(labels ((reset-verify-counter () (setf verify-counter 0))
			 (reset-state-check-counter () (setf state-check-counter 0)))
	  
	  (case state-symbol
		(LOCK
		 (reset-state-check-counter)
		 (when sync-result
		   (incf verify-counter)
		   (setf state-result 'LOCK)))
		 (unless sync-result
		   (incf state-check-counter)
		   (setf state-result 'CHECK))

		(CHECK
		 (when sync-result
		   (reset-state-check-counter)
		   (incf verify-counter)
		   (setf state-result 'LOCK))
		 (unless sync-result
		   (if (> state-check-counter (slot-value sync-strategy 'check-to-lock-good-frames))
			   (progn
				 (reset-state-check-counter)
				 (reset-verify-counter)
				 (setf state-result 'SEARCH))
			   (progn
				 (incf state-check-counter)
				 (setf state-result 'CHECK)))))

		(VERIFY
		 (reset-state-check-counter)
		 (when sync-result
		   (if (> verify-counter (slot-value sync-strategy 'verify-to-lock-good-frames))
			   (progn
				 (incf verify-counter)
				 (setf state-result 'LOCK))
			   (setf state-result 'VERIFY)))
		 (unless sync-result
		   (reset-verify-counter)
		   (setf state-result 'SEARCH)))

		(SEARCH
		 (reset-state-check-counter)
		 (when sync-result
		   (incf verify-counter)
		   (setf state-result 'VERIFY))
		 (unless sync-result
		   (reset-verify-counter)
		   (setf state-result 'SEARCH))))
	(list state-result sync-result (lambda (frame) (process-fixed-frame
											   state-check-counter
											   verify-counter
											   state-result
											   sync-strategy
											   sync-pattern
											   frame))))))
(progn 
  (defparameter *TEST* (process-fixed-frame 0 6 'LOCK (make-sync-strategy) (make-sync-pattern) #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF :aperture 0))
  (print *TEST*))


(process-fixed-frame 0 8 'LOCK (make-sync-strategy) (make-sync-pattern) #x1acffc1eFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

(print-hex (second (process-fixed-frame 0 1 'SEARCH (make-sync-strategy) (make-sync-pattern) #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))




(funcall (third *TEST*) #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

(defun emit! (message)
  (format t "~A" message))

(defun process-fixed-frame-stream
  (fixed-frame-stream
   data-stream
   &key
	 (continuation (lambda (frame aperture) (process-fixed-frame 0 0 'SEARCH (make-sync-strategy) (make-sync-pattern) frame :aperture aperture))))
  
  (declare (ignore fixed-frame-stream))
  (labels ((aperture-values (n)
			 (append '(0)
					 (alexandria:iota n :start 1)
					 (alexandria:iota n :start -1 :step -1)))

		   (find-marker-with-aperture (aperture)
			 (loop for aperture in (aperture-values aperture)
				   for res = (funcall continuation data-stream aperture)
				   when (second res)
					 return (cons aperture res)
				   finally (return (cons 0 res)))))

	(destructuring-bind (aperture state frame next-continuation) (find-marker-with-aperture 5)
	  ;;(print state)
	  ;; (print aperture)
	  ;; (print (print-hex frame))
	  (unless aperture
		(emit! (list "Aperture greater than zero:" aperture)))
	  (case state
		(LOCK
		 (accept-frame frame)
		 (emit! state))
		  
		(VERIFY
		 (emit! state)
		 )
		  
		(SEARCH
		 (emit! (list "Could not find synchronization marker!")))
		  
		(CHECK))
		
	  (return-from process-fixed-frame-stream (list state next-continuation)))))


; (lambda (frame aperture) (process-fixed-frame 0 0 'SEARCH (make-sync-strategy) (make-sync-pattern) frame :aperture aperture)))))))
		  
(process-fixed-frame-stream (make-fixed-frame-stream (make-sync-strategy) 1024) #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

;Fixed frames do not span, immediately move to next level
;Variable sized frames may span, need to move to accumulator (e.g. simulators)


(make-space-system
 '|NASA-cFS|
 :root t
 :short-description
 "Root system for the NASA-cFS"

 :telemetry-metadata
 (make-telemetry-metadata
  :parameter-type-set
  (make-parameter-type-set
  
   (make-integer-parameter-type
	'|U8-Type|
	:short-description "Unsigned 8bit integer with no encoding or calibration."
	:signed nil
	:size-in-bits 8)

  (make-binary-parameter-type
   '|16-Bit-Checksum-Type|
   :short-description
   "16 bit checksum")

   (make-binary-parameter-type
   '|32-Bit-Checksum-Type|
   :short-description
   "32 bit checksum")

   (make-integer-parameter-type
	'|U32-Type|
	:short-description "Unsigned 32 bit integer"
	:signed nil
	:size-in-bits 32)

   (make-string-parameter-type
	'|ASCII-String-Type|
	:short-description "ASCII string")

   (make-integer-parameter-type
	'|U16-Type|
	:short-description "Unsigned 16 bit integer"
	:signed nil
	:size-in-bits 16)

   (make-integer-parameter-type
	'|U64-Type|
	:short-description
	"Unsigned 64 bit integer."
	:signed nil
	:size-in-bits 64)

   (make-float-parameter-type
	'|F64-Type|
	:short-description
	"64 bit float."
	:size-in-bits 64)

   (make-float-parameter-type
	'|F32-Type|
	:short-description
	"32 bit float.")

   (make-enumerated-parameter-type
	'|on-off-enum-type|
	:short-description "On/Off enumeration."
	:enumeration-list (make-enumeration-list (make-enumeration 'ON 1) (make-enumeration 'OFF 0))))

  :parameter-set
  (make-parameter-set
   (make-parameter '|COMMANDCOUNTER| '|U8-Type| :short-description "EVS Command Counter.")))
 
 :space-systems-list
 (make-space-systems-list
  (make-space-system
   '|MGSS|
   :short-description "MGSS sample app"
   :long-description (make-long-description "This is a cFS app that periodically publishes a sine wave with amplitude 1.")
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-type-set
	(make-parameter-type-set
	 
	 (make-float-parameter-type
	  '|Battery-Voltage-Type|
	  :short-description "Battery voltage type."
	  :size-in-bits 32
	  :unit-set (make-unit-set (make-unit :description "Volts" :form 'raw))))

	:parameter-set
	(make-parameter-set
	 (make-parameter '|HEATERSTATUS| '|/on-off-enum-type| :short-description "1 = ON, 0 = OFF")
	 (make-parameter '|ON_ORBIT_ENUM| '|/U8-Type| :short-description "FSW Enumeration") ;TODO: Investigate correct type
	 (make-parameter '|BAT_VOLTAGE| '|Battery-Voltage-Type| :short-description "Battery Voltage (Sine Wave)")
	)))

  (make-space-system
   '|CFE_ES|
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-type-set
	(make-parameter-type-set
	 
	 (make-array-parameter-type
	  '|PERFFILTERMASK-Type|
	  '|/U32-Type|
	  :short-description "Current Setting of Performance Analyzer Filter Masks."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 3)))))

	 (make-array-parameter-type
	  '|PERFTRIGGERMASK-Type|
	  '|/U32-Type|
	  :short-description "Current Setting of Performance Analyzer Trigger Masks."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 3)))))

	 (make-array-parameter-type
	  '|APPDATA-Type|
	  '|/U64-Type|
	  :short-description "Array of registered application table data."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 15))))))

	:parameter-set
	(make-parameter-set
	 (make-parameter '|ACTIVEBUFFER| '|/U8-Type| :short-description "Indicator of whether table buffer validated was 0=Inactive, 1=Active.") ;TODO: Change to enum type
	 (make-parameter '|APPDATA| '|APPDATA-Type| :short-description "Array of registered application table data.")
	 (make-parameter '|APP_BUFFERPOOLHANDLE| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_CHANNUM| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_CLASS| '|/ASCII-String-Type| :short-description "")
	 (make-parameter '|APP_CONDCODE| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_DSTFILE| '|/ASCII-String-Type| :short-description "")
	 (make-parameter '|APP_ENGINECYCLECOUNT| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_LASTFAILEDTRANS| '|/ASCII-String-Type| :short-description "")
	 (make-parameter '|APP_LOWMEMORYMARK| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_MAXMEMNEEDED| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_MEMALLOCATED| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_MEMINUSE| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_NODETYPE| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_PDUSRECEIVED| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_PDUSREJECTED| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_PEAKMEMINUSE| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_PRIORITY| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_QNODESALLOCATED| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_QNODESDEALLOCATED| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_SOURCE| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_SRCENTITYID| '|/ASCII-String-Type| :short-description "")
	 (make-parameter '|APP_SRCFILE| '|/ASCII-String-Type| :short-description "")
	 (make-parameter '|APP_STATUS| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_TOTALABANDONTRANS| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_TOTALCOMPLETEDTRANS| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_TOTALFAILEDTRANS| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_TOTALINPROGTRANS| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_TOTALSUCCESSTRANS| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_TRANSNUM| '|/U32-Type| :short-description "")
	 (make-parameter '|APP_WAKEUPFORFILEPROC| '|/U32-Type| :short-description "")
	 (make-parameter '|AUTOSUSPEND_ENFLAG| '|/U32-Type| :short-description "")
	 (make-parameter '|AUTOSUSPEND_LOWFREEMAR| '|/U32-Type| :short-description "")
	 (make-parameter '|BOOTSOURCE| '|/U32-Type| :short-description "Boot source ( as provided from BSP ).")
	 (make-parameter '|CFECORECHECKSUM| '|/U16-Type|  :short-description "Checksum of cFE Core Code.")
	 (make-parameter '|CFEMAJORVERSION| '|/U8-Type| :short-description "Major Version Number of cFE.")
	 (make-parameter '|CFEMINORVERSION| '|/U8-Type| :short-description "Minor Version Number of cFE.")
	 (make-parameter '|CFEMISSIONREVISION| '|/U8-Type| :short-description "Mission Version Number of cFE.")
	 (make-parameter '|CFEREVISION| '|/U8-Type| :short-description "Sub-Minor Version Number of cFE.")
	 (make-parameter '|CLOCKSTATEAPI| '|/U16-Type| :short-description "API State.")
	 (make-parameter '|CLOCKSTATEFLAGS| '|/U16-Type| :short-description "State Flags.")
	 (make-parameter '|COND_CANCELNUM| '|/U8-Type| :short-description "")
	 (make-parameter '|COND_FILECHECKSUMNUM| '|/U8-Type| :short-description "")
	 (make-parameter '|COND_FILESIZENUM| '|/U8-Type| :short-description "")
	 (make-parameter '|COND_FILESTOREREJNUM| '|/U8-Type| :short-description "")
	 (make-parameter '|COND_INACTIVENUM| '|/U8-Type| :short-description "")
	 (make-parameter '|COND_NAKLIMITNUM| '|/U8-Type| :short-description "")
	 (make-parameter '|COND_POSACKNUM| '|/U8-Type| :short-description "")
	 (make-parameter '|COND_SUSPENDNUM| '|/U8-Type| :short-description "")
	 (make-parameter '|CREATEPIPEERRCNT| '|/U8-Type| :short-description "Count of errors in create pipe API.")
	 (make-parameter '|Command-Counter-8| '|/U8-Type| :short-description "EVS Command Counter.")
	 (make-parameter '|Command-Error-Counter-8| '|/U8-Type| :short-description "EVS Command Error Counter.")
	 (make-parameter '|DUPSUBSCRIPTIONSCNT| '|/U8-Type| :short-description "Count of duplicate subscriptions.")
	 (make-parameter '|ENG_ATTEMPTS| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_CHECKSUM| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_CONDCODE| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_DELICODE| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_DSTFILE| '|/ASCII-String-Type| :short-description "")
	 (make-parameter '|ENG_FDLENGTH| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_FDOFFSET| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_FILESIZE| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_FINALSTAT| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_FLAGS| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_FLIGHTENGINEENTITYID| '|/ASCII-String-Type| :short-description "")
	 (make-parameter '|ENG_MACHINESALLOCATED| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_MACHINESDEALLOCATED| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_NAKS| '|/U8-Type| :short-description "")
	 (make-parameter '|ENG_PARTLEN| '|/U8-Type| :short-description "")
	 (make-parameter '|ENG_PARTVAL| '|/U8-Type| :short-description "")
	 (make-parameter '|ENG_PHASE| '|/U8-Type| :short-description "")
	 (make-parameter '|ENG_RCVDFILESIZE| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_ROLE| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_SRCFILE| '|/ASCII-String-Type| :short-description "")
	 (make-parameter '|ENG_STARTTIME| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_STATE| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_TMPFILE| '|/ASCII-String-Type| :short-description "")
	 (make-parameter '|ENG_TRANSLEN| '|/U8-Type| :short-description "")
	 (make-parameter '|ENG_TRANSNUM| '|/U32-Type| :short-description "")
	 (make-parameter '|ENG_TRANSVAL| '|/U8-Type| :short-description "")
	 (make-parameter '|ERLOGENTRIES| '|/U32-Type| :short-description "Number of entries made in the ER Log since the power on.")
	 (make-parameter '|ERLOGINDEX| '|/U32-Type| :short-description "Current index of the ER Log (wraps around).")
	 (make-parameter '|ERRCOUNTER| '|/U8-Type|  :short-description "The ES Application Command Error Counter.")
	 (make-parameter '|FAILEDVALCTR| '|/U8-Type| :short-description "Total number of unsuccessful table validations.")
	 (make-parameter '|HEAPBLOCKSFREE| '|/U32-Type| :short-description "Number of free blocks remaining in the OS heap.")
	 (make-parameter '|HEAPBYTESFREE| '|/U32-Type| :short-description "Number of free bytes remaining in the OS heap.")
	 (make-parameter '|HEAPMAXBLOCKSIZE| '|/U32-Type| :short-description "Number of bytes in the largest free block.")
	 (make-parameter '|INTERNALERRCNT| '|/U8-Type| :short-description "Count of queue read or write errors.")
	 (make-parameter '|LASTFILEDUMPED| '|/ASCII-String-Type| :short-description "Path and Name of last file dumped to.")
	 (make-parameter '|LASTFILELOADED| '|/ASCII-String-Type| :short-description "Path and Name of last table image file loaded.")
	 (make-parameter '|LASTTABLELOADED| '|/ASCII-String-Type| :short-description "Name of the last table loaded.")
	 (make-parameter '|LASTUPDATEDTBL| '|/ASCII-String-Type| :short-description "Name of the last table updated.")
	 (make-parameter '|LASTUPDATETIME_SECONDS| '|/U32-Type| :short-description "")
	 (make-parameter '|LASTUPDATETIME_SUBSECONDS| '|/U32-Type| :short-description "")
	 (make-parameter '|LASTVALCRC| '|/32-Bit-Checksum-Type| :short-description "Data Integrity Value computed for last table validated.")
	 (make-parameter '|LASTVALSTATUS| '|/U8-Type| :short-description "Returned status from validation function for last table validated.")
	 (make-parameter '|LASTVALTABLENAME| '|/ASCII-String-Type| :short-description "Name of last table validated.")
	 (make-parameter '|LEAPSECONDS| '|/U16-Type| :short-description "Current Leaps Seconds.")
	 (make-parameter '|LOGENABLED| '|/U8-Type| :short-description "Current event log enable/disable state.")
	 (make-parameter '|LOGFULLFLAG| '|/U8-Type| :short-description "Local event log full flag.")
	 (make-parameter '|LOGMODE| '|/U8-Type| :short-description "Local event logging mode (overwrite/discard).")
	 (make-parameter '|LOGOVERFLOWCOUNTER| '|/U8-Type| :short-description "Local event log overflow counter.")
	 (make-parameter '|MAXPROCESSORRESETS| '|/U32-Type| :short-description "Max processor resets before a power on is done.")
	 (make-parameter '|MEMINUSE| '|/U32-Type| :short-description "Memory in use.")
	 (make-parameter '|MEMPOOLHANDLE| '|/U32-Type| :short-description "Handle to TBL's memory pool.")
	 (make-parameter '|MESSAGEFORMATMODE| '|/U8-Type| :short-description "Event message format mode (short/long).")
	 (make-parameter '|MESSAGESENDCOUNTER| '|/U8-Type| :short-description "Event message send counter.")
	 (make-parameter '|MESSAGETRUNCCOUNTER| '|/U8-Type| :short-description "Event message truncation counter.")
	 (make-parameter '|MESSAGE| '|/ASCII-String-Type| :short-description "Event message string.")
	 (make-parameter '|MSGLIMERRCNT| '|/U16-Type| :short-description "Count of msg id to pipe errors.")
	 (make-parameter '|MSGRECEIVEERRCNT| '|/U8-Type| :short-description "Count of message receive errors.")
	 (make-parameter '|MSGSENDERRCNT| '|/U8-Type| :short-description "Count of message send errors.")
	 (make-parameter '|NOSUBSCRIBERSCNT| '|/U8-Type| :short-description "Count pkts sent with no subscribers.")
	 (make-parameter '|NUMFREESHAREDBUFS| '|/U8-Type| :short-description "Number of free Shared Working Buffers.")
	 (make-parameter '|NUMLOADPENDING| '|/U16-Type| :short-description "Number of Tables pending on Applications for their update.")
	 (make-parameter '|NUMTABLES| '|/U16-Type| :short-description "Number of Tables Registered.")
	 (make-parameter '|NUMVALREQUESTS| '|/U8-Type| :short-description "Number of times Table Services has requested validations from Apps.")
	 (make-parameter '|OSALMAJORVERSION| '|/U8-Type| :short-description "OS Abstraction Layer Major Version Number.")
	 (make-parameter '|OSALMINORVERSION| '|/U8-Type| :short-description "OS Abstraction Layer Minor Version Number.")
	 (make-parameter '|OSALMISSIONREVISION| '|/U8-Type| :short-description "OS Abstraction Layer MissionRevision Number.")
	 (make-parameter '|OSALREVISION| '|/U8-Type| :short-description "OS Abstraction Layer Revision Number.")
	 (make-parameter '|OUTPUTPORT| '|/U8-Type| :short-description "Output port mask.")
	 (make-parameter '|PACKETID_APPNAME| '|/ASCII-String-Type| :short-description "")
	 (make-parameter '|PACKETID_EVENTID| '|/U16-Type| :short-description "")
	 (make-parameter '|PACKETID_EVENTTYPE| '|/U16-Type| :short-description "")
	 (make-parameter '|PACKETID_PROCESSORID| '|/U32-Type| :short-description "")
	 (make-parameter '|PACKETID_SPACECRAFTID| '|/U32-Type| :short-description "")
	 (make-parameter '|PERFDATACOUNT| '|/U32-Type| :short-description "Number of Entries Put Into the Performance Analyzer Log.")
	 (make-parameter '|PERFDATAEND| '|/U32-Type| :short-description "Identifies Last Stored Entry in Performance Analyzer Log.")
	 (make-parameter '|PERFDATASTART| '|/U32-Type| :short-description "Identifies First Stored Entry in Performance Analyzer Log.")
	 (make-parameter '|PERFDATATOWRITE| '|/U32-Type| :short-description "Number of Performance Analyzer Log Entries Left to be Written to Log DumpFile.")
	 (make-parameter '|PERFFILTERMASK| '|PERFFILTERMASK-Type| :short-description "Current Setting of Performance Analyzer Filter Masks.")
	 (make-parameter '|PERFMODE| '|/U32-Type| :short-description "Current mode of Performance Analyzer.")
	 (make-parameter '|PERFSTATE| '|/U32-Type| :short-description "Current state of Performance Analyzer.")
	 (make-parameter '|PERFTRIGGERCOUNT| '|/U32-Type| :short-description "Number of Times Perfomance Analyzer has Triggered.")
	 (make-parameter '|PERFTRIGGERMASK| '|PERFTRIGGERMASK-Type| :short-description "Current Setting of Performance Analyzer Trigger Masks.")
	 (make-parameter '|PIPEOVERFLOWERRCNT| '|/U16-Type| :short-description "Count of pipe overflow errors.")
	 (make-parameter '|PROCESSORRESETS| '|/U32-Type| :short-description "Number of processor resets since last power on.")
	 (make-parameter '|REGISTEREDCOREAPPS| '|/U32-Type| :short-description "Number of Applications registered with ES.")
	 (make-parameter '|REGISTEREDEXTERNALAPPS| '|/U32-Type| :short-description "Number of Applications registered with ES.")
	 (make-parameter '|REGISTEREDLIBS| '|/U32-Type| :short-description "Number of Libraries registered with ES.")
	 (make-parameter '|REGISTEREDTASKS| '|/U32-Type| :short-description "Number of Tasks ( main AND child tasks ) registered with ES.")
	 (make-parameter '|RESETSUBTYPE| '|/U32-Type| :short-description "Reset Sub Type.")
	 (make-parameter '|RESETTYPE| '|/U32-Type| :short-description "Reset type ( PROCESSOR or POWERON ).")
	 (make-parameter '|SECONDS1HZADJ| '|/U32-Type| :short-description "Current 1 Hz SCTF adjustment (seconds).")
	 (make-parameter '|SECONDSMET| '|/U32-Type| :short-description "Current MET (seconds).")
	 (make-parameter '|SECONDSSTCF| '|/U32-Type| :short-description "Current STCF (seconds).")
	 (make-parameter '|SPARE| '|/U8-Type| :short-description "Structure padding.")
	 (make-parameter '|SUBSCRIBEERRCNT| '|/U8-Type| :short-description "Count of errors in subscribe API.")
	 (make-parameter '|SUBSECS1HZADJ| '|/U32-Type| :short-description "Current 1 Hz SCTF adjustment (sub-seconds).")
	 (make-parameter '|SUBSECSMET| '|/U32-Type| :short-description "Current MET (sub-seconds).")
	 (make-parameter '|SUBSECSSTCF| '|/U32-Type| :short-description "Current STCF (sub-seconds).")
	 (make-parameter '|SUCCESSVALCTR| '|/U8-Type| :short-description "Total number of successful table validations.")
	 (make-parameter '|SYSLOGBYTESUSED| '|/U32-Type| :short-description "Total number of bytes used in system log.")
	 (make-parameter '|SYSLOGENTRIES| '|/U32-Type| :short-description "Number of entries in the system log.")
	 (make-parameter '|SYSLOGMODE| '|/U32-Type| :short-description "Write/Overwrite Mode.")
	 (make-parameter '|SYSLOGSIZE| '|/U32-Type| :short-description "Total size of the system log.")
	 (make-parameter '|UNMARKEDMEM| '|/U32-Type| :short-description "cfg param CFE_SB_BUF_MEMORY_BYTES minus Peak Memory in use")
	 (make-parameter '|UNREGISTEREDAPPCOUNTER| '|/U8-Type| :short-description "Unregistered application message send counter.")
	 (make-parameter '|UP_FAILEDCOUNTER| '|/U32-Type| :short-description "")
	 (make-parameter '|UP_LASTFILEUPLINKED| '|/ASCII-String-Type| :short-description "")
	 (make-parameter '|UP_METACOUNT| '|/U32-Type| :short-description "")
	 (make-parameter '|UP_SUCCESSCOUNTER| '|/U32-Type| :short-description "")
	 (make-parameter '|UP_UPLINKACTIVEQFILECNT| '|/U32-Type| :short-description "")
	 (make-parameter '|VALIDATIONCTR| '|/U16-Type| :short-description "Number of completed table validations."))))

    (make-space-system
	 '|CS|
	 :short-description "Checksum-Application"
	 :telemetry-metadata
	 (make-telemetry-metadata
	  :parameter-set
	  (make-parameter-set
	   (make-parameter '|CHECKSUMSTATE| '|/U8-Type| :short-description "CS Application global checksum state.")
	   (make-parameter '|EEPROMCSSTATE| '|/U8-Type| :short-description "CS Eeprom table checksum state.")
	   (make-parameter '|MEMORYCSSTATE| '|/U8-Type| :short-description "CS Memory table checksum state.")
	   (make-parameter '|APPCSSTATE| '|/U8-Type| :short-description "CS App table checksum state.")
	   (make-parameter '|TABLESCSSTATE| '|/U8-Type| :short-description "CS Tables table checksum state.")
	   (make-parameter '|OSCSSTATE| '|/U8-Type| :short-description "OS code segment checksum state.")
	   (make-parameter '|CFECORECSSTATE| '|/U8-Type| :short-description "cFE Core code segment checksum state.")
	   (make-parameter '|CHILDTASKINUSE| '|/U8-Type| :short-description " CS 'Child Task In Use' flag.")
	   (make-parameter '|ONESHOTTASKINUSE| '|/U8-Type| :short-description "CS 'OneShot Task In Use' flag.")
	   (make-parameter '|EEPROMCSERRCOUNTER| '|/U16-Type| :short-description "Eeprom miscompare counter.")
	   (make-parameter '|MEMORYCSERRCOUNTER| '|/U16-Type| :short-description "Memory miscompare counter.")
	   (make-parameter '|APPCSERRCOUNTER| '|/U16-Type| :short-description "App miscompare counter.")
	   (make-parameter '|TABLESCSERRCOUNTER| '|/U16-Type| :short-description "Tables miscompare counter.")
	   (make-parameter '|CFECORECSERRCOUNTER| '|/U16-Type| :short-description "cFE core miscompare counter.")
	   (make-parameter '|OSCSERRCOUNTER| '|/U16-Type| :short-description "OS code segment miscopmare counter.")
	   (make-parameter '|CURRENTCSTABLE| '|/U16-Type| :short-description "Current table being checksummed.")
	   (make-parameter '|CURRENTENTRYINTABLE| '|/U16-Type| :short-description "Current entry ID in the table being checksummed.")
	   (make-parameter '|EEPROMBASELINE| '|/U32-Type| :short-description "Baseline checksum for all of Eeprom.")
	   (make-parameter '|OSBASELINE| '|/U32-Type| :short-description "Baseline checksum for the OS code segment.")
	   (make-parameter '|CFECOREBASELINE| '|/U32-Type| :short-description "Basline checksum for the cFE core.")
	   (make-parameter '|LASTONESHOTADDRESS| '|/U32-Type| :short-description "Address used in last one shot checksum command.")
	   (make-parameter '|LASTONESHOTSIZE| '|/U32-Type| :short-description "Size used in the last one shot checksum command.")
	   (make-parameter '|LASTONESHOTCHECKSUM| '|/U32-Type| :short-description "Checksum of the last one shot checksum command.")
	   (make-parameter '|PASSCOUNTER| '|/U32-Type| :short-description "Number of times CS has passed through all of its tables."))))

  (make-space-system
   '|DS|
   :short-description "Data-Storage-Application"
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-set
	(make-parameter-set
	 (make-parameter '|DESTTBLLOADCOUNTER| '|/U8-Type| :short-description "Count of destination file table loads.")
	 (make-parameter '|DESTTBLERRCOUNTER| '|/U8-Type| :short-description "Count of failed attempts to get table data pointer.")
	 (make-parameter '|FILTERTBLLOADCOUNTER| '|/U8-Type| :short-description "Count of packet filter table loads.")
	 (make-parameter '|FILTERTBLERRCOUNTER| '|/U8-Type| :short-description "Count of failed attempts to get table data pointer.")
	 (make-parameter '|APPENABLESTATE| '|/U8-Type| :short-description "Application enable/disable state.")
	 (make-parameter '|FILEWRITECOUNTER| '|/U16-Type| :short-description "Count of good destination file writes.")
	 (make-parameter '|FILEWRITEERRCOUNTER| '|/U16-Type| :short-description "Count of bad destination file writes.")
	 (make-parameter '|FILEUPDATECOUNTER| '|/U16-Type| :short-description "Count of good updates to secondary header.")
	 (make-parameter '|FILEUPDATEERRCOUNTER| '|/U16-Type| :short-description "Count of bad updates to secondary header.")
	 (make-parameter '|DISABLEDPKTCOUNTER| '|/U16-Type| :short-description "Count of packets discarded (DS was disabled).")
	 (make-parameter '|IGNOREDPKTCOUNTER| '|/U32-Type| :short-description "Count of packets discarded. Incoming packets will be discarded when:.")
	 (make-parameter '|FILTEREDPKTCOUNTER| '|/U32-Type| :short-description "Count of packets discarded (failed filter test).")
	 (make-parameter '|PASSEDPKTCOUNTER| '|/U32-Type| :short-description "Count of packets that passed filter test."))))


  (make-space-system
   '|CFE_SB|
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-type-set
	(make-parameter-type-set
	 
	 (make-array-parameter-type
	  '|SPARE2ALIGN|
	  '|/U8-Type|
	  :short-description "Spare bytes to ensure alignment."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 1))))))))

  (make-space-system
   '|CF_TLM|
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-type-set
	(make-parameter-type-set
	 
	 (make-array-parameter-type
	  '|ENG_SPARE|
	  '|/U8-Type|
	  :short-description "3 Spare bytes."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 2))))))))

   (make-space-system
   '|FM|
   :short-description "File Manager Application"
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-type-set
	(make-parameter-type-set
	 
	 (make-array-parameter-type
	  '|SPARE|
	  '|/U8-Type|
	  :short-description "Structure padding."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 1))))))
	:parameter-set
	(make-parameter-set
	 (make-parameter '|DIRNAME| '|/U8-Type| :short-description "Directory Name.")
	 (make-parameter '|TOTALFILES| '|/U32-Type| :short-description "Number of files in the directory.")
	 (make-parameter '|PACKETFILES| '|/U32-Type| :short-description "Number of files in this packet.")
	 (make-parameter '|FIRSTFILE| '|/U32-Type| :short-description "Index into directory files of first packet file.")
	 (make-parameter '|FILESTATUS| '|/U32-Type| :short-description "Status indicating whether the file is open or closed.")
	 (make-parameter '|FILESIZE| '|/U32-Type| :short-description "File Size.")
	 (make-parameter '|LASTMODIFIEDTIME| '|/U32-Type| :short-description "Last Modification Time of File.")
	 (make-parameter '|FILENAME| '|/ASCII-String-Type| :short-description "Name of file.")
	 (make-parameter '|NUMOPENFILES-8| '|/U8-Type| :short-description "Number of open files in the system.")
	 (make-parameter '|CHILDCMDCOUNTER| '|/U8-Type| :short-description "Child task command counter.")
	 (make-parameter '|CHILDCMDERRCOUNTER| '|/U8-Type| :short-description "Child task command error counter.")
	 (make-parameter '|CHILDCMDWARNCOUNTER| '|/U8-Type| :short-description "Child task command warning counter.")
	 (make-parameter '|CHILDQUEUECOUNT| '|/U8-Type| :short-description "Number of pending commands in queue.")
	 (make-parameter '|CHILDCURRENTCC| '|/U8-Type| :short-description "Command code currently executing.")
	 (make-parameter '|CHILDPREVIOUSCC| '|/U8-Type| :short-description "Command code previously executed.")
	 (make-parameter '|NUMOPENFILES-32| '|/U32-Type| :short-description "Number of files opened via cFE."))))
   
   (make-space-system
   '|HK|
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-set
	(make-parameter-set
	 (make-parameter '|COMBINEDPACKETSSENT| '|/U16-Type| :short-description "Count of combined tlm pkts sent.")
	 (make-parameter '|MISSINGDATACTR| '|/U16-Type| :short-description "Number of times missing data was detected.")
	 (make-parameter '|MESSAGEFORMATMODE| '|/U8-Type| :short-description "EVS  Event message format mode (short/long).") ;TODO: Check type
	 (make-parameter '|MESSAGETRUNCCOUNTER| '|/U8-Type| :short-description "EVS  Event message truncation counter.")
	 (make-parameter '|TIME_CMDCOUNTER| '|/U8-Type| :short-description "TIME Time Command Execution Counter.")
	 (make-parameter '|TIME_ERRCOUNTER| '|/U8-Type| :short-description "TIME Time Command Error Counter.")
	 (make-parameter '|CLOCKSTATEFLAGS| '|/U16-Type| :short-description "TIME State Flags.")
	 (make-parameter '|SB_COMMANDCNT| '|/U8-Type| :short-description "SB Count of valid commands received.")
	 (make-parameter '|SB_CMDERRCNT| '|/U8-Type| :short-description "SB Count of invalid commands received.")
	 (make-parameter '|NOSUBSCRIBERSCNT| '|/U8-Type| :short-description "SB Count pkts sent with no subscribers.")
	 (make-parameter '|MSGSENDERRCNT| '|/U8-Type| :short-description "SB Count of message send errors.")
	 (make-parameter '|ES_CMDCOUNTER| '|/U8-Type| :short-description "The ES Application Command Counter.")
	 (make-parameter '|ES_ERRCOUNTER| '|/U8-Type| :short-description "The ES Application Command Error Counter.")
	 (make-parameter '|CFECORECHECKSUM| '|/U16-Type| :short-description "Checksum of cFE Core Code.") ;TODO: Investigate type
	 (make-parameter '|TBL_CMDCOUNTER| '|/U8-Type| :short-description "TBL  Count of valid commands received.")
	 (make-parameter '|TBL_ERRCOUNTER| '|/U8-Type| :short-description "TBL  Count of invalid commands received.")
	 (make-parameter '|NUMTABLES| '|/U16-Type| :short-description "TBL Number of Tables Registered."))))
   
  (make-space-system
   '|HS|
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-type-set
	(make-parameter-type-set
	 (make-array-parameter-type
	  '|App-Mon-Enables-Type|
	  '|/U32-Type|
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 1))))))
	:parameter-set
	(make-parameter-set
	 (make-parameter '|CURRENTAPPMONSTATE| '|/U8-Type| :short-description "Status of HS Critical Application Monitor.")
	 (make-parameter '|CURRENTEVENTMONSTATE| '|/U8-Type| :short-description "Status of HS Critical Events Monitor.")
	 (make-parameter '|CURRENTALIVENESSSTATE| '|/U8-Type| :short-description "Status of HS Aliveness Indicator.")
	 (make-parameter '|CURRENTCPUHOGSTATE| '|/U8-Type| :short-description "Status of HS Hogging Indicator.")
	 (make-parameter '|STATUSFLAGS| '|/U8-Type| :short-description "Internal HS Error States.")
	 (make-parameter '|RESETSPERFORMED| '|/U16-Type| :short-description "HS Performed Processor Reset Count.")
	 (make-parameter '|MAXRESETS| '|/U16-Type| :short-description "HS Maximum Processor Reset Count.")
	 (make-parameter '|EVENTSMONITOREDCOUNT| '|/U16-Type| :short-description "Total count of Event Messages Monitored by the Critical Events Monitor.")
	 (make-parameter '|INVALIDEVENTMONCOUNT| '|/U32-Type| :short-description "Total count of Invalid Event Monitors Monitored by the Critical Events Monitor.")
	 (make-parameter '|APPMONENABLES| '|App-Mon-Enables-Type| :short-description "Enable states of App Monitor Entries.")
	 (make-parameter '|MSGACTEXEC| '|/U32-Type| :short-description "Number of Software Bus Message Actions Executed.")
	 (make-parameter '|UTILCPUAVG| '|/U32-Type| :short-description "Current CPU Utilization Average.")
	 (make-parameter '|UTILCPUPEAK| '|/U32-Type| :short-description "Current CPU Utilization Peak."))))


    (make-space-system
   '|LC|
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-type-set
	(make-parameter-type-set
	 
	 (make-array-parameter-type
	  '|WPRESULTS-Type|
	  '|/U8-Type|
	  :short-description "Packed watchpoint results data, 2 bits per watchpoint."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 47)))))

	 (make-array-parameter-type
	  '|APRESULTS-Type|
	  '|/U8-Type|
	  :short-description "Packed actionpoint results data, 4 bits per actionpoint."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 91))))))

	:parameter-set
	(make-parameter-set
	 (make-parameter '|CURRENTLCSTATE| '|/U8-Type| :short-description "Current LC application operating state.")
	 (make-parameter '|WPRESULTS| '|WPRESULTS-Type| :short-description "Packed watchpoint results data, 2 bits per watchpoint.")
	 (make-parameter '|APRESULTS| '|APRESULTS| :short-description "Packed actionpoint results data, 4 bits per actionpoint.")
	 (make-parameter '|PASSIVERTSEXECCOUNT| '|/U16-Type| :short-description "Total count of RTS sequences not initiated because the LC state is set to LC_STATE_PASSIVE.")
	 (make-parameter '|WPSINUSE| '|/U16-Type| :short-description "How many watchpoints are currently in effect.")
	 (make-parameter '|ACTIVEAPS| '|/U16-Type| :short-description "How many actionpoints are currently active.")
	 (make-parameter '|APSAMPLECOUNT| '|/U32-Type| :short-description "Total count of Actionpoints sampled.")
	 (make-parameter '|MONITOREDMSGCOUNT| '|/U32-Type| :short-description "Total count of messages monitored for watchpoints.")
	 (make-parameter '|RTSEXECCOUNT| '|/U32-Type| :short-description "Total count of RTS sequences initiated."))))


   (make-space-system
   '|MD|
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-type-set
	(make-parameter-type-set
	 
	 (make-array-parameter-type
	  '|DATA-Type|
	  '|/U8-Type|
	  :short-description "Dwell data ( number of bytes varies up to MD_DWELL_TABLE_SIZE *4)."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 99)))))

	 (make-array-parameter-type
	  '|DWELLTBLADDRCOUNT-Type|
	  '|/U16-Type|
	  :short-description "Number of dwell addresses in table."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 7)))))

	  (make-array-parameter-type
	  '|NUMWAITSPERPKT-Type|
	  '|/U16-Type|
	  :short-description "Numer of delay counts in table."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 7)))))

	  (make-array-parameter-type
	  '|BYTECOUNT-Type|
	  '|/U16-Type|
	  :short-description "Number of bytes of data specified by table."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 7)))))

	  (make-array-parameter-type
	  '|DWELLPKTOFFSET-Type|
	  '|/U16-Type|
	  :short-description "Current write offset within dwell pkt data region."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 7)))))

	  (make-array-parameter-type
	  '|DWELLTBLENTRY-Type|
	  '|/U16-Type|
	  :short-description "Next dwell table entry to be processed."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 7)))))

	  (make-array-parameter-type
	  '|COUNTDOWN-Type|
	  '|/U16-Type|
	  :short-description "Current value of countdown timer."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 7))))))
	:parameter-set
	(make-parameter-set
 	 (make-parameter '|TABLEID| '|/U8-Type| :short-description "TableId from 1 to MD_NUM_DWELL_TABLES.")
	 (make-parameter '|ADDRCOUNT| '|/U8-Type| :short-description "Number of addresses being sent - 1..MD_DWELL_TABLE_SIZE valid.")
	 (make-parameter '|BYTECOUNT| '|/U16-Type| :short-description "Number of bytes of dwell data contained in packet.")
	 (make-parameter '|RATE| '|/U32-Type| :short-description "Number of counts between packet sends.")
	 (make-parameter '|DATA| '|DATA-Type| :short-description "Dwell data ( number of bytes varies up to MD_DWELL_TABLE_SIZE *4).")
	 (make-parameter '|INVALIDCMDCNTR| '|/U8-Type| :short-description "Count of invalid commands received.")
	 (make-parameter '|VALIDCMDCNTR| '|/U8-Type| :short-description "Count of valid commands received.")
	 (make-parameter '|DWELLENABLEDMASK| '|/U16-Type| :short-description "Each bit in bit mask enables a table 0x0001=TBL1 enable bit,0x0002=TBL2 enable bit, 0x0004=TBL3 enable bit,0x0008=TBL4 enable bit, etc.") ;TODO: Explore new types
	 (make-parameter '|DWELLTBLADDRCOUNT| '|DWELLTBLADDRCOUNT-Type| :short-description "Number of dwell addresses in table.")
	 (make-parameter '|NUMWAITSPERPKT| '|NUMWAITSPERPKT-Type| :short-description "Number of delay counts in table.")
	 (make-parameter '|BYTECOUNT-Array| '|BYTECOUNT-Type| :short-description "Number of bytes of data specified by table.")
	 (make-parameter '|DWELLPKTOFFSET| '|DWELLPKTOFFSET-Type| :short-description "Current write offset within dwell pkt data region.")
	 (make-parameter '|DWELLTBLENTRY| '|DWELLTBLENTRY-Type| :short-description "Next dwell table entry to be processed.")
	 (make-parameter '|COUNTDOWN| '|COUNTDOWN-Type| :short-description "Current value of countdown timer."))))

   (make-space-system
	'|MM|
	:short-description "Memmory Manager Application"
	:telemetry-metadata
	(make-telemetry-metadata
	 :parameter-set
	 (make-parameter-set
	  (make-parameter '|LASTACTION| '|/U8-Type| :short-description "Last command action executed.")
	  (make-parameter '|MEMTYPE| '|/U8-Type| :short-description "Memory type for last command.")
	  (make-parameter '|ADDRESS| '|/U32-Type| :short-description "Fully resolved address used for last command.")
	  (make-parameter '|DATAVALUE| '|/U32-Type| :short-description "Last command data value -- may be fill pattern or peek/poke value.")
	  (make-parameter '|BYTESPROCESSED| '|/U32-Type| :short-description "Bytes processed for last command.")
	  (make-parameter '|FILENAME| '|/ASCII-String-Type| :short-description "Name of the data file used for last command, where applicable."))))

  (make-space-system
   '|SCH|
   :short-description "Scheduler Application"
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-type-set
	(make-parameter-type-set
	 
	 (make-array-parameter-type
	  '|ENTRYSTATES-Type|
	  '|/U16-Type|
	  :short-description "" 
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 127)))))

	 (make-array-parameter-type
	  '|MSGIDS-Type|
	  '|/U16-Type|
	  :short-description ""
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 999))))))
	:parameter-set
	(make-parameter-set
	 (make-parameter '|ENTRYSTATES| '|ENTRYSTATES-Type| :short-description "States of each Schedule Entry.")
	 (make-parameter '|MSGIDS| '|/U8-Type| :short-description "Message ID of msg associated with each entry.")
	 (make-parameter '|SYNCTOMET| '|/U8-Type| :short-description "Status indicating whether slots are synched to MET.")
	 (make-parameter '|MAJORFRAMESOURCE| '|/U8-Type| :short-description "Major Frame Signal source identifier.")
	 (make-parameter '|SCHEDULEACTIVITYSUCCESSCOUNT| '|/U32-Type| :short-description "Number of successfully performed activities.")
	 (make-parameter '|SCHEDULEACTIVITYFAILURECOUNT| '|/U32-Type| :short-description "Number of unsuccessful activities attempted.")
	 (make-parameter '|SLOTSPROCESSEDCOUNT| '|/U32-Type| :short-description "'Total # of Schedule Slots (Minor Frames) Processed.'")
	 (make-parameter '|SKIPPEDSLOTSCOUNT| '|/U16-Type| :short-description "Number of times that slots were skipped.")
	 (make-parameter '|MULTIPLESLOTSCOUNT| '|/U16-Type| :short-description "Number of times that multiple slots processed.")
	 (make-parameter '|SAMESLOTCOUNT| '|/U16-Type| :short-description "'# of times SCH woke up in the same slot as last time'")
	 (make-parameter '|BADTABLEDATACOUNT| '|/U16-Type| :short-description "'# of times corrupted table entries were processed'")
	 (make-parameter '|TABLEVERIFYSUCCESSCOUNT| '|/U16-Type| :short-description "'# of times table loads successfully verified'")
	 (make-parameter '|TABLEVERIFYFAILURECOUNT| '|/U16-Type| :short-description "'# of times table loads unsuccessfully verified'")
	 (make-parameter '|TABLEPASSCOUNT| '|/U32-Type| :short-description "'# of times Schedule Table has been processed'")
	 (make-parameter '|VALIDMAJORFRAMECOUNT| '|/U32-Type| :short-description "'# of valid Major Frame tones received'")
	 (make-parameter '|MISSEDMAJORFRAMECOUNT| '|/U32-Type| :short-description "'# of missing Major Frame tones'")
	 (make-parameter '|UNEXPECTEDMAJORFRAMECOUNT| '|/U32-Type| :short-description "'# of unexpected Major Frame tones'")
	 (make-parameter '|MINORFRAMESSINCETONE| '|/U16-Type| :short-description "'# of Minor Frames since last Major Frame tone'")
	 (make-parameter '|NEXTSLOTNUMBER| '|/U16-Type| :short-description "Next Minor Frame to be processed.")
	 (make-parameter '|LASTSYNCMETSLOT| '|/U16-Type| :short-description "Slot number where Time Sync last occurred.")
	 (make-parameter '|IGNOREMAJORFRAME| '|/U8-Type| :short-description "Major Frame too noisy to trust.")
	 (make-parameter '|UNEXPECTEDMAJORFRAME| '|/U8-Type| :short-description "Most Recent Major Frame signal was unexpected."))))

   (make-space-system
   '|SC|
   :telemetry-metadata
   (make-telemetry-metadata
	:parameter-type-set
	(make-parameter-type-set
	 
	 (make-array-parameter-type
	  '|ATPFREEBYTES-Type|
	  '|/U32-Type|
	  :short-description "Free Bytes in each ATS."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 7)))))

	 (make-array-parameter-type
	  '|RTSEXECUTINGSTATUS-Type|
	  '|/U16-Type|
	  :short-description "RTS executing status bit map where each uint16 represents 16 RTS numbers.
      Note: array index numbers and bit numbers use base zero indexing, but RTS numbers
      use base one indexing. Thus, the LSB (bit zero) of uint16 array index zero represents
      RTS number 1, and bit one of uint16 array index zero represents RTS number 2,
      etc. If an RTS is IDLE, then the corresponding bit is zero. If an RTS is EXECUTING,
      then the corresponding bit is one."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 9)))))

	 (make-enumerated-parameter-type
	  '|ATSNUMBER-Type|
	  :enumeration-list
	  (make-enumeration-list
	   (make-enumeration '|ATS-A| 1)
	   (make-enumeration '|ATS-B| 2)))

	 (make-enumerated-parameter-type
	  '|ATPSTATE-Type|
	  :enumeration-list
	  (make-enumeration-list
	   (make-enumeration '|IDLE| 2)
	   (make-enumeration '|EXECUTING| 5)))

	 (make-array-parameter-type
	  '|RTSDISABLEDSTATUS-Type|
	  '|/U16-Type|
	  :short-description "RTS disabled status bit map where each uint16 represents 16 RTS numbers.
      Note: array index numbers and bit numbers use base zero indexing, but RTS numbers
      use base one indexing. Thus, the LSB (bit zero) of uint16 array index zero represents
      RTS number 1, and bit one of uint16 array index zero represents RTS number 2,
      etc. If an RTS is ENABLED, then the corresponding bit is zero. If an RTS is
      DISABLED, then the corresponding bit is one."
	  :dimension-list (make-dimension-list (make-dimension
											(make-starting-index (make-fixed-value 0))
											(make-ending-index (make-fixed-value 9))))))

	:parameter-set
	(make-parameter-set
	 (make-parameter '|ATSNUMBER| '|ATSNUMBER-Type| :short-description "current ATS number 1 = ATS A, 2 = ATS B")
	 (make-parameter '|ATPSTATE| '|ATPSTATE-Type| :short-description "'current ATP state valid values are: 2 = IDLE, 5 = EXECUTING'")
	 (make-parameter '|CONTINUEATSONFAILUREFLAG| '|/U8-Type| :short-description "In the event of ATS execution failure (ats command fails checksum) , the
      ATS execution will continue if this flag is set to TRUE and will stop if this flag is set to FALSE.")
	 (make-parameter '|CMDERRCTR| '|/U8-Type| :short-description "Counts Request Errors.")
	 (make-parameter '|CMDCTR| '|/U8-Type| :short-description "Counts Ground Requests.")
	 (make-parameter '|SWITCHPENDFLAG| '|/on-off-enum-type| :short-description "Is an ats switch pending? 0 = NO, 1 = YES This means that the ATS switch
      is waiting until a safe time")
	 (make-parameter '|NUMRTSACTIVE| '|/U16-Type| :short-description "number of RTSs currently active")
	 (make-parameter '|RTSNUMBER| '|/U16-Type| :short-description "next RTS number")
	 (make-parameter '|RTSACTIVECTR| '|/U16-Type| :short-description "Increments when an RTS is started without error.")
	 (make-parameter '|RTSACTIVEERRCTR| '|/U16-Type| :short-description "Increments when an attempt to start an RTS fails.")
	 (make-parameter '|ATSCMDCTR| '|/U16-Type| :short-description "Total ATS cmd cnter counts commands sent by the ATS.")
	 (make-parameter '|ATSCMDERRCTR| '|/U16-Type| :short-description "Total ATS cmd Error ctr command errors in the ATS.")
	 (make-parameter '|RTSCMDCTR| '|/U16-Type| :short-description "Counts TOTAL rts cmds that were sent out from ALL active RTSs.")
	 (make-parameter '|RTSCMDERRCTR| '|/U16-Type| :short-description "Counts TOTAL number of errs from ALL RTSs that are active.")
	 (make-parameter '|LASTATSERRSEQ| '|/U16-Type| :short-description "'Last ATS Errant Sequence Num Values: 1 or 2.'")
	 (make-parameter '|LASTATSERRCMD| '|/U16-Type| :short-description "Last ATS Errant Command Num.")
	 (make-parameter '|LASTRTSERRSEQ| '|/U16-Type| :short-description "Last RTS Errant Sequence Num.")
	 (make-parameter '|LASTRTSERRCMD| '|/U16-Type| :short-description "The OFFSET in the RTS buffer of the command that had an error It will be
      a WORD value i.e. 1st command had an error, this value would be 0, if the 2nd
      command started at int8 10 in the buffer, this value would be 5.")
	 (make-parameter '|APPENDCMDARG| '|/U16-Type| :short-description "ATS selection argument from most recent Append ATS command.")
	 (make-parameter '|APPENDENTRYCOUNT| '|/U16-Type| :short-description "Number of cmd entries in current Append ATS table.")
	 (make-parameter '|APPENDBYTECOUNT| '|/U16-Type| :short-description "Size of cmd entries in current Append ATS table.")
	 (make-parameter '|APPENDLOADCOUNT| '|/U16-Type| :short-description "Total number of Append ATS table loads.")
	 (make-parameter '|ATPCMDNUMBER| '|/U32-Type| :short-description "current command number")
	 (make-parameter '|ATPFREEBYTES| '|ATPFREEBYTES-Type| :short-description "Free Bytes in each ATS.")
	 (make-parameter '|NEXTRTSTIME| '|/U32-Type| :short-description "next RTS cmd Absolute Time")
	 (make-parameter '|NEXTATSTIME| '|/U32-Type| :short-description "Next ATS Command Time (seconds).")
	 (make-parameter '|RTSEXECUTINGSTATUS| '|RTSEXECUTINGSTATUS-Type| :short-description "")
	 (make-parameter '|RTSDISABLEDSTATUS| '|RTSDISABLEDSTATUS-Type| :short-description ""))))

   (make-space-system
	'|TO|
	:short-description "Telemetry Output Application"
	:telemetry-metadata
	(make-telemetry-metadata
	 :parameter-set
	 (make-parameter-set
	  (make-parameter '|TO_MESSAGE_SUB_COUNT| '|/U16-Type| :short-description "Count of subscribed messages by all telemetry pipe.")
	  (make-parameter '|TO_MESSAGE_SUB_ERROR_COUNT| '|/U16-Type| :short-description "Count of subscription errors")
	  (make-parameter '|TO_TABLE_UPDATE_COUNT| '|/U16-Type| :short-description "Count of table updates through CFE_TBL")
	  (make-parameter '|TO_TABLE_UPDATE_ERROR_COUNT| '|/U16-Type| :short-description "Count of table update errors")
	  (make-parameter '|TO_CONFIG_ROUTES| '|/U16-Type| :short-description "Current mask of configured routes")
	  (make-parameter '|TO_ENABLED_ROUTES| '|/U16-Type| :short-description "Current mask of enabled routes")
	  (make-parameter '|SYNCH| '|/U16-Type| :short-description "")
	  (make-parameter '|TO_BITS| '|/U16-Type| :short-description "'16 bits total, broken down as: bit1:1; bit2:1; bit34:2; bit56:2; bit78:2;
      nibble1:4; nibble2:4;'")
										;TODO: Also add a decoder to boolean
	  (make-parameter '|TO_BL_1| '|/U8-Type| :short-description "")
	  (make-parameter '|TO_BL_2| '|/U8-Type| :short-description "")
	  (make-parameter '|TO_B_1| '|/U8-Type| :short-description "") 
	  (make-parameter '|TO_B_2| '|/U8-Type| :short-description "") 
	  (make-parameter '|TO_B_3| '|/U8-Type| :short-description "") 
	  (make-parameter '|TO_B_4| '|/U8-Type| :short-description "") 
	  (make-parameter '|TO_W_1| '|/U16-Type| :short-description "") 
	  (make-parameter '|TO_W_2| '|/U16-Type| :short-description "") 
	  (make-parameter '|TO_DW_1| '|/U16-Type| :short-description "")
	  (make-parameter '|TO_DW_2| '|/U16-Type| :short-description "")
	  (make-parameter '|TO_F_1| '|/F32-Type| :short-description "") 
	  (make-parameter '|TO_F_2| '|/F32-Type| :short-description "") 
	  (make-parameter '|TO_DF_1| '|/F64-Type| :short-description "")
	  (make-parameter '|TO_DF_2| '|/F64-Type| :short-description "")
	  (make-parameter '|TO_STR| '|/ASCII-String-Type| :short-description ""))))))


(time (dump-space-system-xml (symbol-value '|NASA-cFS|)))


(defparameter *frame* #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

;; (process-fixed-frame-stream (make-fixed-frame-stream (make-sync-strategy) 1024) )


(defun monad (data-stream system-tree)
  
  )

(describe #'accept-frame)

(describe '|NASA-cFS|)

 ;TODO: There is an electronic data sheet that can be used to interpret the AMPCS xml
