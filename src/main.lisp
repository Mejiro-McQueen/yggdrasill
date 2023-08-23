1(ql:quickload "bifrost-yggdrasill")
(ql:quickload "uiop")
(ql:quickload "alexandria")
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
 "SpaceVechicle"
 :telemetry-metadata
 (make-telemetry-metadata
  :parameter-type-set
  (make-parameter-type-set
   (make-integer-parameter-type
    "IDType"
    :data-encoding
    (make-integer-data-encoding)
	:signed nil)
   (make-integer-parameter-type
	"SecHType"
	:signed nil
	:data-encoding
	(make-integer-data-encoding :size-in-bits 1))
   (make-integer-parameter-type
	"TypeType"
	:signed nil
	:data-encoding
	(make-integer-data-encoding :size-in-bits 1))
   (make-integer-parameter-type
	"LengthType"
	:signed nil
	:data-encoding
	(make-integer-data-encoding :size-in-bits 16))
   (make-enumerated-parameter-type
	"PSWHLTIMFLGType"
	:data-encoding
	(make-integer-data-encoding :size-in-bits 16)
	:enumeration-list
	(make-enumeration-list
	 (make-enumeration 'TIMER_OFF 0)
	 (make-enumeration 'TIMER_ON 1)
	 (make-enumeration 'TIMER_COMPLETED 2)))
   
   (make-float-parameter-type
	"PBATMTEMPType"
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
	"MissionTimeType"
	:reference-time
	(make-reference-time
	 (make-offset-from "Seconds")))
   
   (make-absolute-time-parameter
	"SecondsType"
	:encoding
	(make-encoding
	 :units "seconds"
	 :data-encoding
	 (make-integer-data-encoding :size-in-bits 32))
	:reference-time
	(make-reference-time
	 (make-offset-from "Milliseconds")))
   
   (make-absolute-time-parameter
	"MillisecondsType"
	:encoding
	(make-encoding
	 :units "seconds"
	 :scale 0.001
	 :data-encoding
	 (make-integer-data-encoding :size-in-bits 16))
	:reference-time
	(make-reference-time
	 (make-epoch 'TAI)))
   )
  :parameter-set
  (make-parameter-set
   (make-parameter "SecH" "SecHType")
   (make-parameter "Type" "TypeType")
   (make-parameter "ID" "IDType")
   (make-parameter "Length" "LengthType")
   (make-parameter "Seconds" "SecondsType")
   (make-parameter "Milliseconds" "MillisecondsType")
   (make-parameter "PBATMTEMP" "PBATMTEMPType")
   (make-parameter "PSWHLTIMFLG" "PSWHLTIMFLGType")
   (make-parameter "MissionTime" "MissionTimeType"
				   :parameter-properties
				   (make-parameter-properties :data-source "derived")))
  :container-set
  (make-container-set
   (make-sequence-container
	"Header"
	(make-entry-list
	 (make-parameter-ref-entry "ID")
	 (make-parameter-ref-entry "SecH")
	 (make-parameter-ref-entry "Type")
	 (make-parameter-ref-entry "Length")
	 (make-parameter-ref-entry "SecondaryHeader"
							   :include-condition
							   (make-include-condition (make-comparison "SecH" 1))))))))


(time (dump-space-system-xml (symbol-value 'SPACEVECHICLE)))


(defparameter *TEST* (make-container-set 
 (make-sequence-container
  "MyFormatHeader"
  (make-entry-list
   (make-parameter-ref-entry "Version")
   (make-parameter-ref-entry "Type")
   (make-parameter-ref-entry "ID")
   (make-parameter-ref-entry "Length"))
   :abstract t)
 
 (make-sequence-container
  "MyTimeStampHeader"
  (make-entry-list
   (make-parameter-ref-entry "TimeStamp"))
  :abstract t
  :base-container
  (make-base-container
   "MyFormatHeader"
   :restriction-criteria
   (make-restriction-criteria
	(make-comparison-list
	 (make-comparison "Version" 1)
	 (make-comparison "Type" 1)))))

 (make-sequence-container
  "MyConcretePacket"
  (make-entry-list
   (make-parameter-ref-entry "P1")
   (make-parameter-ref-entry "P2")
   (make-parameter-ref-entry "P3"))
  :base-container
  (make-base-container
   "MyTimeStampHeader"
   :restriction-criteria
   (make-restriction-criteria
	(make-comparison "ID" 100))))))


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

(defclass steam-set (xtce-set) ())


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

;Rule: references must use ./ or /, or neither for local.


(defun find-key-by-path (requested-key current-table root-table)
  (unless current-table
	#+ *debug-mode*
	(print "No Hash Table Found: Are your references broken?")
	(return-from find-key-by-path nil))
  
  (labels ((format-path (string-list)
			 (reduce (lambda (str1 str2)
					   (concatenate 'string str1 "/" (if (equalp str2 :BACK) "../" str2))) string-list :initial-value ".")))
	(multiple-value-bind (flag path-components file) (uiop::split-unix-namestring-directory-components requested-key)
	  (let* ((target (intern file))
			 (match-in-current-table? (gethash target current-table))
			 (parent-table (if current-table
							   (gethash (intern "./") current-table)))
			 (next-requested-key (format-path (append (cdr path-components) (list (format-symbol target))))))

		#+ *debug-mode*
		(progn
		  (print (format nil "~%"))
		  (print (format nil "Flag: ~A" flag))
		  (print (format nil "Target: ~A" target))
		  (print (format nil "Requested-Key: ~A" requested-key))
		  (print (format nil "Next-Key: ~A" next-requested-key))
		  (print (format nil "Target Components: ~A" path-components))
		  (print (format nil "Current Table Keys: ~A" (alexandria:hash-table-keys current-table))))
	
		(case flag
		  (:absolute
		   #+ *debug-mode*
		   (print 'ABSOLUTE)
		   ;Enforce Recursion
		   (return-from find-key-by-path (find-key-by-path next-requested-key root-table root-table)))

		  (:relative
		   (when (member :BACK path-components)
			 #+ *debug-mode*
			 (print 'Go-Back)
			 (return-from find-key-by-path (find-key-by-path next-requested-key parent-table root-table)))

		   (when path-components
			 #+ *debug-mode*
			 (print 'Keep-Looking)
			 (return-from find-key-by-path (find-key-by-path next-requested-key
															 (gethash (intern (car path-components)) current-table)
															 root-table)))

		   (when match-in-current-table?
			 #+ *debug-mode*
			 (print 'GET!)
			 (return-from find-key-by-path match-in-current-table?))

		   (unless match-in-current-table?
			 #+ *debug-mode*
			 (print 'Path-Exhausted-No-Match)
			 nil)
		   ))))))

(defun accept-frame (frame)
  frame
  )

(describe #'accept-frame)
	


(logtest #b1010101010101010 #b1010101010101010)

(logand #xFFFF #xFFFF)

(print-hex (ldb-left 4 4  #x5555555555555555))
(print-hex (ldb-left 4 0  #xAFFFFFFFFFFFFFFF))

(hex-length-in-bits #x5555)

(print-hex 0)

(hamming-distance #x029a #x029b)

(logcount 36)


(concatenate 'string "LOL" "LMAO")
