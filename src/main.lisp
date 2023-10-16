(ql:quickload "bifrost-yggdrasill")
(ql:quickload "lparallel")
(ql:quickload "filesystem-hash-table")

(declaim (optimize (speed 0) (space 0) (debug 3)))
(defvar debug-mode t)

(in-package :xtce-engine)
(defparameter *TEST* (make-enumerated-parameter-type
 '|STC:CCSDS:Sequence-Flags-Type|
 :enumeration-list (list
					(make-enumeration #b00 'Continuation :short-description "Space Packet contains a continuation segment of User Data.")
					(make-enumeration #b01 'First-Segment :short-description "Space Packet contains the first segment of User Data.")
					(make-enumeration #b10 'Last-Segment :short-description "Space Packet contains the last segment of User Data.")
					(make-enumeration #b11 'Unsegmented :short-description "Space Packet is unsegmented."))))

; Generate and store speculative container match
; When container is called again, check for speculative match, then check against restriction criteria.
; When the full container match occurs, you win

;Fixed frames do not span, immediately move to next level
;Variable sized frames may span, need to move to accumulator (e.g. simulators)

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

;(find-sync-pattern  #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF (make-sync-pattern))

(defun process-fixed-frame (state-check-counter verify-counter state-symbol sync-strategy frame &key (aperture 0))
  (with-slots (xtce::sync-pattern verify-to-lock-good-frames check-to-lock-good-frames max-bit-errors-in-sync-pattern) sync-strategy
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
			 (if (> state-check-counter check-to-lock-good-frames)
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
			 (incf verify-counter)
			 (if (> verify-counter verify-to-lock-good-frames)
				 (progn
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

		;; (print state-result)
		;; (print state-check-counter)
		;; (print verify-counter)
		
		(values state-result sync-result (lambda (frame aperture)
										   (process-fixed-frame
											state-check-counter
											verify-counter
											state-result
											sync-strategy
											frame
											:aperture aperture)))))))

;TODO: Check counters
(process-fixed-frame 0 0 'VERIFY (make-sync-strategy (make-sync-pattern)) #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

(defun emit! (message)
  (format t "~A" message))

(defgeneric get-frame-processor-initial-state (frame-type))

(defmethod get-frame-processor-initial-state ((frame-type fixed-frame-stream))
  (with-slots (sync-strategy) frame-type
	(lambda (frame aperture) (process-fixed-frame 0 0 'SEARCH sync-strategy frame :aperture aperture))))

(defmethod get-frame-processor-initial-state ((frame-type variable-frame-stream)))

;(defmethod get-frame-processor-initial-state ((frame-type custom-frame-stream)))

(defun get-fixed-frame-stream-initial-state (fixed-frame-stream-type)
  (let ((fixed-frame-processor-continuation (get-frame-processor-initial-state fixed-frame-stream-type)))
	(lambda (frame) (process-fixed-frame-stream fixed-frame-stream-type fixed-frame-processor-continuation frame))))

(defun process-fixed-frame-stream
	(fixed-frame-stream-type
	 fixed-frame-processor-continuation
	 frame)

  (labels ((aperture-values (n)
			 (append '(0)
					 (alexandria:iota n :start 1)
					 (alexandria:iota n :start -1 :step -1)))

		   (find-marker-with-aperture (aperture)
			 (loop for aperture in (aperture-values aperture)
				   for res = (multiple-value-list (funcall fixed-frame-processor-continuation frame aperture))
				   when (second res)
					 return (cons aperture res) ; Exit early
				   finally (return (cons aperture res))))) ; Giving up

	(with-slots (sync-aperture-in-bits frame-length-in-bits sync-strategy) fixed-frame-stream-type
	  (destructuring-bind (aperture state frame next-continuation) (find-marker-with-aperture sync-aperture-in-bits)
		;;(print state)
		;; (print aperture)
		;; (print (print-hex frame))
		(unless aperture
		  (emit! (list "Aperture greater than zero:" aperture)))		
		(return-from process-fixed-frame-stream (values frame state (lambda (frame) (process-fixed-frame-stream fixed-frame-stream-type next-continuation frame))))))))

(defun get-frame-processor (stream-type)
  (typecase stream-type
	(fixed-frame-stream
	 'process-fixed-frame-stream)
	(variable-frame-stream)
	(custom-stream)))

(defparameter qq #x1acffc1eFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

(setf qq #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

(defun process-frame-result (frame state next-ref symbol-table)
  (declare (ignore frame)
		   (ignore next-ref)
		   (ignore symbol-table)
		   )
  (case state
	(LOCK
										;(accept-frame frame)
	 (emit! state))
	
	(VERIFY
	 (emit! state)
	 )
	
	(SEARCH
	 (emit! (list "Could not find synchronization marker!")))
	
	(CHECK))
  )

;; (defun monad (space-system frame-queue)
;;   (with-state
;; 	(loop
;; 	  for frame = (lparallel.queue:pop-queue frame-queue)
;; 	  when (null frame)
;; 		return :exit
;; 	  do
;; 		 (print "Got Frame")
;; 		 (multiple-value-bind (frame state next-continuation) (funcall frame-stream-processor-continuation frame)
;; 		   (incf frame-counter)
;; 		   (setf frame-stream-processor-continuation next-continuation)
;; 		   (print frame-counter)
;; 		   (print frame)
;; 		   (print state)
;; 		   (print (slot-value stream-type 'next-ref))
;; 		   )
;; 	)))

(defmacro with-state (space-system &body body)
  `(let* ((telemetry-metadata (slot-value ,space-system 'telemetry-metadata))
		  (stream-type (when telemetry-metadata (first (slot-value telemetry-metadata 'stream-set))))
		  (frame-stream-processor-continuation (get-fixed-frame-stream-initial-state stream-type))
		  (symbol-table (slot-value space-system 'symbol-table))
		  (next-ref (slot-value stream-type 'next-ref))
		  (next (dereference next-ref symbol-table))
		  (frame-counter 0))
	 ,@body
	 ))

;;;Decode returns from encodings should not modify the alist, this may change in the future when we also want to record precalibrated values
;; Parameters must call decode on the encoding, calibrate the value, place it into the alist, and multiple value return

(defgeneric decode (data decodable-object symbol-table alist bit-offset))

;Dispatch on container-ref
(defmethod decode (data (container-ref-entry xtce::container-ref-entry) symbol-table alist bit-offset)
  (let* ((dereferenced-container (xtce:dereference container-ref-entry symbol-table)))
	(multiple-value-bind (next-bit-offset res) (decode data dereferenced-container symbol-table alist bit-offset)
	(values next-bit-offset res))))

;; Dispatch on container
(defmethod decode (data (container xtce::sequence-container) symbol-table alist bit-offset)
  (with-slots (name) container
	(let ((res-list '()))
	  (dolist (ref (entry-list container))
		(let ((dereference (xtce::dereference ref symbol-table)))
		  (assert dereference)
		  (multiple-value-bind (next-bit-offset res) (decode data dereference symbol-table alist bit-offset)
			(assert next-bit-offset () "REEEE ~A" dereference)
			;(print res)
			(push res res-list)
			(setf bit-offset next-bit-offset))))
	  (values bit-offset (list (cons (symbol-name name) res-list))))))

;; Dispatch on Parameter
(defmethod decode (data (parameter xtce::parameter) symbol-table alist bit-offset)
  (With-slots (name) parameter
  (let ((parameter-type (xtce::dereference parameter symbol-table)))
	(multiple-value-bind (next-bit-offset res) (decode data parameter-type symbol-table alist bit-offset)
	  (values next-bit-offset (list (cons (symbol-name name) res)))))))

;; Dispatch on binary-parameter-type
(defmethod decode (data (parameter-type xtce::binary-parameter-type) symbol-table alist bit-offset)
  (with-slots (name) parameter-type
	(let ((data-encoding (xtce:data-encoding parameter-type)))
	  (unless data-encoding 
		(error "Can not decode data from stream without a data-encoding for ~A" parameter-type))
	  (multiple-value-bind (bit-offset res) (decode data data-encoding symbol-table alist bit-offset)
		;(setf res (bit-vector->hex res))
		(values bit-offset res)))))

;;Decode binary-data encoding
(defmethod decode (data (encoding xtce::binary-data-encoding) symbol-table alist bit-offset)
  (with-slots (xtce::size-in-bits) encoding
	(let* ((size-in-bits (xtce::get-size xtce::size-in-bits))
		   (data-segment (subseq data bit-offset (+ bit-offset size-in-bits))))
	  (values (+ bit-offset size-in-bits) data-segment))))

;Deccode enumerated-parameter
;; (defmethod decode (data (parameter-type xtce::enumerated-parameter-type) symbol-table alist bit-offset)
  
;;   )

;;Decode boolean parameter 
(defmethod decode (data (parameter-type xtce::boolean-parameter-type) symbol-table alist bit-offset)
  (with-slots (name) parameter-type
	(let* ((data-encoding (xtce:data-encoding parameter-type))
		   (res nil))
	  (unless data-encoding ;Empty data-encoding is only valid for ground derrived telemetry 			
		(error "Can not decode data from stream without a data-encoding for ~A" parameter-type))
	  (multiple-value-bind (bit-offset decoded-flag) (decode data data-encoding symbol-table alist bit-offset)
		(with-slots (xtce::zero-string-value xtce::one-string-value xtce::name) parameter-type
		  (setf res (typecase decoded-flag
					  (bit-vector
					   (if (equal decoded-flag #*0)
						   xtce::zero-string-value
						   xtce::one-string-value))
					  (number
					   (if (equalp decoded-flag 0)
						   xtce::zero-string-value
						   xtce::one-string-value))
					  (string
					   (if (member decoded-flag '("F" "False" "Null" "No" "None" "Nil" "0" "") :test 'equalp)
						   xtce::zero-string-value
						   xtce::one-string-value)))))
		(values bit-offset res)))))

;;Decode Integer Parameter
(defmethod decode (data (parameter-type xtce::integer-parameter-type) symbol-table alist bit-offset)
  (let ((data-encoding (xtce:data-encoding parameter-type)))
	(unless data-encoding ;Empty data-encoding is only valid for ground derrived telemetry 			
	  (error "Can not decode data from stream without a data-encoding for ~A" parameter-type))
	(multiple-value-bind (bit-offset res) (decode data data-encoding symbol-table alist bit-offset)
	  (with-slots (xtce::name) parameter-type
		(values bit-offset res)))))

;;Decode Integer Encoding
(defmethod decode (data (integer-data-encoding xtce::integer-data-encoding) symbol-table alist bit-offset)
  (with-slots (xtce::integer-encoding xtce::size-in-bits) integer-data-encoding
	(let ((res nil)
		  (next-bit-offset (+ bit-offset xtce::size-in-bits)))
	  (case xtce::integer-encoding
		(xtce::'unsigned
		 (setf res (bit-vector->uint (subseq data bit-offset next-bit-offset)))))
	  (values next-bit-offset res))))

(defun a (space-system frame)
  (with-state space-system
	(multiple-value-bind (frame state next-continuation) (funcall frame-stream-processor-continuation frame)
	  (incf frame-counter)
	  (setf frame-stream-processor-continuation next-continuation)
	  ;; (print frame-counter)
	  ;; (print frame)
	  ;; (print state)
	  ;; (print next-ref)
										;(print next)
										;(print (type-of next))
	  (decode frame next symbol-table '() 0)
	  )
	))

										;TODO: Typecheck when dereferencing


;; I think you only need 2 threads: 1 for uplink and 1 for downlink, I think the overhead from multiple threads would exceed just finishing off the computation

;; (defparameter frame-queue (lparallel.queue:make-queue))
;; (defparameter test (bt:make-thread (lambda () (monad nasa-cfs::NASA-cFS frame-queue)) :name "monad"))

;; (lparallel.queue:push-queue qq frame-queue)
;; (lparallel.queue:push-queue nil frame-queue)



;; (print-hex (second (process-fixed-frame 0 1 'SEARCH (make-sync-strategy) (make-sync-pattern) #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))

;; (lambda (frame aperture) (process-fixed-frame 0 0 'SEARCH (make-sync-strategy) (make-sync-pattern) frame :aperture aperture))


;; (time (process-fixed-frame-stream (make-fixed-frame-stream (make-sync-strategy) 1024) qq))


;; (ql:quickload "lparallel")
;; (defpackage :queue-example (:use :cl ))
;; (in-package :queue-example)

;; (setf lparallel:*kernel* (lparallel:make-kernel 10))

;; (let ((queue (make-queue))
;;       (channel (make-channel)))
;;   (submit-task channel (lambda () (list (pop-queue queue)
;;                                    (pop-queue queue))))
;;   (push-queue "hello" queue)
;;   (push-queue "world" queue)
;;   (receive-result channel))

;; (defun forever (queue channel)
;;   (loop
;; 	(let ((x (pop-queue queue)))
;; 	  (print x))))

;; (defparameter q (make-queue))
;; (defparameter c (make-channel))

;; (defparameter test (bt:make-thread (lambda () (forever q c))))


;; (push-queue "hello" q)


;; (defun print-message-top-level-fixed ()
;;   (let ((top-level *standard-output*))
;;     (bt:make-thread
;;      (lambda ()
;; 	   (loop
;; 		 (format top-level "Hello from thread!")
;; 		 (sleep 1)
;; 		 ))
;;      :name "hello"))
;;   nil)

;; (print-message-top-level-fixed)

;; (defparameter *counter* 0)

;; (defun test-update-global-variable ()
;;   (bt:make-thread
;;    (lambda ()
;;      (sleep 1)
;;      (incf *counter*)))
;;   *counter*)


(defun bit-array-list->concat-bit-vector (l)
  (apply #'concatenate-bit-arrays l))

(defun alist->bit-vector (l)
  (bit-array-list->concat-bit-vector (mapcar #'cdr l)))

(defun new-bit-vector () (make-array 1 :element-type 'bit :adjustable t :fill-pointer 0))

(defun concatenate-bit-arrays (&rest rest)
  (apply #'concatenate 'bit-vector rest))

(defun invert (bit)
  (declare (type bit bit))
  (logxor bit 1))

(defun bit-vector->twos-complement->integer (v)
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((neg (equal 1 (bit v 0)))
		(res nil))
	(if neg
		(setf res (map 'string #'digit-char (bit-not v)))
		(setf res (map 'string #'(lambda (bit) (digit-char bit)) v)))

	(setf res (parse-integer res :radix 2))
	;;(setf res (format nil "~b" res))
	(when neg
	  (setf res (- (+ res #b1))))
	res
	))

(defun uint->bit-vector (n &optional (pad (integer-length n))) 
  "~43 HAYAI!"
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((pos (- pad 1))
		(res (make-sequence 'bit-vector pad :initial-element 0)))
	(loop while (> n 0)
		  do
			 (setf (aref res pos) (logand n 1))
			 (setf n (ash n -1))
			 (setf pos (- pos 1)))
	res))

(defun bit-vector->twos-complement->dec (v)
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((neg (equal 1 (bit v 0)))
		(res nil))
	(if neg
		(setf res (map 'string #'digit-char (bit-not v)))
		(setf res (map 'string #'(lambda (bit) (digit-char bit)) v)))
	(setf res (parse-integer res :radix 2))
	(when neg
	  (setf res (- (+ res #b1))))
	res))

(defun bit-vector->ones-complement->dec (v)
  ;;(declare (optimize (speed 3) (safety 0)))
  (let ((neg (equal 1 (bit v 0)))
		(res nil))
	(if neg
		(setf res (map 'string #'digit-char (bit-not v)))
		(setf res (map 'string #'(lambda (bit) (digit-char bit)) v)))
	(setf res (parse-integer res :radix 2))
	(when neg
	  (setf res (- res)))
	res))

(defun bit-vector->uint (v)
  (parse-integer (map 'string #'digit-char v) :radix 2))

(defun bit-vector->hex (v)
  (let* ((uint (bit-vector->uint v))
		 (hex (format nil "0x~X" uint)))
	hex))

(defun twos-complement-representable-p (n bits)
  (let ((max (expt 2 (- bits 1))))
	(and (< n max) (>= n (- max)))))

(defun dec->twos-complement (integer pad)
  ;;(declare (optimize (speed 3) (safety 0)))
  (assert (twos-complement-representable-p integer pad) (integer pad) "Insufficient bits to represent this integer.")
  (if (< integer 0)
	  (uint->bit-vector (+ #b1 (bit-vector->uint (bit-not (uint->bit-vector (abs integer) pad)))) pad)
	  (pad-bit-vector (uint->bit-vector integer) pad)))

(defun dec->ones-complement (integer pad)
  (if (< integer 0)
	  (bit-not (uint->bit-vector (abs integer) pad)
			   (uint->bit-vector integer pad))))

(defun pad-bit-vector (v pad &optional (pad-element 0))
  ;;(declare (optimize (speed 3) (safety 0)))
  (if (< (length v) pad)
	  (concatenate-bit-arrays (make-sequence 'bit-vector (- pad (length v)) :initial-element pad-element) v)
	  v))

(defun bit-vector->sign-mag->dec (v)
  (let* ((sign (bit v 0))
		 (neg (equal 1 sign))
		 (res nil))
	(setf (bit v 0) 0)
	(setf res (bit-vector->uint v))
	(if neg
		(- res)
		res)))

(defun dec->sign-mag (n pad)
  (let ((res (uint->bit-vector (abs n) pad)))
	(when (< n 0)
	  (setf (bit res 0) 1))
	res
	))

(defvar packedBCD-Table
  '((1 . #*0000)
	(2 . #*0001)
	(3 . #*0010)
	(4 . #*0100)
	(5 . #*0101)
	(6 . #*0110)
	(7 . #*0111)
	(8 . #*1000)
	(9 . #*1001)))

(defun bcd->dec (v byte-size)
  (loop for i from 0 to (length v) by byte-size
		while (< i (- (length v) (- byte-size 1)))
		collect (first(rassoc (subseq v i (+ i byte-size)) packedBCD-Table :test 'equal)) into res
		finally (return (mapcar #'identity res))))

(defun digit-list->integer (l)
  (reduce #'(lambda (acc digit) (+ (* acc 10) digit)) l))

(defun dec->bcd (n &optional (pad 4))
  (let ((digit-list (integer->digit-list n)) )
	(apply 'concatenate-bit-arrays (mapcar #'(lambda (digit) (pad-bit-vector (cdr (assoc digit packedBCD-Table :test 'equal)) pad)) digit-list))))

(defun integer->digit-list (n)
  (loop while (> n 1)
		collect (rem n 10 )
		do
		   (setf n (floor (/ n 10)))))

;;;;;;;;;;
(defparameter AOS-TEST-HEADER (list (cons 'transfer-frame-version-number #*01)
									(cons 'spacecraft-id #*01100011) ;0x63
									(cons 'virtual-channel-id #*101011) ;43
									(cons 'virtual-channel-frame-count #*100101110000100010101011); 9898155
									(cons 'replay-flag #*0)
									(cons 'virtual-channel-frame-count-usage-flag #*1)
									(cons 'reserved-space #*00)
									(cons 'vc-frame-count-cycle #*1010)
									(cons 'data-field (pad-bit-vector #* 976 1))
									))

(defparameter AOS-TEST-HEADER-BIN (alist->bit-vector AOS-TEST-HEADER))

(defparameter TEST-TABLE (xtce::register-keys-in-sequence
						  (stc::with-ccsds.mpdu.containers
							  (stc::with-ccsds.mpdu.types
							  (stc::with-ccsds.mpdu.parameters
								  (stc::with-ccsds.aos.containers
									  (stc::with-ccsds.aos.header.parameters
										  (stc::with-ccsds.aos.header.types '()))))))
							(filesystem-hash-table:make-filesystem-hash-table) 'Test))


(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number-Type" TEST-TABLE) TEST-TABLE '() 0)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Spacecraft-Identifier-Type" TEST-TABLE) TEST-TABLE '() 2)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-ID-Type" TEST-TABLE) TEST-TABLE '() 10)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Type" TEST-TABLE) TEST-TABLE '() 16)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Replay-Flag-Type" TEST-TABLE) TEST-TABLE '() 40)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag-Type" TEST-TABLE) TEST-TABLE '() 41)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Reserved-Spare-Type" TEST-TABLE) TEST-TABLE '() 42)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle-Type" TEST-TABLE) TEST-TABLE '() 44)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Transfer-Frame-Data-Field-Type" TEST-TABLE) TEST-TABLE '() 44)

(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Transfer-Frame-Version-Number" TEST-TABLE) TEST-TABLE '() 0)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Spacecraft-Identifier" TEST-TABLE) TEST-TABLE '() 2)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-ID" TEST-TABLE) TEST-TABLE '() 10)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count" TEST-TABLE) TEST-TABLE '() 16)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Replay-Flag" TEST-TABLE) TEST-TABLE '() 40)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Usage-Flag" TEST-TABLE) TEST-TABLE '() 41)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Reserved-Spare" TEST-TABLE) TEST-TABLE '() 42)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Header.Virtual-Channel-Frame-Count-Cycle" TEST-TABLE) TEST-TABLE '() 44)
(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Transfer-Frame-Data-Field" TEST-TABLE) TEST-TABLE '() 44)




(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Container.Transfer-Frame-Primary-Header" TEST-TABLE) TEST-TABLE '() 0)

(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Container.Frame" TEST-TABLE) TEST-TABLE '() 0)



(defun mpdu-depacketizer (alist symbol-table)
  (let* ((frame (second (assoc "STC.CCSDS.AOS.Container.Frame" alist :test 'equalp)))
		 (frame-data-field (second (assoc "STC.CCSDS.AOS.Container.Transfer-Frame-Data-Field" frame :test 'equalp)))
		 (frame-data (cdr (assoc "STC.CCSDS.AOS.Transfer-Frame-Data-Field" frame-data-field :test 'equalp)))
		 )
	frame-data
	)
  )

(multiple-value-bind (_ alist)
	(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Container.Frame" TEST-TABLE) TEST-TABLE '() 0)
  (defparameter decoded-frame alist)
  (mpdu-depacketizer decoded-frame TEST-TABLE))


;; (assoc "STC.CCSDS.AOS.Container.Frame" decoded-frame :test 'equalp)

;; decoded-frame


;; (let* ((a (pairlis '(a b) '(c d)))
;; 	   (z (pairlis '(e) (list a))))
;;   (second (assoc 'e z))
;;   )


(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Container.Transfer-Frame-Primary-Header.Master-Channel-ID" TEST-TABLE)
		TEST-TABLE '() 0)

(pairlis '(a b) '(c d))

(decode AOS-TEST-HEADER-BIN (gethash "STC.CCSDS.AOS.Container.Frame" TEST-TABLE) TEST-TABLE '() 0)
