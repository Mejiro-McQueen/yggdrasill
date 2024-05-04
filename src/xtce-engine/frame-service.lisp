;Fixed frames do not span, immediately move to next level
;Variable sized frames may span, need to move to accumulator (e.g. simulators)
(in-package :xtce-engine)

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
		(log:debug "Check-Counter: ~A, Verify Counter:~A, State: ~A" state-check-counter verify-counter state-symbol)
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
		
		(values state-result sync-result (lambda (frame aperture)
										   (process-fixed-frame
											state-check-counter
											verify-counter
											state-result
											sync-strategy
											frame
											:aperture aperture)))))))

(defun process-fixed-frame-stream
	(fixed-frame-stream-type
	 &key (fixed-frame-processor-continuation (lambda (frame aperture) (process-fixed-frame 0 0 'SEARCH (sync-strategy fixed-frame-stream-type) frame :aperture aperture)))
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
		(log:debug "State: ~A" state)
		(log:debug "Aperture: ~A" aperture)
		(log:debug "Frame: ~A" (print-hex frame))
		(unless aperture
		  (log:warn "Aperture greater than zero: ~A" aperture))
		(unless frame
		  (log:info "Could not synchronize frame"))
		(return-from process-fixed-frame-stream (values frame state (lambda (frame) (process-fixed-frame-stream
																				fixed-frame-stream-type
																				:fixed-frame-processor-continuation next-continuation
																				:frame frame))))))))
(defun process-frame-result (frame state next-ref symbol-table)
  ;(log:error symbol-table)
  ;(log:error next-ref)
  (case state
	(LOCK
	 ;(log:info "Frame Locked and Accepted")
	 ;(log:info "Next stage: ~A" next-ref)
	 (decode (uint->bit-vector frame) next-ref symbol-table '() 0)
	 )
	
	(VERIFY
	 (log:info "Frame discarded due to VERIFY state."))
	
	(SEARCH
	 (log:warn "Could not find synchronization marker!"))
	
	(CHECK
	 (log:info "Frame discarded due to CHECK state."))))

(defgeneric frame-sync (frame stream-type symbol-table &key this-continuation frame-counter))
(defmethod frame-sync (frame (stream-type fixed-frame-stream) symbol-table
				   &key
					 (this-continuation (lambda (frame) (process-fixed-frame-stream stream-type :frame frame)))
					 (frame-counter 0))
  ;(log:info "Starting Frame Sync Service for ~A" stream-type)
  (multiple-value-bind (frame-result state next-continuation) (funcall this-continuation frame)
	(incf frame-counter)
	;(log:info "Total Frames Synchronized: ~A" frame-counter)
	;(log:info "Current Synchronization State: ~A" state)
	(let ((res (process-frame-result frame state (xtce::ref stream-type) symbol-table)))
	  (values res state (lambda (next-frame stream-type symbol-table) (frame-sync
																  next-frame
																  stream-type
																  symbol-table
																  :this-continuation next-continuation
																  :frame-counter frame-counter))))))
;; (defmethod frame-sync (frame (stream-type variable-frame-stream)
;; 				   &key
;; 					 (this-continuation (get-frame-processor-function stream-type))
;; 					 (frame-counter 0))
;;   (log:error "Not implemented Frame Sync Service for ~A" stream-type))


;; (defmethod frame-sync (frame (stream-type custom-stream)
;; 				   &key
;; 					 (this-continuation (get-frame-processor-function stream-type))
;; 					 (frame-counter 0))
;;   (log:error "Not implemented Frame Sync Service for ~A" stream-type))


;; (frame-sync 18828336199429801156648552013508144182012514821973559160702289297511120971662290594260501819590494654490051816847667710640417415297314085581983899864651526208966978638647803315176772478555677603831721718618445925971784302694756508652907583850879874593132025327973325395061214364289979416854521388303185346559
;;  			(make-fixed-frame-stream 'lol 1024 (make-container-ref 'lol) (make-sync-strategy (make-sync-pattern))) (make-hash-table))

;; (decode (uint->bit-vector #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF) (make-container-ref '|STC.CCSDS.AOS.Container.Transfer-Frame-Primary-Header|) test-1 '() 0)


;; (defparameter test-1 (xtce::register-keys-in-sequence
;; 					  (funcall 
;; 					   (alexandria:compose #'stc::with-ccsds.space-packet.parameters
;; 										   #'stc::with-ccsds.space-packet.types
;; 										   #'stc::with-ccsds.space-packet.containers
;; 										   #'stc::with-ccsds.mpdu.containers
;; 										   #'stc::with-ccsds.mpdu.types
;; 										   #'stc::with-ccsds.mpdu.parameters
;; 										   #'stc::with-ccsds.aos.containers
;; 										   #'stc::with-ccsds.aos.header.parameters
;; 										   #'stc::with-ccsds.aos.header.types) nil)
;; 					  (filesystem-hash-table:make-filesystem-hash-table)
;; 					  'Test))



;; (dereference (make-container-ref "CCSDS.AOS.Container.Frame") test-1)


;; (filesystem-hash-table:find-key-by-path "STC.CCSDS.AOS.Container.Frame" test-1)

