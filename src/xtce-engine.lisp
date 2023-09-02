(in-package :xtce-engine)
(use-package :xtce)

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
		 ;(accept-frame frame)
		 (emit! state))
		  
		(VERIFY
		 (emit! state)
		 )
		  
		(SEARCH
		 (emit! (list "Could not find synchronization marker!")))
		  
		(CHECK))
		
	  (return-from process-fixed-frame-stream (list state next-continuation)))))
