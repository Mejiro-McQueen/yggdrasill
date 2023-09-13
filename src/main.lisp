(ql:quickload "bifrost-yggdrasill")
(ql:quickload "lparallel")

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
		
		(return-from process-fixed-frame-stream (values frame state (lambda (frame) (process-fixed-frame-stream fixed-frame-stream-type next-continuation frame))))))))

(defun get-frame-processor (stream-type)
  (typecase stream-type
	(fixed-frame-stream
	 'process-fixed-frame-stream)
	(variable-frame-stream)
	(custom-stream)))

(defparameter qq #x1acffc1eFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

(setf qq #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

(defun monad (space-system frame-queue)
  (let* ((telemetry-metadata (slot-value space-system 'telemetry-metadata))
		 (stream-type (when telemetry-metadata (first (slot-value telemetry-metadata 'stream-set))))
		 (frame-stream-processor-continuation (get-fixed-frame-stream-initial-state stream-type))
		 (frame-counter 0))

	(loop
	  for frame = (lparallel.queue:pop-queue frame-queue)
	  when (null frame)
		return :exit
	  do
		 (print "Got Frame")
		 (multiple-value-bind (frame state next-continuation) (funcall frame-stream-processor-continuation frame)
		   (incf frame-counter)
		   (setf frame-stream-processor-continuation next-continuation)
		   (print frame-counter)
		   (print frame)
		   (print state))
	)))


(defparameter frame-queue (lparallel.queue:make-queue))
(defparameter test (bt:make-thread (lambda () (monad nasa-cfs::NASA-cFS frame-queue)) :name "monad"))

(lparallel.queue:push-queue qq frame-queue)
(lparallel.queue:push-queue nil frame-queue)


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

