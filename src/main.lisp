;(ql:quickload "lparallel")
;(ql:quickload "filesystem-hash-table")
;(ql:quickload "log4cl")
;(ql:quickload :log4cl.log4sly)
;(log4cl.log4sly:install)
(ql:quickload '(clack websocket-driver))

(declaim (optimize (speed 0) (space 0) (debug 3)))
(defvar debug-mode t)
;(setf lparallel:*kernel* (lparallel:make-kernel 10))

(in-package :xtce-engine)
;; (defparameter *TEST* (make-enumerated-parameter-type
;;  '|STC:CCSDS:Sequence-Flags-Type|
;;  :enumeration-list (list
;; 					(make-enumeration #b00 'Continuation :short-description "Space Packet contains a continuation segment of User Data.")
;; 					(make-enumeration #b01 'First-Segment :short-description "Space Packet contains the first segment of User Data.")
;; 					(make-enumeration #b10 'Last-Segment :short-description "Space Packet contains the last segment of User Data.")
;; 					(make-enumeration #b11 'Unsegmented :short-description "Space Packet is unsegmented."))))

; Generate and store speculative container match
; When container is called again, check for speculative match, then check against restriction criteria.
; When the full container match occurs, you win




;; (defun a (space-system frame)
;;   (with-state space-system
;; 	(multiple-value-bind (frame state next-continuation) (funcall frame-stream-processor-continuation frame)
;; 	  (incf frame-counter)
;; 	  (setf frame-stream-processor-continuation next-continuation)
;; 	  ;; (print frame-counter)
;; 	  ;; (print frame)
;; 	  ;; (print state)
;; 	  ;; (print next-ref)
;; 										;(print next)
;; 										;(print (type-of next))
;; 	  (decode frame next symbol-table '() 0)
;; 	  )
;; 	))

										;TODO: Typecheck when dereferencing


;; I think you only need 2 threads: 1 for uplink and 1 for downlink, I think the overhead from multiple threads would exceed just finishing off the computation

;; (defparameter frame-queue (lparallel.queue:make-queue))
;; (defparameter test (bt:make-thread (lambda () (monad nasa-cfs::NASA-cFS frame-queue)) :name "monad"))

;; (lparallel.queue:push-queue qq frame-queue)
;; (lparallel.queue:push-queue nil frame-queue)



;; (print-hex (second (process-fixed-frame 0 1 'SEARCH (make-sync-strategy) (make-sync-pattern) #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))

;; (lambda (frame aperture) (process-fixed-frame 0 0 'SEARCH (make-sync-strategy) (make-sync-pattern) frame :aperture aperture))


;; (time (process-fixed-frame-stream (make-fixed-frame-stream (make-sync-strategy) 1024) qq))



;;; Services
;;; A service is identified exclusively by its name

(defparameter *service-threads* (make-hash-table))

(defun generate-services (service-list symbol-table &optional (service-table (make-hash-table)))
  "Generate all services"
  (dolist (service service-list)
	(case (name service)
	  ('|STC.CCSDS.MPDU.Container.MPDU|
	   (setf (gethash service service-table) (generate-mpdu-decoding-service service symbol-table)))
	  (t
	   (error (format nil "No service generator found for ~A" service) ))))
  service-table)

(defun generate-mpdu-decoding-service (service symbol-table)
  (with-slots (name reference-set short-description ancillary-data-set) service
	(let* ((reference-container (first reference-set))
		   (reference (dereference reference-container symbol-table))
		   (vcid (xtce::value (gethash 'VCID (xtce::items ancillary-data-set)))))
	  (assert reference-container)
	  (assert vcid)
	  (assert (equal (name reference) stc::'|STC.CCSDS.MPDU.Container.MPDU|) (reference) "Could not find a service for reference ~A" reference)
	  (log:info "Generated MPDU Service for VCID ~A!" vcid)
	  (list #'decode-mpdu service))))

(defparameter test-service-list
  (list (make-service '|STC.CCSDS.MPDU.Container.MPDU|
					  (list (make-container-ref '|STC.CCSDS.MPDU.Container.MPDU|))
					  :short-description "Test MPDU Service"
					  :ancillary-data-set
					  (xtce::make-ancillary-data-set
					   (xtce::make-ancillary-data 'VCID 43)))))

(with-test-table
  (defparameter test-table test-table)
  )

(generate-services test-service-list test-table)

;;; Service Loops
;;; Targets for threads to execute
(defun deframing-service-loop ())

(defun frame-decoding-service-loop (service-list symbol-table service-queue output-queue)
  (log:info "UP")
  (let ((services (generate-services service-list symbol-table)))
	(loop
	  (let* ((frame-alist (pop-queue service-queue))
			 (vcid (cdr (assoc stc::'|STC.CCSDS.AOS.Header.Virtual-Channel-ID| frame-alist)))
			 (monad-pair (gethash vcid services))
			 (monad (first monad-pair))
			 (service (second monad-pair)))
		(multiple-value-bind (packet-list next-monad) (funcall monad frame-alist symbol-table)
		  (log:info packet-list)
		  (setf (gethash vcid services) (list next-monad service))
		  (push-queue packet-list output-queue)
		  (log:info vcid)
		  )))))

;;; Startup

(defun create-frame-listener (env)
  (let ((ws (websocket-driver:make-server env)))
	(websocket-driver:on ws :open
						 (lambda () (handle-new-listener ws)))

	(websocket-driver:on :message ws
						 (lambda (message)
						   (handle-message ws)))

	(websocket-driver:on :close ws
						 (lambda (&key code reason)
						   (handle-error ws code reason)))))

(defun handle-error (ws code reason))

(defun handle-new-listener (ws))

(defun handle-close (ws))


(defun start-command-processors (space-system))

(defun start-telemetry-processors (space-system))

(defparameter *frame-listeners* (make-hash-table))

(defun start-frame-listeners (stream-list)
  (check-type stream-list xtce::stream-set)
  (dolist (stream stream-list)
	(with-slots (ancillary-data-set) stream
	  (let* ((port (xtce::value (gethash :port (xtce::items ancillary-data-set))))
			 (listener (progn
						 (assert port (port) "No key :port found for ~A in ancillary data" stream)
						 (clack:clackup #'create-frame-listener :port port))))
		(log:info "Started frame listener on port ~A" port)
		(setf (gethash stream *frame-listeners*) listener)))))

(defun stop-frame-listeners ()
  (maphash #'stop-frame-listener
		   *frame-listeners*))

(defun stop-frame-listener (stream listener)
  (clack:stop listener)
  (log:info "Shutdown frame listener ~A" stream))

(defun bifrost.start (space-system)
  (start-telemetry-processors)
  (start-command-processors)
  )


(bifrost.start nasa-cfs::NASA-cFS)


(start-frame-listeners
 (stc:with-ccsds.aos.stream 1024 8081))

(stop-frame-listeners)



(with-test-table
  (log:info "Going up!")
  (bt:make-thread (vcid-services-start q test-table service-queue output-queue) :name "Test1")
										;(log:info "Down")
  )



;; (defparameter q
;;   (with-test-table
;; 	(list (make-service "STC.CCSDS.MPDU.Container.MPDU"
;; 						(list (make-container-ref '|STC.CCSDS.MPDU.Container.MPDU|))
;; 						:short-description "Test MPDU Service"
;; 						:ancillary-data-set (xtce::make-ancillary-data-set (xtce::make-ancillary-data 'VCID 43))))
;; 	))


;; (with-test-table
;;   (generate-services (list (make-service "STC.CCSDS.MPDU.Container.MPDU"
;; 										 (list (make-container-ref '|STC.CCSDS.MPDU.Container.MPDU|))
;; 										 :short-description "Test MPDU Service"
;; 										 :ancillary-data-set (xtce::make-ancillary-data-set (make-ancillary-data 'VCID 0))))
;; 					 test-table))
