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



;; (print-hex (second (process-fixed-frame 0 1 'SEARCH (make-sync-strategy) (make-sync-pattern) #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)))

;; (lambda (frame aperture) (process-fixed-frame 0 0 'SEARCH (make-sync-strategy) (make-sync-pattern) frame :aperture aperture))


;; (time (process-fixed-frame-stream (make-fixed-frame-stream (make-sync-strategy) 1024) qq))


;; (defparameter qq #x1acffc1eFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)

;; (setf qq #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF)


;;; Streams

(defparameter *port->state* (make-hash-table))
(defstruct stream-state sync-closure container-closure stream-def server)

(defgeneric initialize-stream-state (port xtce-stream server))

(defmethod initialize-stream-state ((port integer) (stream fixed-frame-stream) (server t))
  (with-slots (name ancillary-data-set) stream
	(let ((stream-state (make-stream-state :sync-closure #'frame-sync :container-closure nil :stream-def stream :server server)))
	  (log:info "Initialized state for ~A" (name stream))
	  (setf (gethash port *port->state*) stream-state))))

   ;; (make-service '|Service.CCSDS.MPDU|
		;; 			  (list (make-container-ref '|STC.CCSDS.MPDU.Container.MPDU|))
		;; 			  :short-description "MPDU Decoding for VCID 43"
		;; 			  :ancillary-data-set
		;; 			  (xtce::make-ancillary-data-set
		;; 			   (xtce::make-ancillary-data 'Service-Function 'stc::decode-mpdu)
		;; 			   (xtce::make-ancillary-data 'VCID 43)
		;; 			   (xtce::make-ancillary-data 'Next-Service '|Service.CCSDS.Space-Packet|)))
		

		;; (make-service '|Service.CCSDS.Space-Packet|
		;; 			  (list (make-container-ref '|STC.CCSDS.Space-Packet|))
		;; 			  :short-description "CCSDS Space Packet Decoding"
		;; 			  :ancillary-data-set
		;; 			  (xtce::make-ancillary-data-set
		;; 			   (xtce::make-ancillary-data 'Service-Function 'identity)))
; stc::'ccsds.aos.frame.decode


(defparameter Test-System
  (make-space-system
   '|Test-System|
   :root t

   :short-description
   "Root system for the Test System"

   :ancillary-data-set
   (list (make-ancillary-data "test" "1"))

   :telemetry-metadata
   (make-telemetry-metadata
	
	:parameter-type-set
	(stc::with-ccsds.aos.header.types
		(stc::with-ccsds.mpdu.types
			(stc:with-ccsds.space-packet.header.types nil)))

	:parameter-set
	(stc::with-ccsds.mpdu.parameters
		(stc::with-ccsds.aos.header.parameters
			(stc::with-ccsds.space-packet.header.parameters nil)))

	:container-set
	(stc::with-ccsds.aos.containers nil)
	
	:stream-set
	(stc:with-ccsds.aos.stream 1024 8888 15))))


;; (lparallel.queue:push-queue #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF input-queue)

;; (lparallel.queue:push-queue nil input-queue)

;;; Web Service Side
(defun create-stream-listener (env)
  (let ((ws (wsd:make-server env)))
	(wsd:on :open ws
			(lambda ()
			  (handle-open ws env)))

	(wsd:on :message ws
			(lambda (message)
			  (handle-message ws message env)))

	(wsd:on :error ws
			(lambda (error)
			  (format t "Got an error: ~S~%" error)))

	(wsd:on :close ws
			(lambda (&key code reason)
			  (format t "Closed because '~A' (Code=~A)~%" reason code)))

	(lambda (responder)
        (declare (ignore responder))
        (wsd:start-connection ws))
	))

(defun handle-error (ws code reason))

(defun handle-open (ws env)
  (log:info "Connected.~%")
  (let* ((server-port (getf env :SERVER-PORT))
		 (port-state (gethash server-port *port->state*)))
	port-state))
		 
	
(defun handle-close (ws)
  (log:info "Connection closed: ~A" ws))

(defun handle-message (ws message env)
  (let* ((port (getf env :SERVER-PORT))
		 (stream-state (gethash port *port->state*))
		 (message (byte-array-to-uint message)))
	(multiple-value-bind (stream-result state next-continuation) (funcall (stream-state-sync-closure stream-state)
																		  message (stream-state-stream-def stream-state))
	  (setf (stream-state-sync-closure stream-state) next-continuation)
	  (setf (gethash port *port->state*) stream-state)
	  (log:info next-continuation)
	  (log:info stream-result)
	  (log:info state)
	  (wsd:send-text ws (format nil "~A" state))
	  )
	)
  )
 
;;; Server Side
(defun start-stream-listener (stream)
  (with-slots (ancillary-data-set) stream
	  (let* ((port (xtce::value (gethash :port (xtce::items ancillary-data-set))))
			 (listener (progn
						 (clack:clackup #'create-stream-listener :port port))))
		(log:info "Started stream listener on port ~A" port)
		(initialize-stream-state port stream listener))))

(defun start-stream-listeners (stream-list)
  (check-type stream-list xtce::stream-set)
  (dolist (stream stream-list)
	(start-stream-listener stream)))
  
(defun stop-stream-listeners ()
  (log:info "Shutting down stream listeners.")
  (maphash #'stop-stream-listener
		   *port->state*))

(defun stop-stream-listener (port stream-state)
  (let* ((listener (stream-state-server stream-state))
		 (stopped (clack:stop listener)))
	(declare (ignore stopped))
	(remhash port *port->state*)
	(log:info "Shutdown stream listener ~A" (name (stream-state-stream-def stream-state)))))

(defun bifrost.start (space-system)
  (with-slots (telemetry-metadata) space-system
	(start-stream-listeners (xtce::stream-set telemetry-metadata))))
 
(bifrost.start Test-System)

(defparameter tt
  (hex-string-to-byte-array "1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))

(defparameter *client* (wsd:make-client "ws://127.0.0.1:8888"))

(sleep 2)
(wsd:start-connection *client*)
;; ;; (wsd:on :message *client*
;; ;;         (lambda (message)
;; ;;           (format t "~&Got: ~A~%" message)))
;; ;; (wsd:send *client* "Hi")
(wsd:send-binary *client* tt)
(wsd:send-binary *client* tt)
(wsd:send-binary *client* tt)
(wsd:send-binary *client* tt)
(wsd:send-binary *client* tt)
(wsd:send-binary *client* tt)
(wsd:close-connection *client*)
(stop-stream-listeners)

;;TODO:
;; What should we define as a space system?
;; Maybe a space system should correspond to a single onboard computer with its subsystems pointing to services
;; This would help flatten a large root space system.

;; TODO:
;; We're going to use streams for setting up bifrost services.
;; We'll traverse into the root system's telemetry node and pick up what we need from the ancillary data, mainly the port.
;; We'll pick up the next stage by looking at the container/service/stream ref
;; If container then we decode, if stream-ref we place into the stream queue, we don't know what to use services for.
;; All stream outputs will be published via the websocket.
;; The receiving end will be a small adaptor routing input from DSN/SIM/Radio and routing output from ygdrassil to the GDS network (i.e. RabbitMQ)
;; We could also write more adaptors to read telemetry from files (command loader), s3, binary dump, etc...
;; I think this approach, relying on websockets and adaptors, would provide the most flexibility for fulfilling mission needs.
