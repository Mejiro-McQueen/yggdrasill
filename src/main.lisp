(ql:quickload '(:clack :websocket-driver))
(ql:quickload :websocket-driver-client)

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


;;; Services
;;; A service is identified exclusively by its name

(defparameter *service-states* (make-hash-table))

(defun generate-service-states (service-list)
  "Generate all services"
  (dolist (service service-list)
	(with-slots (name ancillary-data-set) service
	  (let ((f (gethash 'Service-Function (xtce::items ancillary-data-set))))
		(if (typep f xtce::'ancillary-data)
			(setf (gethash name *service-states*) (symbol-function (xtce::value f))))
		(log:info "Generated service state for ~A using ~A" name (symbol-function (xtce::value f)))))))


;; (generate-service-states test-service-list)

(defparameter test-services-list
  (list (make-service '|Service.CCSDS.MPDU|
					  (list (make-container-ref '|STC.CCSDS.MPDU.Container.MPDU|))
					  :short-description "MPDU Decoding for VCID 43"
					  :ancillary-data-set
					  (xtce::make-ancillary-data-set
					   (xtce::make-ancillary-data 'Service-Function 'stc::decode-mpdu)
					   (xtce::make-ancillary-data 'VCID 43)
					   (xtce::make-ancillary-data 'Next-Service '|Service.CCSDS.Space-Packet|)))
		
		(make-service '|Service.CCSDS.AOS.Frame|
					  (list (make-container-ref '|STC.CCSDS.AOS.Container.Frame|))
					  :short-description "Test AOS Frame Decoding"
					  :ancillary-data-set
					  (xtce::make-ancillary-data-set
					   (xtce::make-ancillary-data 'Service-Function stc::'ccsds.aos.frame.decode)
					   (xtce::make-ancillary-data 'Next-Service '|Service.CCSDS.MPDU|)))

		(make-service '|Service.CCSDS.Space-Packet|
					  (list (make-container-ref '|STC.CCSDS.Space-Packet|))
					  :short-description "CCSDS Space Packet Decoding"
					  :ancillary-data-set
					  (xtce::make-ancillary-data-set
					   (xtce::make-ancillary-data 'Service-Function 'identity)))))

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
	(stc:with-ccsds.aos.stream 1024 8081))
   
   :service-set test-services-list))


;; (lparallel.queue:push-queue #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF input-queue)

;; (lparallel.queue:push-queue nil input-queue)

(defparameter *ws->state* (make-hash-table))

(defparameter *port->state* (make-hash-table))

;;; Web Service Side
(defun create-frame-listener (env)
  (let ((ws (wsd:make-server env)))
	(wsd:on :open ws
			(lambda ()
			  (handle-open ws env)))

	(wsd:on :message ws
			(lambda (message)
			  (handle-message ws message)))

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
		 (stream (second (gethash server-port *port->state*)))
		 (f (lambda (frame) (frame-sync frame stream))))
	(setf (gethash ws *ws->state*) f)))
	
(defun handle-close (ws)
  (log:info "Connection closed: ~A" ws))

(defun handle-message (ws message)
  (let ((frame-sync-func (gethash ws *ws->state*))
		(message (byte-array-to-uint message)))
	(multiple-value-bind (frame-result state next-continuation) (funcall frame-sync-func message)
	  (setf (gethash ws *ws->state*) next-continuation)
										;(log:info frame-result)
	  (log:info state)
	  (wsd:send-text ws (format nil "~A" state))
	  )
	))
 
;;; Server Side
(defun start-frame-listener (stream)
  (with-slots (ancillary-data-set) stream
	  (let* ((port (xtce::value (gethash :port (xtce::items ancillary-data-set))))
			 (listener (progn
						 (assert port (port) "No key :port found for ~A in ancillary data" stream)
						 (clack:clackup #'create-frame-listener :port port))))
		(log:info "Started frame listener on port ~A" port)
		(values listener port))))

(defun start-frame-listeners (stream-list)
  (check-type stream-list xtce::stream-set)
  (dolist (stream stream-list)
	(multiple-value-bind (listener port) (start-frame-listener stream)
	  (setf (gethash port *port->state*) (list listener stream)))))

(defun stop-frame-listeners ()
  (log:info "Shutting down frame listeners.")
  (maphash #'stop-frame-listener
		   *port->state*))

(defun stop-frame-listener (port stream-info)
  (let* ((listener (first stream-info))
		 (stream (second stream-info))
		 (stopped (clack:stop listener)))
	(remhash port *port->state*)
	(log:info stopped)
	(log:info "Shutdown frame listener ~A" stream)))

(defun bifrost.start (space-system)
  (with-slots (telemetry-metadata) space-system
	(start-frame-listeners (xtce::stream-set telemetry-metadata))))
 
;; (bifrost.start Test-System)
;; (stop-frame-listeners)

;; (defparameter *client* (wsd:make-client "ws://localhost:8081"))

;; (wsd:start-connection *client*)
;; ;; (wsd:on :message *client*
;; ;;         (lambda (message)
;; ;;           (format t "~&Got: ~A~%" message)))
;; ;; (wsd:send *client* "Hi")
;; (wsd:send-binary *client* tt)
;; (wsd:close-connection *client*)

;; (U8-Array->Hex tt)

;; (U8-Array->uint tt)

;; (defparameter tt
;;   (hex-string-to-byte-array "1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))

;; (xtce-engine::byte-array-to-uint tt)

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
