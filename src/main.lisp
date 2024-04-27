(declaim (optimize (speed 0) (space 0) (debug 3)))
;(defvar debug-mode t)
;(setf lparallel:*kernel* (lparallel:make-kernel 10))

(in-package :xtce-engine)

;;Test Data
(defparameter tt
  (hex-string-to-byte-array "1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))

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


;;; Streams
(defvar *stream-name->input-queue* (make-hash-table))
(defvar *stream-name->output-queue* (make-hash-table))
(defvar *stream-name->output-thread* (make-hash-table))
(defvar *stream-name->input-thread* (make-hash-table))
(defvar *stream-name->stream-state* (make-hash-table))
(defvar *port->stream-name* (make-hash-table))

*stream-name->stream-state*

(defstruct stream-state sync-closure container-closure stream-def server symbol-table)

(defgeneric initialize-stream-state (xtce-stream server symbol-table))
(defmethod initialize-stream-state ((stream fixed-frame-stream) (server t) (symbol-table t))
  (with-slots (name) stream
	(let ((stream-state (make-stream-state :sync-closure #'frame-sync
										   :container-closure nil
										   :stream-def stream
										   :server server
										   :symbol-table symbol-table)))
	  (log:info "Initialized state for ~A" (name stream))
	  (let* ((input-queue (lparallel.queue:make-queue))
			(output-queue (lparallel.queue:make-queue))
			(input-thread (bt:make-thread
						   (lambda () (start-telemetry-stream-input-thread name
																	  input-queue
																	  output-queue)))))
		(setf (gethash name *stream-name->input-queue*) input-queue)
		(setf (gethash name *stream-name->output-queue*) output-queue)
		(setf (gethash name *stream-name->input-thread*) input-thread)
		(setf (gethash name *stream-name->stream-state*) stream-state)))))

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

(defun start-telemetry-stream-input-thread (stream-name input-queue output-queue)
  (loop
	(let* ((message (byte-array-to-uint (lparallel.queue:pop-queue input-queue)))
		   (stream-state (gethash stream-name *stream-name->stream-state*))
		   (stream-def (stream-state-stream-def stream-state))
		   (next-stream (stream-ref stream-def))
		   (next-stream-input-queue (gethash next-stream *stream-name->input-thread*)))
	  (multiple-value-bind (result state next-continuation) (funcall (stream-state-sync-closure stream-state)
																			message
																			(stream-state-stream-def stream-state)
																			(stream-state-symbol-table stream-state))
		(setf (stream-state-sync-closure stream-state) next-continuation)
		(setf (gethash stream-name *stream-name->stream-state*) stream-state)
		(log:info next-continuation)
		(log:info result)
		(log:info state)
										;Send to socket output queue
		(lparallel.queue:push-queue result output-queue)
										;Send copy to next stream
		(when next-stream-input-queue
		  (log:debug "Pushing data to next stream ~A" next-stream)
		  (lparallel.queue:push-queue result next-stream-input-queue))))))
	
(defun start-stream-output-thread (input-queue websocket)
  (loop
	(wsd:send-text websocket (lparallel.queue:pop-queue input-queue))))

(defun telemetry-stream-handler (env)
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
			  (handle-close ws env code reason)))

	(lambda (responder)
        (declare (ignore responder))
        (wsd:start-connection ws))
	))

(defun handle-error (ws code reason))

(defun handle-open (ws env)
  (let* ((server-port (getf env :SERVER-PORT))
		 (stream-name (gethash server-port *port->stream-name*))
		 (stream-output (gethash stream-name *stream-name->output-queue*))
		 (existing-output-thread? (gethash stream-name *stream-name->output-thread*)))
	(when existing-output-thread?
	  (log:warn "A connection previously existed for stream ~A on port ~A. Replacing connection." stream-name server-port)
	  (bt:destroy-thread existing-output-thread?))
	(let ((new-thread (bt:make-thread (lambda () (start-stream-output-thread stream-output ws)))))
	  (setf existing-output-thread? new-thread))
  (log:info "Started output thread for stream ~A" stream-name)))
  
(defun handle-close (ws env &key code reason)
  (declare (ignore ws))
  (let* ((server-port (getf env :SERVER-PORT))
		 (stream-name (gethash server-port *port->stream-name*))
		 (output-thread (gethash stream-name *stream-name->output-thread*)))
	(bt:destroy-thread output-thread)
	(remhash stream-name *stream-name->output-thread*)
	(log:info "Stopped output thread for stream ~A and closed connection. Reason: ~A (code=~A)" stream-name reason code)))

(defun handle-message (ws message env)
  (declare (ignore ws))
  (let* ((port (getf env :SERVER-PORT))
		 (stream-name (gethash port *port->stream-name*))
		 (stream-input (gethash stream-name *stream-name->input-queue*)))
	(lparallel.queue:push-queue message stream-input)))
 
;;; Server Side
(defun start-telemetry-stream-server (stream symbol-table)
  (with-slots (name ancillary-data-set) stream
	  (let* ((port (xtce::value (gethash :port (xtce::items ancillary-data-set))))
			 (server (clack:clackup #'telemetry-stream-handler :port port)))
		(initialize-stream-state stream server symbol-table)
		(setf (gethash port *port->stream-name*) name)
		(log:info "Started stream server on port ~A for stream ~A" port stream))))

(defun start-telemetry-stream-servers (stream-list symbol-table)
  (check-type stream-list xtce::stream-set)
  (dolist (stream stream-list)
	(start-telemetry-stream-server stream symbol-table)))
	  
(defun stop-telemetry-stream-servers ()
  (log:info "Shutting down stream servers.")
  (maphash #'stop-telemetry-stream-server
		   *stream-name->stream-state*))

(defun stop-telemetry-stream-server (stream-name stream-state)
  (let* ((server (stream-state-server stream-state))
		 (stopped (clack:stop server)))
	(declare (ignore stopped))
	(remhash stream-name *stream-name->stream-state*)
	(log:info "Shutdown stream server ~A" (name (stream-state-stream-def stream-state)))))

(defun bifrost.start (space-system)
  (with-slots (telemetry-metadata) space-system
	(start-telemetry-stream-servers (xtce::stream-set telemetry-metadata) (symbol-table space-system))))

 
(bifrost.start Test-System)

(defparameter *client* (wsd:make-client "ws://127.0.0.1:8888"))

(sleep 1)
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
(stop-telemetry-stream-servers)

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
;; The receiving end will be a small adaptor routing input from DSN/SIM/Radio and routing output from ygdrasil to the GDS network (i.e. RabbitMQ)
;; We could also write more adaptors to read telemetry from files (command loader), s3, binary dump, etc...
;; I think this approach, relying on websockets and adaptors, would provide the most flexibility for fulfilling mission needs.



;; (decode (uint->bit-vector #x1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF) (make-container-ref '|STC.CCSDS.AOS.Container.Transfer-Frame-Primary-Header|) (symbol-table Test-System) '() 0)





