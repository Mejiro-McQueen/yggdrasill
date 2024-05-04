(declaim (optimize (speed 0) (space 0) (debug 3)))
;(defvar debug-mode t)
;(setf lparallel:*kernel* (lparallel:make-kernel 10))

(in-package :xtce-engine)

;;Test Data
(defparameter tt
  (hex-string-to-byte-array "1acffc1dFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF"))

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
	(stc::with-ccsds.mpdu.containers
		(stc::with-ccsds.aos.containers nil))
	
	:stream-set
	(stc:with-ccsds.aos.stream 1024 8888 15)
	)

   :service-set
   (list ;; (make-service "STC.CCSDS.Space-Packet.Stream.15"
		 ;; 			   (list (make-container-ref '|STC.CCSDS.Space-Packet|))
		 ;; 			   :short-description "CCSDS Space Packet Decoding"
		 ;; 			   :ancillary-data-set
		 ;; 			   (xtce::make-ancillary-data-set
		 ;; 				;(make-ancillary-data :service-function nil)
		 ;; 				(make-ancillary-data :port 8900)))
		 (make-service "STC.CCSDS.MPDU.Stream.15"
					   (list (make-container-ref '|STC.CCSDS.MPDU|))
					   :short-description "MPDU Decoding for VCID 15"
					   :ancillary-data-set
					   (xtce::make-ancillary-data-set
						(make-ancillary-data :service-function #'stc::decode-mpdu-service)
						(make-ancillary-data :port 8901)
						(xtce::make-ancillary-data 'VCID 43)
						(xtce::make-ancillary-data 'next-stream '|Service.CCSDS.Space-Packet.15|))))))

;Server State Management
(defvar *port->stream-name* (make-hash-table :test 'equal))
(defvar *stream-name->input-queue* (make-hash-table :test 'equal))
(defvar *stream-name->output-queue* (make-hash-table :test 'equal))
(defvar *stream-name->output-thread* (make-hash-table :test 'equal))
(defvar *stream-name->input-thread* (make-hash-table :test 'equal))
(defvar *stream-name->server-state* (make-hash-table :test 'equal))

(defclass server-state ()
  ((server :initarg :server :reader server)
   (symbol-table :initarg :symbol-table :accessor symbol-table)
   (xtce-definition :initarg :xtce-definition :accessor xtce-definition)
   (server-closure :initarg :server-closure :accessor server-closure)))

(defclass telemetry-commanding-server (server-state)
  ((container-closure :initarg :container-closure :accessor container-closure))) ;override to  stream-def?

(defclass telemetry-stream-state (telemetry-commanding-server) ())
(defclass command-stream-state (telemetry-commanding-server) ())
(defclass algorithm-server (server-state) ())
(defclass service-stream-state (server-state)
  ((container-closure :initarg :container-closure :accessor container-closure)))

(defmacro with-server (xtce-obj server-handler &body body)
  `(with-slots (ancillary-data-set) ,xtce-obj
	 (let* ((name (name ,xtce-obj))
			(port (xtce::value (gethash :port (xtce::items ancillary-data-set))))
			(server (clack:clackup ,server-handler :port port)))
	   ,@body)))

(defun make-telemetry-stream (telemetry-stream-def symbol-table)
  (log:info "Telemetry stream ~A is up." (name telemetry-stream-def))
  (with-server telemetry-stream-def #'telemetry-stream-handler
	(update-states #'service-telemetry-stream-queue
				   name
				   (make-instance 'telemetry-stream-state
								  :server-closure #'frame-sync
								  :container-closure nil
								  :xtce-definition telemetry-stream-def
								  :server server
								  :symbol-table symbol-table)
				   port)))

(defun make-command-stream (command-stream-def symbol-table))

(defun make-service-stream (service-def symbol-table)
  (log:info "Service stream ~A is up." (name service-def))
    (with-server service-def #'service-stream-handler
	  (let ((server-closure (gethash :service-function (xtce::get-ancillary-data service-def))))
		(if (functionp server-closure)
			(update-states #'service-service-queue
						   name
						   (make-instance 'service-stream-state
										  :server-closure server-closure
										  :container-closure nil
										  :xtce-definition service-def
										  :server server
										  :symbol-table symbol-table)
						   port)
			(log:info "Skipping creating service for ~A due to no server closure defined." name)))))

(defun update-states (thread-function name server-state port)
  (let* ((input-queue (lparallel.queue:make-queue))
		 (output-queue (lparallel.queue:make-queue))
		 (input-thread (bt:make-thread (lambda () (funcall thread-function
													  name
													  input-queue
													  output-queue)))))
		 (setf (gethash port *port->stream-name*) name)
		 (setf (gethash name *stream-name->input-queue*) input-queue)
		 (setf (gethash name *stream-name->output-queue*) output-queue)
		 (setf (gethash name *stream-name->input-thread*) input-thread)
		 (setf (gethash name *stream-name->server-state*) server-state)))

; Server Handling
(defun service-telemetry-stream-queue (stream-name input-queue output-queue)
  (loop
	(let* ((message (byte-array-to-uint (lparallel.queue:pop-queue input-queue)))
		   (server-state (gethash stream-name *stream-name->server-state*))
		   (stream-def (xtce-definition server-state))
		   (next-stream (stream-ref stream-def))
		   (next-stream-input-queue nil))
	  (multiple-value-bind (result state next-continuation) (funcall (server-closure server-state)
																	 message
																	 (xtce-definition server-state)
																	 (symbol-table server-state))
		(setf (server-closure server-state) next-continuation)
		(setf (gethash stream-name *stream-name->server-state*) server-state)
		;; (log:info next-continuation)
		;; (log:info result)
		;; (log:info state)
										;Send to socket output queue
		(lparallel.queue:push-queue result output-queue)
										;Send copy to next stream
		(when result
		  (typecase next-stream
			(xtce::stream-ref
			 (setf next-stream (xtce::ref next-stream)))
			(t (log:info next-stream)))

		  (setf next-stream-input-queue (gethash next-stream *stream-name->input-queue*))
										;(log:info "Pushing data to next stream ~A? ~A" next-stream next-stream-input-queue)
		  (when next-stream-input-queue
			(lparallel.queue:push-queue result next-stream-input-queue)))))))

(defun service-service-queue (service-name input-queue output-queue)
  (loop
	(let* ((message (lparallel.queue:pop-queue input-queue))
		   (server-state (gethash service-name *stream-name->server-state*))
		   (service-def (xtce-definition server-state))
		   (next-stream nil)
		   (next-stream-input-queue (gethash next-stream *stream-name->input-thread*)))

	  (log:error (server-closure server-state))
	  (multiple-value-bind (result state next-continuation) (funcall (server-closure server-state)
																	 message
																	 (xtce-definition server-state)
																	 (symbol-table server-state))
		(setf (server-closure server-state) next-continuation)
		(setf (gethash service-name *stream-name->server-state*) server-state)
		;; (log:info next-continuation)
		;; (log:info result)
		;; (log:info state)
										;Send to socket output queue
		(lparallel.queue:push-queue result output-queue)
		(setf message result)
										;Send copy to next stream
		(setf next-stream (gethash "next-stream" (xtce::get-ancillary-data service-def)))
		(when (and result next-stream-input-queue)
		  (log:debug "Pushing data to next stream ~A" next-stream)
		  (lparallel.queue:push-queue message next-stream-input-queue))
		))))

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
			  (handle-stream-message ws message env)))

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

(defun service-stream-handler (env)
  (let ((ws (wsd:make-server env)))
	(wsd:on :open ws
			(lambda () (handle-open ws env)))

	(wsd:on :message ws
			(lambda (message) (handle-service-message ws message env)))

	(wsd:on :error ws
			(lambda (error) (format t "Got an error: ~S~%" error)))

	(wsd:on :close ws
			(lambda (&key code reason) (handle-close ws env code reason)))

	(lambda (responder)
        (declare (ignore responder))
        (wsd:start-connection ws))
	))

(defun handle-error (ws code reason)
  (declare (ignore ws code reason)))

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
	(remhash output-thread *stream-name->output-thread*)
	(log:info "Stopped output thread for stream ~A and closed connection. Reason: ~A (code=~A)" stream-name reason code)))

(defun handle-stream-message (ws message env)
  (declare (ignore ws))
  (let* ((port (getf env :SERVER-PORT))
		 (stream-name (gethash port *port->stream-name*))
		 (stream-input (gethash stream-name *stream-name->input-queue*)))
	(lparallel.queue:push-queue message stream-input)))

(defun handle-service-message (ws message env)
  (declare (ignore ws))
  (let* ((port (getf env :SERVER-PORT))
		 (stream-name (gethash port *port->stream-name*))
		 (stream-input (gethash stream-name *stream-name->input-queue*)))
	(lparallel.queue:push-queue message stream-input)))
 

;;; Server Controls 
(defun stop-stream-server (stream-name)
  (let* ((server-state (gethash stream-name *stream-name->server-state*)))
	(when server-state (server server-state)
		  (clack:stop (server server-state))
		  (remhash stream-name *stream-name->server-state*)
		  (log:info "Shutdown server ~A" stream-name))))

;;; Server Controls 
(defun stop-service-server (service-name)
  (let* ((server-state (gethash service-name *stream-name->server-state*)))
	(when server-state (server server-state)
		  (clack:stop (server server-state))
		  (remhash service-name *stream-name->server-state*)
		  (log:info "Shutdown server ~A" service-name))))

(defun stop-servers (space-system service-type)
  (with-slots (telemetry-metadata command-metadata service-set symbol-table) space-system
	(case service-type
	  (:telemetry
	   (when telemetry-metadata
		 (dolist (i (stream-set telemetry-metadata))
		   (stop-stream-server (name i)))))
	  (:command
	   (when command-metadata
		 (dolist (i (stream-set command-metadata))
		   (stop-stream-server (name i)))))
	  (:service
	   (dolist (i (service-set space-system))
		   (stop-service-server (name i)))))))

(defun start-servers (space-system service-type)
  (with-slots (telemetry-metadata command-metadata service-set symbol-table) space-system
	  (case service-type
		(:telemetry
		 (when telemetry-metadata
		   (dolist (i (stream-set telemetry-metadata))
			 (make-telemetry-stream i symbol-table))))
		(:command
		 (when command-metadata
		   (dolist (i (stream-set command-metadata))
			 (make-command-stream i symbol-table))))
		(:service
		   (dolist (i service-set)
			 (make-service-stream i symbol-table))))))

(defun bifrost.start (space-system)
	(start-servers space-system :telemetry)
	(start-servers space-system :command)
	(start-servers space-system :service))

(defun bifrost.stop (space-system)
	(stop-servers space-system :telemetry)
	(stop-servers space-system :command)
	(stop-servers space-system :service))
 
(bifrost.start Test-System)

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

(sleep 2)
(bifrost.stop Test-System)

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

;(32°F − 32) × 5/9 = 0°C


;; We can define the algorithm as a regular lisp function
;; This lets us use and test it in the typical fashion.
;; By making a call to function-lambda-expression we can embed it into the algorithm text
;; Functions that are evaluated in a compiled program are themselves compiled.
;; The input-set should compose an ordered set of constants and input-parameter-instance-ref
;; Whenever input-parameter-instance-ref defines input-name, parameter-ref will be mapped to the key input-name in the algorithm function definition
;; Whenever consant defines constant-name, the constant value will be mapped to the key constant-name in the algorithm function definition
;; Otherwise the input-parameter-instance-ref and constant name will be passed in as regular positional arguments
;; You may choose to define an algorithm function with no, some, or all keys.
;; You may use default arguments
;; Your function must return an alist of parameters to values
;; If output-set is available the prameter-ref values will be copied to to algorithm's output under the output-name if it is set.
(defun f->c (&key degrees-fahrenheit  calibration-offset (useless-offset 0))
						   (+ useless-offset calibration-offset (/ (- degrees-fahrenheit 32) (/ 9 5) )))

(defparameter test1
  (make-decoding-algorithm 'Test
						   :algorithm-text (make-algorithm-text (function-lambda-expression #'f->c))
						   :input-set (list (make-input-parameter-instance-ref
											 'deg-f
											 :input-name 'degrees-fahrenheit)
											(make-constant 100 :constant-name 'calibration-offset))))

;;; Otherwise, we can simply quote the function definition
;; (defparameter f->c
;; 	(make-algorithm-text '(defun f->c (&key degrees-fahrenheit  calibration-offset)
;; 						   (+ calibration-offset (/ (- degrees-fahrenheit 32) (/ 9 5) )))))

;; (defparameter test1
;;   (make-decoding-algorithm 'Test
;; 						   :algorithm-text f->c
;; 						   :input-set (list (make-input-parameter-instance-ref
;; 											 'deg-f
;; 											 :input-name 'degrees-fahrenheit)
;; 											(make-constant 100 :constant-name 'calibration-offset)
;; 											)))

(defparameter alist '((deg-f . 100)))

(defun eval-algorithm (decoding-algorithm parameter-alist)
  (let ((algorithm (eval (xtce::algorithm-text (xtce::algorithm-text decoding-algorithm))))
		(input-set (xtce::input-set decoding-algorithm))
		(arg-list nil))
	(dolist (argument input-set)
	  (print argument)
	  (typecase argument
		(xtce::input-parameter-instance-ref
		 (when (xtce::input-name argument)
		   (push (alexandria:make-keyword (xtce::input-name argument)) arg-list ))
		 (log:info "Searching for ~A in ~A" (xtce::parameter-ref argument) parameter-alist)
		 (push (cdr (assoc (xtce::parameter-ref argument) parameter-alist)) arg-list))
		(xtce::constant
		 (when (xtce::constant-name argument)
		   (push (alexandria:make-keyword (xtce::constant-name argument)) arg-list))
		 (push (xtce::value argument) arg-list))))
	(setf arg-list (nreverse arg-list))
	(log:info "Applying ~A with ~S" algorithm arg-list)
	(apply algorithm arg-list)))

;; (eval-algorithm test1 alist)

(gethash "STC.CCSDS.MPDU.Stream.15" *stream-name->input-queue*)
