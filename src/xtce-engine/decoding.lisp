(in-package :xtce-engine)

(define-condition fragmented-packet-error (SB-KERNEL:BOUNDING-INDICES-BAD-ERROR)
  ((start :initarg :start :reader start)
   (end :initarg :end :reader end))
  (:report (lambda (condition stream) (format stream "Attempted to index outside of frame data while reading packet -> start: ~A, end:~A" (start condition) (end condition)))))

(defun packet-subseq (sequence start &optional end)
  (handler-case (subseq sequence start end)
	(SB-KERNEL:BOUNDING-INDICES-BAD-ERROR ()
	  (signal 'fragmented-packet-error :start start
									   :end end))))


;;;Decode returns from encodings should not modify the alist, this may change in the future when we also want to record precalibrated values
;; Parameters must call decode on the encoding, calibrate the value, place it into the alist, and multiple value return

(defgeneric decode (data decodable-object symbol-table alist bit-offset))

;Dispatch on container-ref
(defmethod decode (data (container-ref-entry xtce::container-ref-entry) symbol-table alist bit-offset)
  (log:debug "Dispatching on: " container-ref-entry)
  (let* ((dereferenced-container (xtce:dereference container-ref-entry symbol-table)))
	(multiple-value-bind (res next-bit-offset) (decode data dereferenced-container symbol-table alist bit-offset)
	  (log:debug bit-offset next-bit-offset res)
	  (values res next-bit-offset))))

(defmethod decode (data (parameter-ref-entry xtce::parameter-ref-entry) symbol-table alist bit-offset)
  (log:debug "Dispatching on:" parameter-ref-entry)
  (let* ((dereferenced-container (xtce:dereference parameter-ref-entry symbol-table)))
	(multiple-value-bind (res next-bit-offset) (decode data dereferenced-container symbol-table alist bit-offset)
	  (log:debug bit-offset next-bit-offset res)
	  (values res next-bit-offset))))

;; Dispatch on container
(defmethod decode (data (container xtce::sequence-container) symbol-table alist bit-offset)
  (with-slots (name) container
	(log:debug "Dispatch on: " name)
	(let ((res-list '()))
	  (dolist (ref (entry-list container))
		(multiple-value-bind (res next-bit-offset) (decode data ref symbol-table (append alist res-list) bit-offset)
		  ;(print bit-offset)
		  (log:debug "Got Result: ~A, ~A" next-bit-offset res)
		  (setf bit-offset next-bit-offset)
		  (typecase ref
			(xtce::container-ref-entry
			 (log:debug "Appending container result.")
			 (setf res-list (append res-list res)))
			(xtce::parameter-ref-entry
			 (log:debug "Pushing parameter result.")
			 (push res res-list)))))
	  (log:debug "Finished container processing: ~A, ~A" bit-offset res-list)
	  (values res-list bit-offset))))

;; Dispatch on Parameter
(defmethod decode (data (parameter xtce::parameter) symbol-table alist bit-offset)
  (log:debug "Dispatch on: " parameter)
  (with-slots (name) parameter
	(let ((parameter-type (xtce::dereference parameter symbol-table)))
	  (assert parameter-type () "No dereference for parameter ~A" parameter)
	  (multiple-value-bind (res next-bit-offset) (decode data parameter-type symbol-table alist bit-offset)
		(log:debug bit-offset next-bit-offset res)
		(values (cons name res) next-bit-offset)))))

;; Dispatch on binary-parameter-type
(defmethod decode (data (parameter-type xtce::binary-parameter-type) symbol-table alist bit-offset)
  (log:debug "Dispatch on: " parameter-type)
  (with-slots (name) parameter-type
	(let ((data-encoding (xtce:data-encoding parameter-type)))
	  (unless data-encoding 
		(error "Can not decode data from stream without a data-encoding for ~A" parameter-type))
	  (multiple-value-bind (res next-bit-offset) (decode data data-encoding symbol-table alist bit-offset)
		;(setf res (bit-vector->hex res))
		(log:debug bit-offset next-bit-offset res)
		(values res next-bit-offset)))))

;; Dispatch on string-parameter-type
(defmethod decode (data (parameter-type xtce::string-parameter-type) symbol-table alist bit-offset)
  (with-slots (name) parameter-type
	(let ((data-encoding (xtce:data-encoding parameter-type)))
	  (unless data-encoding 
		(error "Can not decode data from stream without a data-encoding for ~A" parameter-type))
	  (multiple-value-bind (res next-bit-offset) (decode data data-encoding symbol-table alist bit-offset)
										;(setf res (bit-vector->hex res))
		(log:debug bit-offset next-bit-offset res)
		(values res next-bit-offset)))))

;;Decode binary-data encoding
(defmethod decode (data (encoding xtce::binary-data-encoding) symbol-table alist bit-offset)
  (with-slots (xtce::size-in-bits) encoding
	(let* ((size-in-bits (xtce::resolve-get-size xtce::size-in-bits :alist alist :db-connection nil))
		   (data-segment (packet-subseq data bit-offset (+ bit-offset size-in-bits)))
		   (next-offset (+ bit-offset size-in-bits)))
	  (log:debug "Extracting: " size-in-bits)
	  (log:debug bit-offset next-offset data-segment)
	  (values data-segment next-offset))))

;; Dispatch on enumerated-parameter-type
(defmethod decode (data (parameter-type xtce::enumerated-parameter-type) symbol-table alist bit-offset)
  (log:debug parameter-type)
  (with-slots (name) parameter-type
	(let ((data-encoding (xtce:data-encoding parameter-type)))
	  (unless data-encoding 
		(error "Can not decode data from stream without a data-encoding for ~A" parameter-type))
	  (multiple-value-bind (res next-bit-offset) (decode data data-encoding symbol-table alist bit-offset)
		(log:debug bit-offset next-bit-offset res)
		(values res next-bit-offset)))))

;;Decode boolean parameter 
(defmethod decode (data (parameter-type xtce::boolean-parameter-type) symbol-table alist bit-offset)
  (log:debug parameter-type)
  (with-slots (name) parameter-type
	(let* ((data-encoding (xtce:data-encoding parameter-type))
		   (res nil))
	  (unless data-encoding ;Empty data-encoding is only valid for ground derrived telemetry 			
		(error "Can not decode data from stream without a data-encoding for ~A" parameter-type))
	  (multiple-value-bind (decoded-flag next-bit-offset) (decode data data-encoding symbol-table alist bit-offset)
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
		(log:debug next-bit-offset bit-offset res)
		(values res next-bit-offset)))))

;;Decode Integer Parameter
(defmethod decode (data (parameter-type xtce::integer-parameter-type) symbol-table alist bit-offset)
  (log:debug parameter-type)
  (let ((data-encoding (xtce:data-encoding parameter-type)))
	(unless data-encoding ;Empty data-encoding is only valid for ground derrived telemetry 			
	  (error "Can not decode data from stream without a data-encoding for ~A" parameter-type))
	(multiple-value-bind (res next-bit-offset) (decode data data-encoding symbol-table alist bit-offset)
	  (with-slots (xtce::name) parameter-type
		(log:debug bit-offset res)
		(values res next-bit-offset)))))

;;Decode Integer Encoding
(defmethod decode (data (integer-data-encoding xtce::integer-data-encoding) symbol-table alist bit-offset)
  (log:debug integer-data-encoding)
  (with-slots (xtce::integer-encoding xtce::size-in-bits) integer-data-encoding
	(let ((res nil)
		  (next-bit-offset (+ bit-offset xtce::size-in-bits)))
	  (case xtce::integer-encoding
		(xtce::'unsigned
		 (let* ((data-segment (subseq data bit-offset next-bit-offset)))
		   ;;(print (format nil "~A ~A" bit-offset next-bit-offset))
		   ;;(print data-segment)
		   (setf res (bit-vector->uint data-segment))
		   (log:debug bit-offset next-bit-offset res)
		   (log:debug "Extracted: " data-segment)
		   )))
	  (values res next-bit-offset))))
