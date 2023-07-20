(ql:quickload "bifrost-integral")
(declaim (optimize (debug 3)))

(in-package :xtce)

(defparameter *ROOT*
  (make-space-system
   "SpaceVechicle"
   :telemetry-metadata
   (make-telemetry-metadata
    :parameter-type-set
    (make-parameter-type-set
     (make-integer-parameter-type
      'IDType
      :encoding-type
      (make-integer-data-encoding)
	  :signed nil)
     (make-integer-parameter-type
	  'SecHType
	  :signed nil
	  :encoding-type
	  (make-integer-data-encoding :size-in-bits 1))
	 (make-integer-parameter-type
	  'TypeType
	  :signed nil
	  :encoding-type
	  (make-integer-data-encoding :size-in-bits 1))
	 (make-integer-parameter-type
	  'LengthType
	  :signed nil
	  :encoding-type
	  (make-integer-data-encoding :size-in-bits 16))))))


(dump-space-system-xml *ROOT*)
