(ql:quickload "bifrost-integral")
(declaim (optimize (speed 0) (space 0) (debug 3)))

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
      :data-encoding
      (make-integer-data-encoding)
	  :signed nil)
     (make-integer-parameter-type
	  'SecHType
	  :signed nil
	  :data-encoding
	  (make-integer-data-encoding :size-in-bits 1))
	 (make-integer-parameter-type
	  'TypeType
	  :signed nil
	  :data-encoding
	  (make-integer-data-encoding :size-in-bits 1))
	 (make-integer-parameter-type
	  'LengthType
	  :signed nil
	  :data-encoding
	  (make-integer-data-encoding :size-in-bits 16))
	 (make-enumerated-parameter-type
	  'PSWHLTIMFLGType
	  :data-encoding
	  (make-integer-data-encoding :size-in-bits 16)
	  :enumeration-list
	  (make-enumeration-list
	   (make-enumeration 'TIMER_OFF 0)
	   (make-enumeration 'TIMER_ON 1)
	   (make-enumeration 'TIMER_COMPLETED 2)))

	 (make-float-parameter-type
	  'PBATMTEMPTYPE
	  :size-in-bits 64
	  :unit-set
	  (make-unit-set
	   (make-unit
		:description "Bq"
		:form "units:Becquerel"))
	  :data-encoding
	  (make-integer-data-encoding
	   :size-in-bits 16
	   :encoding 'twos-complement
	   :default-calibrator
	   (make-polynomial-calibrator
		:term-list
		(make-xtce-list 'term nil 
						(list (make-term :coefficient -7459.23273708 :exponent 0)
							  (make-term :coefficient 8.23643519148 :exponent 1)
							  (make-term :coefficient -3.02185061876e3 :exponent 2)
							  (make-term :coefficient 2.33422429056e-7 :exponent 3)
							  (make-term :coefficient 5.67189556173e11 :exponent 4))))))

	 (make-absolute-time-parameter
	   'MISSIONTIMETYPE
	   :reference-time
	   (make-reference-time
		(make-offset-from 'SECONDS)))

	 (make-absolute-time-parameter
	   'SECONDSTYPE
	   :encoding
	   (make-encoding
		:units "seconds"
		:data-encoding
		(make-integer-data-encoding :size-in-bits 32))
	   :reference-time
	   (make-reference-time
		(make-offset-from 'MILLISECONDS)))

	 (make-absolute-time-parameter
	   'MILLISECONDS
	   :encoding
	   (make-encoding
		:units "seconds"
		:scale 0.001
		:data-encoding
		(make-integer-data-encoding :size-in-bits 16))
	   :reference-time
	   (make-reference-time
		(make-epoch 'TAI)))
	 
	 ))))

(dump-space-system-xml *ROOT*)




