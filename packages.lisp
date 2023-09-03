(defpackage :xtce
  (:use :cl
		:cxml
		:filesystem-hash-table)
  (:documentation "XTCE")
  (:export
   #:format-bool
   #:format-number
   #:format-symbol
   #:hamming-distance
   #:hex-length-in-bits
   #:ldb-left
   #:make-absolute-time-parameter
   #:make-argument-instance-ref
   #:make-array-parameter-ref-entry
   #:make-array-parameter-type
   #:make-base-container
   #:make-binary-data-encoding
   #:make-binary-parameter-type
   #:make-comparison
   #:make-comparison-list
   #:make-container-ref-entry
   #:make-container-segment-ref-entry
   #:make-container-set
   #:make-count
   #:make-data-stream-set
   #:make-default-rate-in-stream
   #:make-dimension-list
   #:make-discrete-lookup-list
   #:make-dynamic-value
   #:make-encoding
   #:make-ending-index
   #:make-enumerated-parameter-type
   #:make-enumeration
   #:make-enumeration-alarm
   #:make-enumeration-list
   #:make-epoch
   #:make-fixed-frame-stream
   #:make-fixed-value
   #:make-float-data-encoding
   #:make-float-parameter-type
   #:make-include-condition
   #:make-indirect-parameter-ref-entry
   #:make-integer-data-encoding
   #:make-integer-parameter-type
   #:make-leading-size
   #:make-linear-adjustment
   #:make-location-in-container-in-bits
   #:make-long-description
   #:make-next-container
   #:make-off-set-from
   #:make-offset
   #:make-parameter
   #:make-parameter-instance-ref
   #:make-parameter-ref-entry
   #:make-parameter-segment-ref-entry
   #:make-parameter-set
   #:make-parameter-type-set
   #:make-polynomial-calibrator
   #:make-rate-in-stream
   #:make-reference-time
   #:make-repeat-entry
   #:make-restriction-criteria
   #:make-sequence-container
   #:make-size-in-bits
   #:make-size-range-in-characters
   #:make-space-system-list
   #:make-spline-point
   #:make-starting-index
   #:make-stream-segment-entry
   #:make-string-data-encoding
   #:make-string-parameter-type
   #:make-sync-pattern
   #:make-sync-strategy
   #:make-term
   #:make-term-list
   #:make-termination-char
   #:make-unit
   #:make-unit-set
   #:print-bin
   #:print-hex
   #:prompt-new-value
   #:truncate-from-left
   #:truncate-from-left-to-size
   #:make-telemetry-metadata
   #:make-space-system
   #:make-space-system-list
   #:make-dimension
   #:dump-xml
   #:instantiate-parameter
   ))

(defpackage :xtce-engine
  (:use :cl
		:xtce)
  (:documentation "XTCE-Engine")
  (:export #:STC.CCSDS.Space-Packet-Types))


(defpackage :standard-template-constructs
  (:use :cl
		:xtce)
  (:documentation "Standard Template Constructs")
  (:nicknames :stc)
  (:export
   #:CCSDS.Space-Packet.Header.Types
   #:CCSDS.Space-Packet.Header.Packet-Name
   #:CCSDS.Space-Packet.Header.Packet-Sequence-Count
   #:CCSDS.Space-Packet.Header.Packet-Data-Length-Type
   #:CCSDS.Space-Packet.Header.Application-Process-Identifier-Type
   #:CCSDS.Space-Packet.Header.Secondaty-Header-Flag-Type
   #:CCSDS.Space-Packet.Header.Packet-Version-Number-Type
   #:with-ccsds.space-packet.header.types))

(defpackage :nasa-cfs
  (:use :cl
		:xtce)
  (:nicknames :cfs)
  (:documentation "NASA-cFS")
  (:export))
