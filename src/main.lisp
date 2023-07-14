(ql:quickload "bifrost-integral")
(declaim (optimize (debug 3)))

(in-package :xtce)

(defparameter *unique-keys* ())




(defparameter *ROOT* (make-space-system "SpaceVechicle"
                                        :telemetry-metadata (make-telemetry-metadata :parameter-type-set (make-parameter-type-set (make-integer-parameter-type 'IDType
                                                                                                                                                               :encoding-type (make-integer-data-encoding))))))




(dump-space-system-xml *ROOT*)
