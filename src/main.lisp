(ql:quickload "bifrost-integral")

(in-package :xtce)

(defparameter *unique-keys* ())

(cxml:with-xml-output (cxml:make-rod-sink :indentation 4 :canonical nil)
  (cxml-marshall (make-space-system "SpaceVechicle")))


(make-telemetry-metadata )
