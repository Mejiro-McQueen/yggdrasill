(in-package :standard-template-constructs)
(use-package :xtce)


(defun boolean-flag ()
  (make-binary-data-encoding (make-size-in-bits (make-fixed-value 1))))
