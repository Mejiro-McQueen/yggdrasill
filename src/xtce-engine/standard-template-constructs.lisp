(in-package :standard-template-constructs)
(use-package :xtce)


(defun boolean-flag ()
  (make-binary-data-encoding (make-size-in-bits (make-fixed-value 1))))






;; (defun CCSDS.MPDU.Container ()
;;   (make-sequence-container '|STC.CCSDS.MPDU.Container|
;; 						   (list
;; 							(make-parameter-ref-entry '|CCSDS.MPDU.Header.Reserved-Spare|)
;; 							(make-parameter-ref-entry '|CCSDS.MPDU.Header.First-Header-Pointer|)
;; 							(make-container-ref-entry '|CCSDS.Space-Packet.Header|)
							
;; 							)))
