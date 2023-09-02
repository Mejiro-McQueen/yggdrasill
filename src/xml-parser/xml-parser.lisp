(defparameter REE (dump-xml 
				   (make-parameter-set
					(make-parameter '|T1| '|T2|)
					(make-parameter '|T3| '|T4|))))

(print REE)

(defparameter *source* (cxml:make-source REE))


(klacks:peek *source*)
(klacks:peek-next *source*)
(multiple-value-call #'mode (klacks:peek-next *source*))

(defun r (namespace lname qname value explicit)
  (print "OK")
  (print namespace)
  (print lname)
  (print qname)
  (print value)
  (print explicit)
  (print (format t "REEEE~%"))
  'ok
  )

(defun m (uri lname qname)
  ;figure out type
  (print "Processing!")
  (print uri)
  (print lname)
  (print qname)
  (print "")

  ;extract 
  (klacks:peek *source*)
  (klacks:serialize-element *source* (cxml-xmls:make-xmls-builder))
  )

(defun mode (key &rest rest)
  (print key)
  ;; (print rest)
  (case key
	(:start-document
	 'Starting)
	(:comment
	 (print rest))
	(:start-element
	 (multiple-value-call #'m (values-list rest)))
	(:characters
	   (format nil "~a" rest))))



(string-trim '(#\Space #\Newline #\Backspace #\Tab 
									   #\Linefeed #\Page #\Return #\Rubout)
									 (first rest))
(print ree)

(klacks:consume *source*)
