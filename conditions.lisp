(in-package #:cl-modbus)

;; CL-MODBUS API Errors

(define-condition cl-modbus-error (error)
  ((text :initarg :text :reader modbus-error-text))
  (:report (lambda (condition stream)
	     (if (slot-boundp condition 'text)
		 (format stream "MODBUS ERROR : ~a"
			 (modbus-error-text condition))
		 (format stream "MODBUS ERROR"))))
  (:documentation ""))

;; (define-condition illegal-exception-code-error (cl-modbus-error) ())

;; (define-condition gateway-connexion-error (cl-modbus-error) ())

;; (define-condition unknown-function-code-error (cl-modbus-error) ())

;; (define-condition illegal-slave-id-error (cl-modbus-error) ())

;; (define-condition modbus-timeout-error (cl-modbus-error) ())

;; (define-condition pdu-size-exceeded-error (cl-modbus-error)
;;   ()
;;   (:documentation
;;    "Signaled when a modbus PDU exceeded max size")) 

(define-condition illegal-gateway-mode-error (cl-modbus-error) ())

;; MODBUS protocol defined errors

(define-condition modbus-response-error (cl-modbus-error)
  ()
  (:report (lambda (condition stream)
	     (if (slot-boundp condition 'text)
		 (format stream "MODBUS RESPONSE ERROR : ~a"
			 (modbus-error-text condition))
		 (format stream "MODBUS RESPONSE ERROR"))))
  (:documentation "MODBUS Response condition"))

(defparameter *modbus-exception-table*
  '((1 . "illegal function")
    (2 . "illegal data address")
    (3 . "illegal data value")
    (4 . "server device failure")
    (5 . "acknowledge")
    (6 . "server device busy")
    (8 . "memory parity")
    (#xa . "gateway path unavailable")
    (#xb . "gateway target device failed to response")))

(defun modbus-exception-name (exception-code)
  (cdr (assoc exception-code *modbus-exception-table*)))

(defun raise-exception (function-code exception-code)
  (let ((function-name (modbus-function-name
			(logand #x7F function-code))))
    (if (not function-name)
	(error 'modbus-response-error
	       :text (format nil "Unknown modbus function : ~a"
			     function-code))
	(let ((exception-name (or (modbus-exception-name
				   exception-code)
				  'illegal-exception-code-error)))
	  
	  (error 'modbus-response-error
		 :text
		 (if exception-name
		     (format nil "Error Function : ~a - ~a"
			     function-name exception-name)
		     (format
		      nil
		      "Error Function : ~a - Unknown error code : ~a"
		      function-name exception-code)))))))
