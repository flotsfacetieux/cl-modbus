(in-package #:cl-modbus)

(defclass modbus-gateway ()
  ((ip-address :accessor gw-ip-address
	       :initarg :ip-address)
   (port :accessor gw-port
	 :initarg :port)
   (timeout :accessor modbus-timeout
	    :initform 2)
   (stream :accessor gw-stream)))

(defclass modbus-rtu-gateway (modbus-gateway) ())
(defclass modbus-tcp-gateway (modbus-gateway)
  ((transaction-identifier :initform 0)))

(defmethod gw-transaction-id ((gw modbus-tcp-gateway))
  (incf (slot-value gw 'transaction-identifier))
  (1- (slot-value gw 'transaction-identifier)))


;; Application Data Unit

(defmethod request-adu ((gw modbus-rtu-gateway) slave-id pdu)
  "Return an array of bytes, a valid adu modbus message.
   ADU for modbus rtu over tcp is : slave-id + pdu + crc (slave-id + pdu).
   - gw : a modbus gateway object
   - slave-id :  the slave id to send request
   - pdu : the pdu part of the request"
  (when (not (slave-id-p slave-id))
    (error 'cl-modbus-error :text
	   (format nil "Invalid Slave ID (0 <= id <= 247): ~a"
		   slave-id)))
  (let ((adu (make-array (1+ (length pdu))
			 :element-type '(unsigned-byte 8)
			 :fill-pointer 0
			 :adjustable t)))
    (vector-push slave-id adu)
    (dotimes (index (length pdu))
      (vector-push (aref pdu index) adu))
    (let ((crc (modbus-crc adu)))
      (vector-push-extend (ldb (byte 8 0) crc) adu)
      (vector-push-extend (ldb (byte 16 8) crc) adu))
    adu))

(defmethod request-adu ((gw modbus-tcp-gateway) slave-id pdu)
  "Return an array of bytes, a valid adu modbus message.
   ADU for modbus TCP is : mbap-header + pdu.
   - gw : a modbus gateway object
   - slave-id :  the slave id to send request
   - pdu : the pdu part of the request"
  (when (not (slave-id-p slave-id))
    (error 'cl-modbus-error :text
	   (format nil "Invalid Slave ID (0 <= id <= 247): ~a"
		   slave-id)))
  (concatenate 'vector (request-header gw slave-id pdu) pdu))

;; Response Headers
(defparameter *rtu-header-response-size* 2)
(defparameter *mbap-header-response-size* 7)

(defun read-from-stream (stream nb-bytes)
  (loop repeat nb-bytes
     collect (read-byte stream)))

(defmethod response-header ((gw modbus-rtu-gateway) stream)
  (read-from-stream stream *rtu-header-response-size*))

(defmethod response-header ((gw modbus-tcp-gateway) stream)
  (read-from-stream stream *mbap-header-response-size*))

;; Request Headers
(defmethod request-header ((gw modbus-rtu-gateway) slave-id pdu)
  (declare (ignore pdu))
  (ldb (byte 8 0) slave-id))

(defmethod request-header ((gw modbus-tcp-gateway) slave-id pdu)
  (let ((transaction-identifier (gw-transaction-id gw)))
    (list
     (ldb (byte 16 8) transaction-identifier)
     (ldb (byte 8 0) transaction-identifier)
     0 0
     (ldb (byte 16 8) (1+ (length pdu)))
     (ldb (byte 8 0) (1+ (length pdu)))
     (ldb (byte 8 0) slave-id))))

;; 
(defun write-to-stream (adu stream)
  (dotimes (index (length adu))
    (write-byte (aref adu index) stream))
  (force-output stream))

(defun wait-for-response (gw socket)
  (when (not (usocket:wait-for-input socket
				     :timeout (modbus-timeout gw)
				     :ready-only t))
    (error 'cl-modbus-error :text "Network Timeout")))
;; RTU Only
(defun exception-p (function-code)
  (>= function-code #x80))

(defun exception-code (stream)
  (read-byte stream))

;; Response
(defmethod response ((gw modbus-rtu-gateway)
		     response-function stream)
  (let* ((header (response-header gw stream))
	 (function-code (nth 1 header)))
    (when (exception-p function-code)
      (raise-exception function-code (exception-code stream)))
    (funcall response-function stream)))

(defmethod response ((gw modbus-tcp-gateway)
		     response-function stream)
  (let* ((header (response-header gw stream))
	 (function-code (read-byte stream)))
    (when (exception-p function-code)
      (raise-exception function-code (exception-code stream)))
    (funcall response-function stream)))

;; Request

(defmethod query ((gateway modbus-rtu-gateway) slave-id
	      pdu response-function)
  (let ((adu (request-adu gateway slave-id pdu)))
    (handler-case
	(usocket:with-client-socket
	    (socket stream 
		    (gw-ip-address gateway)
		    (gw-port gateway)
		    :element-type 'unsigned-byte
		    :timeout 3)
	  (write-to-stream adu stream)
	  (wait-for-response gateway socket)
	  (response gateway response-function stream))
      (usocket:connection-refused-error
	  ()
	(error 'cl-modbus-error
	       :text
	       "Impossible de joindre la passerelle ~
                - Connexion refusée"))
      (usocket:timeout-error
	  ()
	(error 'cl-modbus-error
	       :text
	       "Impossible de joindre la passerelle ~
                - Connexion timeout")))))

(defmethod query ((gateway modbus-tcp-gateway) slave-id
		  pdu response-function)
  ;; un truc à faire avec le "transaction id" 
  (let ((adu (request-adu gateway slave-id pdu)))
    (handler-case
	(usocket:with-client-socket
	    (socket stream 
		    (gw-ip-address gateway)
		    (gw-port gateway)
		    :element-type 'unsigned-byte
		    :timeout 3)
	  (write-to-stream adu stream)
	  (wait-for-response gateway socket)
	  (response gateway response-function stream))
      (usocket:connection-refused-error
	  ()
	(error 'cl-modbus-error
	       :text
	       "Impossible de joindre la passerelle ~
                - Connexion refusée"))
      (usocket:timeout-error
	  ()
	(error 'cl-modbus-error
	       :text
	       "Impossible de joindre la passerelle ~
                - Connexion timeout")))))
