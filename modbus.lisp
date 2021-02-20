(in-package #:cl-modbus)

(defparameter *modbus-max-pdu-size* 253)

(defun make-modbus-gateway (host port mode)
  "Return a modbus gateway object which to send requests.
   - host : ip address of the gateway.
   - port : TCP port whivh the gateway listen to.
   - mode : must be :tcp or :rtu.
   TCP mode is part of the MODBUS protocol.
   RTU mode is 'RTU over TCP'."
  (case mode
    (:rtu (make-instance 'modbus-rtu-gateway
			 :ip-address host
			 :port port))
    (:tcp (make-instance 'modbus-tcp-gateway
			 :ip-address host
			 :port port))
    (otherwise (error 'cl-modbus-error :text
		      (format nil "Invalid gateway mode : ~a. Muste be :rtu or :tcp" mode)))))

;; Slaves

(defun slave-id-p (slave-id)
  "Return true if the slave-id value is valid, i.e. between 0 and 247. See MODBUS protocol."
  (and (numberp slave-id) (>= slave-id 0) (<= slave-id 247)))

;; Check functions
(defun addressp (address)
  "Return true if the address value is valid, i.e. between 0 and 0xFFFF. See MODBUS protocol."
  (and (numberp address) (>= address 0) (<= address #xFFFF)))

(defun data-valid-p (data min max)
  "Return true if the data value is into the range.
   See MODBUS protocol."
  (and (>= data min) (<= data max)))

(defun pdu-size-valid-p (pdu)
  "Return true if the pdu size is valid, i.e. lower then 253.
   See MODBUS protocol."
  (< (length pdu) *modbus-max-pdu-size*))

;; PDU

(defun common-pdu (function-code address &key data bytes)
  "Return an array of bytes, a valid pdu modbus message.
   - Function code : 1 byte. Must be a valid function-code.
   - Address : 2 bytes
   - data : 2 bytes
   - bytes : an array of bytes"
  (when (not (addressp address))
    (format t "address = ~a~%" address)
    (error 'cl-modbus-error :text
	   (format nil "Invalid data adress : ~a" address)))
  (let* ((pdu (make-array (+ 5 (if bytes (length bytes) 0))
			  :element-type '(unsigned-byte 8)
			  :fill-pointer 0)))
    (vector-push function-code pdu)
    (vector-push (ldb (byte 16 8) address) pdu)
    (vector-push (ldb (byte 8 0) address) pdu)
    (when data
      (vector-push (ldb (byte 16 8) data) pdu)
      (vector-push (ldb (byte 8 0) data) pdu))
    (dolist (byte bytes)
      (when (not (= byte (ldb (byte 8 0) byte)))
	(error 'cl-modbus-error :text
	       (format nil "Invalid data value : ~a" data)))
      (vector-push (ldb (byte 8 0) byte) pdu))
    pdu))


