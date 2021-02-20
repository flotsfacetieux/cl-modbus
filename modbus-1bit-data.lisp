(in-package #:cl-modbus)

(defun coil-code (value)
  (if value #xff00 0))

(defun bytes-to-data (bytes quantity)
  "Return a list of true/false values which represents bytes of 8 bits.
Values is a list of bytes.
Bits are stored in the following way : the first bit is stored in the lower bit of the byte (rightmost position).
Example : 
 - input '(#b01010111 #b00000011)
 - output '(t t t nil t nil t nil t t)
"
  (let ((data '()))
    (dolist (byte bytes)
      (loop for index from 0 to 7
	 do (push (= 1 (logand byte 1)) data)
	 do (setf byte (ash byte -1))))
    (subseq (reverse data) 0 quantity)))

(defun bits-to-integer (bits)
  "Return an integer from a vector of bits."
  (reduce #'(lambda (a b)
	      (+ b (ash a 1))) bits))


(defun data-to-bytes (values)
  "Return a list of bytes of 8 bits which represents the values list.
Values is a list of True Nil value.
Bits are stored in the following way : the first bit is stored in the lower bit of the byte (rightmost position).
Example : 
 - input '(t t t nil t nil t nil t t)
 - output '(#b01010111 #b00000011)"
  ;; values : list of true false values
  (let ((bytes ())
	(bindex 7)
	(bits (make-array 8
			  :element-type 'bit 
			  :initial-element 0)))
    (dolist (val values)
      (setf (sbit bits bindex) (if val 1 0))
      (decf bindex)
      (when (< bindex 0)
	(push (bits-to-integer bits) bytes)
	(setf bindex 7)
	(setf bits (make-array 8
			       :element-type 'bit 
			       :initial-element 0))))
    (when (not (= bindex 7))
      (push (bits-to-integer bits) bytes))
    (reverse bytes)))

;;;;;;;;;;;;;;;;;;;;
;; API Functions  ;;
;;;;;;;;;;;;;;;;;;;;

(defun read-coils (gateway
		   slave-id address quantity)
  "Return a list of true/false values from a read-coils modbus request.
   - gateway : a modbus gateway object
   - slave-id :  the slave id to send request (1 byte)
   - address : the start adress to read coils (0x0000 to 0xFFFF - 2 bytes)
   - quantity : nb coils to read (1 to 2000 (0x7D0) - 2 bytes)"
  (when (not (data-valid-p quantity 1 #x7d0))
    (error 'cl-modbus-error :text "Invalid value"))
  (let* ((mb-fun-code 1)
	 (response
	  (query gateway slave-id
		 (common-pdu mb-fun-code
			     address
			     :data quantity)
		 'coils-data)))
    (bytes-to-data response quantity)))

(defun read-discrete-inputs (gateway
			     slave-id address quantity)
  "Return a list of true/false values.
   (function code : 0x02).
   - gw : a modbus gateway object
   - slave-id :  the slave id to send request (1 byte)
   - address : the start adress to read coils (0x0000 to 0xFFFF - 2 bytes)
   - quantity : Quantity of inputs to read (1 to 2000 (0x7D0) - 2 bytes)"
  (when (not (data-valid-p quantity 1 #x7d0))
    (error 'cl-modbus-error :text "Invalid value"))
  (let* ((mb-fun-code 2)
	 (response
	  (query gateway slave-id
		 (common-pdu mb-fun-code
			     address
			     :data quantity)
		 'coils-data)))
    (bytes-to-data response quantity)))

(defun write-single-coil (gateway
			  slave-id address data)
  "Return true if the write is successfull.
   - gw : a modbus gateway object
   - slave-id :  the slave id to send request (1 byte)
   - address : (0x0000 to 0xFFFF - 2 bytes)
   - data : data to write (t or nil)"
  (when (not (typep data 'boolean))
    (error 'cl-modbus-error :text "Invalid value"))
  (let* ((mb-fun-code 5)
	 (coil-status (coil-code data))
	 (response (query gateway slave-id
			  (common-pdu mb-fun-code
				      address
				      :data coil-status)
			  'write-status)))
    (and (= address (car response))
	 (= coil-status (cadr response)))))


(defun write-multiple-coils (gateway
			     slave-id
			     address
			     values)
  "Return true if the write is successfull.
   (function code : 0x0F).
   - gw : a modbus gateway object
   - slave-id :  the slave id to send request (1 byte)
   - address : the start adress to write coils (0x0000 to 0xFFFF - 2 bytes)
   - values : a list of true nil values. Quantity 0x0001 to 0x07B0"
  (let* ((mb-fun-code #xf)
	 (coils (data-to-bytes values))
	 (quantity (length values)))
    (when (not (data-valid-p quantity 1 #x7b0))
      (error 'cl-modbus-error :text "Invalid value"))
    (let ((response (query gateway slave-id
			   (common-pdu
			    mb-fun-code
			    address
			    :data quantity
			    :bytes (cons (length coils)
					 coils))
			   'write-status)))
      (and (= address (car response))
	   (= (length values) (cadr response))))))

