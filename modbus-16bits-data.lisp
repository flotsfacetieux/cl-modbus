(in-package #:cl-modbus)

(defun registers-to-bytes (registers)
  (loop for register in registers
     collect (ldb (byte 16 8) register)
     collect (ldb (byte 8 0) register)))


;;;;;;;;;;;;;;;;;;;
;; API Functions ;;
;;;;;;;;;;;;;;;;;;;

(defun read-holding-registers (gateway
			       slave-id address quantity)
  "Return a list of registers values.
   (function code : 0x03).
   - gw : a modbus gateway object
   - slave-id :  the slave id to send request (1 byte)
   - address : the start adress to read coils (0x0000 to 0xFFFF - 2 bytes)
   - quantity : Quantity of inputs to read (1 to 125 (0x7D) - 2 bytes)"
  (when (not (data-valid-p quantity 1 #x7d))
    (error 'cl-modbus-error :text
	   (format nil"Invalid quantity : ~a. ~
                       Must be between 1 and 125." quantity)))
  (let* ((mb-fun-code 3)
	 (response
	  (query gateway slave-id
		 (common-pdu mb-fun-code
			    address
			    :data quantity)
		 'registers-data)))
    response))

(defun read-input-registers (gateway
			     slave-id address quantity)
  "Return a list of registers values.
   (function code : 0x04).
   - gw : a modbus gateway object
   - slave-id :  the slave id to send request (1 byte)
   - address : the start adress to read coils (0x0000 to 0xFFFF - 2 bytes)
   - quantity : Quantity of inputs to read (1 to 125 (0x7D) - 2 bytes)"
  (when (not (data-valid-p quantity 1 #x7d))
    (error 'cl-modbus-error :text
	   (format nil"Invalid quantity : ~a. ~
                       Must be between 1 and 125." quantity)))
  (let* ((mb-fun-code 4)
	 (response
	  (query gateway slave-id
		 (common-pdu mb-fun-code
			     address
			     :data quantity)
		 'registers-data)))
    response))

(defun write-single-register (gateway
			      slave-id address value)
  "Return true if the write is successfull.
   - gw : a modbus gateway object
   - slave-id :  the slave id to send request (1 byte)
   - address :0x0000 to 0xFFFF - 2 bytes
   - value : 0x0000 to 0xFFFF - 2 bytes"
  (when (not (data-valid-p value 0 #xffff))
    (error 'cl-modbus-error :text
	   (format nil"Invalid value : ~a. ~
                       Must be between 0 and 0xFFFF." value)))
  (let* ((mb-fun-code 6)
	 (response (query gateway slave-id
			  (common-pdu mb-fun-code
				      address
				      :data value)
			  'write-status)))
    (and (= address (car response))
	 (= value (cadr response)))))


(defun write-multiple-registers (gateway
				 slave-id address values)
  "Return true if the write is successfull.
   (function code : 0x10).
   - gw : a modbus gateway object
   - slave-id :  the slave id to send request (1 byte)
   - address : the start adress to write into registers (0x0000 to 0xFFFF - 2 bytes)
   - values : a list of registers values. Quantity 0x0001 to 0x07B"
  (let* ((mb-fun-code #x10)
	 (bytes (registers-to-bytes values))
	 (response (query gateway
			  slave-id
			  (common-pdu mb-fun-code
				      address
				      :data (length values)
				      :bytes bytes)
			  'write-status)))
    (and (= address (car response))
	 (= (length values) (cadr response)))))

(defun mask-write-register (gateway slave-id
			    address and-mask or-mask)
  "Return true if the request is successfull.
   (function code : 0x16).
   - gw : a modbus gateway object
   - slave-id :  the slave id to send request (1 byte)
   - address : 0x0000 to 0xFFFF - 2 bytes
   - and-mask : 0x0000 to 0xFFFF - 2 bytes
   - or-mask: 0x0000 to 0xFFFF - 2 bytes"
  (when (not (data-valid-p and-mask 0 #xffff))
    (error 'cl-modbus-error :text
	   (format nil"Invalid mask : ~a. ~
                       Must be between 0 and 0xFFFF." and-mask)))
  (when (not (data-valid-p or-mask 0 #xffff))
    (error 'cl-modbus-error :text
	   (format nil"Invalid mask : ~a. ~
                       Must be between 0 and 0xFFFF." or-mask)))
  (let* ((mb-fun-code #x16)
	 (bytes (registers-to-bytes (list and-mask or-mask)))
	 (response (query gateway
			  slave-id
			  (common-pdu mb-fun-code
				      address
				      :bytes bytes)
			  'write-status)))
  (and (= and-mask (car response))
       (= or-mask (cadr response)))))

(defun read/write-multiple-registers (gateway
				      slave-id
				      read-address
				      nb-read
				      write-address
				      nb-write
				      write-values)
  "Return a list of registers values.
23 (0x17) Read/Write Multiple registers
     Read Starting Address : 0x0000 to 0xFFFF - 2 Bytes
     Quantity to Read : 0x0001 to 0x007D - 2 Bytes
     Write Starting Address : 0x0000 to 0xFFFF - 2 Bytes
     Quantity to Write : 0x0001 to 0X0079 - 2 Bytes
     Write Byte Count - 1 Byte
     Write Registers Value - N*x 2 Bytes"
  (when (not (data-valid-p nb-read 1 #x7D))
    (error 'cl-modbus-error :text
	   (format nil"Invalid quantity of inputs read : ~a. ~
                       Must be between 1 and 0x7d." nb-read)))
  (when (not (addressp write-address))
    (error 'cl-modbus-error :text
	   (format nil"Invalid address : ~a. ~
                       Must be between 0 and 0xFFFF."
		   write-address)))
  (when (not (data-valid-p nb-write 1 #x79))
    (error 'cl-modbus-error :text
	   (format nil"Invalid quantity of writes : ~a. ~
                       Must be between 0 and 0x79." nb-write)))
  (when (not (= nb-write (length write-values)))
    (error 'cl-modbus-error :text
	   (format nil"Error. Declared ~a nb writes but found ~a."
		   nb-write (length write-values))))
  (let* ((mb-fun-code #x17)
	 (bytes (append (registers-to-bytes (list nb-read
						  write-address
						  nb-write))
			(list (length write-values))
			(registers-to-bytes write-values)))
	 (response (query gateway
			  slave-id
			  (common-pdu mb-fun-code
				      read-address
				      :bytes bytes)
			  'registers-data)))
    response))

(defun  read-fifo-queue (gateway slave-id address)
  "Return the content of the FIFO queue
   (function code 24 - 0x18)
   FIFO Pointer address : 0x0000 to 0xFFFF - 2 Bytes"
  (let* ((mb-fun-code #x18)
	 (response
	  (query gateway slave-id (common-pdu mb-fun-code
					      address)
		 'fifo-queue-data)))
    (cdr response)))
