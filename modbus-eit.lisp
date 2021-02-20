(in-package #:cl-modbus)

;; !! Warning !!
;; To be implemented

(defun mei-type-valid-p (mei-type)
  (or (= mei-type #xd) (= mei-type #xe)))


(defun mei-pdu (data))

;;;;;;;;;;;;;;;;;;;
;; API Functions ;;
;;;;;;;;;;;;;;;;;;;

(defun encapsulated-interface-transport (gateway
					 slave-id
					 mei-type
					 mei-datas)
  "43 (0x2B) Encapsulated Interface Transport
   MEI (Modbus Encapsuled Interface) Type :
       0x0D or 0x0E - 1 byte
   MEI type specific data - n bytes"
  (when (mei-type-valid-p mei-type)
    (error 'cl-modbus-error :text
	   (format nil "Invalid MEI Type : ~a. Must be 0x0D or 0x0E."
		   mei-type))))

