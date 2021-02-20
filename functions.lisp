(in-package #:cl-modbus)

(defparameter *modbus-function-table*
  '((#x1 "Read Coils" read-coils)
    (#x2 "Read Discrete Inputs" read-discrete-inputs)
    (#x3 "Read Holding Registers" read-holding-registers)
    (#x4 "Read Input Registers" read-input-registers)
    (#x5 "Write Single Coil" write-single-coil)
    (#x6 "Write Single Register" write-single-register)
    (#xf "Write Multiple Coils" write-multiple-coils)
    (#x10 "Write Multiple Registers" write-multiple-registers)
    (#x14 "Read File Record" read-file-record)
    (#x15 "Write File Record" write-file-record)
    (#x16 "Mask Write Register" mask-write-register)
    (#x17 "Read/Write Multiple Registers"
     read/write-multiple-registers)
    (#x18 "Read Fifo Queue" read-fifo-queue)
    (#x2b "Encapsulated Interface Transport"
     encapsulated-interface-transport))
  "MODBUS functions table. For each function name, define the modbus identification code. ")

(defun modbus-function (function-code)
  (assoc function-code *modbus-function-table*))

(defun modbus-function-string (function-code)
  (nth 1 (modbus-function function-code)))

(defun modbus-function-name (function-code)
  (nth 2 (modbus-function function-code)))


