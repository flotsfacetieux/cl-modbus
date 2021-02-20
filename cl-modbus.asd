(defsystem "cl-modbus"
    :name "CL Modbus"
    :version "0.0.1"
    :maintainer "Flots Facetieux"
    :author "Flots Facetieux"
    :licence "GPL v3"
    :serial t
    :description "Modbus Library."
    :depends-on (:usocket :usocket-server :lisp-unit)
    :components ((:file "package")
		 (:file "conditions")
		 (:file "functions")
		 (:file "crc")
		 (:file "responses")
		 (:file "network")
		 (:file "modbus")
		 (:file "modbus-1bit-data")
		 (:file "modbus-16bits-data")
		 (:module test
			  :components
			  ((:file "server")
			   (:file "tests")))))
