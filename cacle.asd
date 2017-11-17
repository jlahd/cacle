(in-package #:cl)
(defpackage #:cacle-system (:use #:asdf #:cl))
(in-package #:cacle-system)

(asdf:defsystem cacle
  :name "cacle"
  :version "1.0.0"
  :author "Jussi Lahdenniemi <jussi@lahdenniemi.fi>"
  :maintainer "Jussi Lahdenniemi <jussi@lahdenniemi.fi>"
  :license "MIT"
  :description "Extensible cache services for Common Lisp"
  :encoding :utf-8
  :depends-on (bordeaux-threads)
  :components
  ((:module cacle
    :pathname "src"
    :components ((:file "package")
		 (:file "testsuite" :depends-on ("package"))
		 (:file "entry" :depends-on ("package" "testsuite"))
		 (:file "policy" :depends-on ("entry"))
		 (:file "simple-policies" :depends-on ("policy"))
		 (:file "lfu-policies" :depends-on ("policy"))
		 (:file "cache" :depends-on ("entry" "simple-policies"))))))
