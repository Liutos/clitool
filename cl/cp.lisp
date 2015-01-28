(in-package :common-lisp)

(defpackage liutos.cli.cp
  (:use :common-lisp)
  (:export :cp))

(in-package :liutos.cli.cp)

;;; cp: String|Pathname -> String|Pathname -> IO ()
(defun cp (from to)
  "Copy files and directories."
  (with-open-file (src from :element-type '(unsigned-byte 8))
    (with-open-file (dest to
                          :direction :output
                          :element-type '(unsigned-byte 8)
                          :if-does-not-exist :create
                          :if-exists :overwrite)
      (loop
         :for byte = (read-byte src nil)
         :while byte
         :do (write-byte byte dest)))))
