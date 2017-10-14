(in-package #:liutos.cli.misc)

(defvar *incremented-variable* 0)

(defun get-uuid-short ()
  "Generate a 64-bit unique number.

Use the algorithm from this page:
https://dev.mysql.com/doc/refman/5.7/en/miscellaneous-functions.html#function_uuid-short."
  (let ((server-id (random (1- (ash 2 31))))
        (server-start-time-in-seconds (get-system-uptime)))
    (+ (ash (logand server-id 255) 56)
       (ash server-start-time-in-seconds 24)
       (incf *incremented-variable*))))
