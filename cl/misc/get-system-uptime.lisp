(in-package #:liutos.cli.misc)

(defun get-system-uptime ()
  "Return the UNIX timestamp answering when system was up.

Use the external program `who` and its `-b` option to get a string
describing the system's uptime, and parse this string to generate a
coresponding UNIX timestamp."
  (let* ((output (with-output-to-string (s)
                   (uiop:run-program "who -b" :output s)))
         (parts (nth-value 1
                           (cl-ppcre:scan-to-strings "(\\d{4})-(\\d{2})-(\\d{2}) (\\d{2}):(\\d{2})" output)))
         (year (parse-integer (elt parts 0)))
         (month (parse-integer (elt parts 1)))
         (day (parse-integer (elt parts 2)))
         (hour (parse-integer (elt parts 3)))
         (minute (parse-integer (elt parts 4)))
         (timestamp (local-time:encode-timestamp
                     0 0 minute hour day month year)))
    (local-time:timestamp-to-unix timestamp)))
