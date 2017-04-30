(in-package #:liutos.cli.misc)

(defun ip-string-to-vector (ip-string)
  "Convert a string contains an IP address to a vector contains for integers."
  (check-type ip-string string)
  (map 'vector
       #'(lambda (ns)
           (parse-integer ns))
       (split-sequence:split-sequence #\. ip-string)))

(defun ip-vector-to-number (ip-vector)
  "Convert a vector contains four numbers between 0 and 255 to an integer."
  (check-type ip-vector vector)
  (reduce #'(lambda (n part)
              (+ (* 255 n) part))
          ip-vector
          :initial-value 0))

(defvar *private-ip-address-ranges*
  (mapcar #'(lambda (range)
              (destructuring-bind (min max) range
                (list (ip-vector-to-number (ip-string-to-vector min))
                      (ip-vector-to-number (ip-string-to-vector max)))))
          '(("10.0.0.0" "10.255.255.255")
            ("172.16.0.0" "172.31.255.255")
            ("192.168.0.0" "192.168.255.255"))))

(defgeneric is-private-ip-p (ip-address))

(defmethod is-private-ip-p ((ip-address integer))
  "Return T if IP-ADDRESS indicates a private IP address."
  (dolist (range *private-ip-address-ranges*)
    (destructuring-bind (min max) range
      (when (and (<= min ip-address)
                 (<= ip-address max))
        (return-from is-private-ip-p t))))
  nil)

(defmethod is-private-ip-p ((ip-address string))
  "Return T if IP-ADDRESS indicates a private IP address."
  (let ((ip-address (ip-string-to-vector ip-address)))
    (is-private-ip-p ip-address)))

(defmethod is-private-ip-p ((ip-address vector))
  "Return T if IP-ADDRESS indicates a private IP address."
  (let ((ip-address (ip-vector-to-number ip-address)))
    (is-private-ip-p ip-address)))
