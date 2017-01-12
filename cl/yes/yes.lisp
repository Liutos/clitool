(defun yes (&optional (string "y"))
  "Output a string repeatedly until killed"
  (loop
     (princ string)))
