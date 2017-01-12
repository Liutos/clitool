(defun yes (&optional (string "y")
              &key version)
  "Output a string repeatedly until killed"
  (when version
    (princ "0.0.1")
    (return-from yes))
  (loop
     (princ string)))
