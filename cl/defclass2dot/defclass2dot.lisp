(in-package #:liutos.cli.defclass2dot)

(defun defclass2dot (form
                     &key (output *standard-output*))
  "Convert FORM, which is a DEFCLASS form, to codes in DOT language."
  (let ((name (nth 1 form))
        (direct-superclasses (format nil "~{~A~^,~}" (nth 2 form)))
        (direct-slots (mapcar #'first (nth 3 form))))
    (format output "digraph g {~%")
    (format output "    node0[label = \"~A | ~A |~{ ~A~^ |~}\", shape = \"record\"];~%"
            name
            direct-superclasses
            direct-slots)
    (format output "}~%")))
