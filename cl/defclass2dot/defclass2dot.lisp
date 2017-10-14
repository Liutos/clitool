(in-package #:liutos.cli.defclass2dot)

(defparameter *defclass-template*
  "
<TABLE>
  <TR><TD><!-- TMPL_VAR name --></TD></TR>
  <!-- TMPL_LOOP direct-superclasses -->
  <TR><TD><!-- TMPL_VAR name --></TD></TR>
  <!-- /TMPL_LOOP -->
  <!-- TMPL_LOOP direct-slots -->
  <TR><TD><!-- TMPL_VAR name --></TD></TR>
  <!-- /TMPL_LOOP -->
</TABLE>")

(defun defclass2label (form)
  (let ((name (nth 1 form))
        (direct-superclasses (nth 2 form))
        (direct-slots (nth 3 form)))
    (declare (ignorable direct-superclasses direct-slots))
    (with-output-to-string (output)
      (html-template:fill-and-print-template
       *defclass-template*
       (list :name name
             :direct-superclasses (mapcar #'(lambda (name)
                                              `(:name ,name))
                                          direct-superclasses)
             :direct-slots (mapcar #'(lambda (slot)
                                       `(:name ,(first slot)))
                                   direct-slots))
       :stream output))))

(defun remove-newline (str)
  (let ((parts (split-sequence #\Newline str)))
    (apply #'concatenate 'string parts)))

(defun defclass2dot (form
                     &key (output *standard-output*))
  "Convert FORM, which is a DEFCLASS form, to codes in DOT language."
  (let* ((label (defclass2label form))
         (label (remove-newline label)))
    (format output "digraph g {~%")
    (format output "    node0[label = <~A>, shape = \"record\"];~%" label)
    (format output "}~%")))
