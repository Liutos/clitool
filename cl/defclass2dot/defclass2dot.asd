(defsystem #:liutos.cli.defclass2dot
  :description "Convert DEFCLASS forms to codes in DOT language."
  :version "0.0.1"
  :author "Liutos <mat.liutos@gmail.com>"
  :components
  ((:file "defclass2dot"
          :depends-on ("package"))
   (:file "package")))
