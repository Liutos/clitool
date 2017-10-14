(asdf:defsystem #:liutos.cli.misc
  :version "0.0.1"
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (#:cl-ppcre
               :ip-interfaces
               :local-time
               :split-sequence)
  :serial t
  :components ((:file "package")
               (:file "get-private-ip")
               (:file "misc")
               (:file "now")
               (:file "snowflake")))

