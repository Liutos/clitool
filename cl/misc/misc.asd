(asdf:defsystem #:liutos.cli.misc
  :version "0.0.1"
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (:ip-interfaces
               :split-sequence)
  :components ((:file "get-private-ip")
               (:file "misc")))

