(asdf:defsystem #:gnu-find
  :version "0.0.1"
  :author "Liutos <mat.liutos@gmail.com>"
  :depends-on (#:split-sequence)
  :serial t
  :components ((:file "package")
               (:file "main")))

