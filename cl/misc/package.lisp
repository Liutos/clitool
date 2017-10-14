(in-package #:cl)

(defpackage #:liutos.cli.misc
  (:use #:cl)
  (:export #:get-private-ip
           #:get-system-uptime
           #:get-uuid-short
           #:is-private-ip-p
           #:now
           #:snowflake))
