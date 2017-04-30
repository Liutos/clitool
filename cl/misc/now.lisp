(in-package #:liutos.cli.misc)

(defgeneric now (precision)
  (:documentation "Returns the current time interval in second, millisecond, or microsecond precision."))

(defmethod now ((precision (eql :second)))
  "Returns the number of seconds elapsed since 1 January 1970 00:00:00 UTC."
  (local-time:timestamp-to-unix (local-time:now)))

(defmethod now ((precision (eql :millisecond)))
  "Returns the number of milliseconds elapsed since 1 January 1970 00:00:00 UTC."
  (let* ((now (local-time:now))
         (seconds (local-time:timestamp-to-unix now))
         (milliseconds (local-time:timestamp-millisecond now)))
    (+ (* 1000 seconds) milliseconds)))

(defmethod now ((precision (eql :microsecond)))
  "Returns the number of microseconds elapsed since 1 January 1970 00:00:00 UTC."
  (let* ((now (local-time:now))
         (seconds (local-time:timestamp-to-unix now))
         (microsecond (nth-value 1 (local-time:timestamp-millisecond now))))
    (+ (* 1000000 seconds) microsecond)))
