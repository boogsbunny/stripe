(in-package #:stripe)

(define-object test-clock ()
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "test_helpers.test_clock"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (created
   :type local-time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (deleted
   :documentation "Indicates whether the object is deleted. Presence
indicates deletion.")
  (deletes-after
   :type local-time:timestamp
   :documentation "Time at which this clock is scheduled to auto
delete.")
  (frozen-time
   :type local-time:timestamp
   :documentation "Time at which all objects belonging to this clock
are frozen.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (name
   :type (or string null)
   :documentation "The custom name supplied at creation.")
  (status
   :type string
   :documentation "The status of the Test Clock. One of `advancing`,
`internal_failure`, or `ready`.")
  (status-details
   :type test-clock-status-details))

(define-object test-clock-status-details ()
  (advancing
   :type (or test-clock-status-details-advancing null)))

(define-object test-clock-status-details-advancing ()
  (target-frozen-time
   :type local-time:timestamp
   :documentation "The `frozen_time` that the Test Clock is advancing
towards."))

(defmethod initialize-instance :after ((instance test-clock) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created)
                 (decode-timestamp value)))
          (:deletes-after
           (when value
             (setf (slot-value instance '%deletes-after)
                   (decode-timestamp value))))
          (:frozen-time
           (when value
             (setf (slot-value instance '%frozen-time)
                   (decode-timestamp value))))
          (:status-details
           (when value
             (setf (slot-value instance '%status-details)
                   (make-instance 'test-clock-status-details :data value)))))))))

(defmethod initialize-instance :after ((instance test-clock-status-details)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:advancing
           (when value
             (setf (slot-value instance '%advancing)
                   (make-instance 'test-clock-status-details-advancing :data value)))))))))

(defmethod initialize-instance :after ((instance test-clock-status-details-advancing)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:target-frozen-time
           (when value
             (setf (slot-value instance '%target-frozen-time)
                   (decode-timestamp value)))))))))
