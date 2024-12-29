(in-package #:stripe)

(define-object billing-alert-triggered ()
  (object
   :type string
   :initform "billing.alert_triggered"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (alert
   :type billing-alert
   :documentation "A billing alert is a resource that notifies you
when a certain usage threshold on a meter is crossed. For example, you
might create a billing alert to notify you when a certain user made 100
API requests.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (customer
   :type string
   :documentation "ID of customer for which the alert triggered.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (value
   :type integer
   :documentation "The value triggering the alert."))

(defmethod initialize-instance :after ((instance billing-alert-triggered)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:alert
           (unless (eql 'null value)
             (setf (slot-value instance '%alert)
                   (make-instance 'billing-alert :data value))))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value))))))))
