(in-package #:stripe)

(define-object billing-alert ()
  "A billing alert is a resource that notifies you when a certain usage
threshold on a meter is crossed. For example, you might create a
billing alert to notify you when a certain user made 100 API requests."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "billing.alert"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (alert-type
   :type string
   :initform "usage_threshold"
   :documentation "Defines the type of the alert.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (status
   :type (or string null)
   :documentation "Status of the alert. This can be `active`,
`inactive`, or `archived`.")
  (title
   :type string
   :documentation "Title of the alert.")
  (usage-threshold
   :type (or billing-alert-usage-threshold null)
   :documentation "Encapsulates configuration of the alert to monitor
usage on a specific
[Billing Meter](https://stripe.com/docs/api/billing/meter)."))

(defmethod initialize-instance :after ((instance billing-alert) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:usage-threshold
           (unless (eql 'null value)
             (setf (slot-value instance '%usage-threshold)
                   (make-instance 'billing-alert-usage-threshold :data value)))))))))

(define-object billing-alert-usage-threshold ()
  (filters
   :type (or (vector billing-alert-usage-threshold-filter) null)
   :documentation "The filters allow limiting the scope of this usage
alert. You can only specify up to one filter at this time.")
  (gte
   :type number
   :documentation "The value at which this alert will trigger.")
  (meter
   :type (or string billing-meter)
   :documentation "The [Billing Meter]
(https://stripe.com/api/billing/meter) ID whose usage is monitored.")
  (recurrence
   :type string
   :initform "one_time"
   :documentation "Defines how the alert will behave."))

(defmethod initialize-instance :after ((instance billing-alert-usage-threshold)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:filters
           (unless (eql 'null value)
             (setf (slot-value instance '%filters)
                   (map 'vector
                        (lambda (filter)
                          (make-instance 'billing-alert-usage-threshold-filter
                                         :data filter))
                        value))))
          (:meter
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%meter)
                   (make-instance 'billing-meter :data value)))))))))

(define-object billing-alert-usage-threshold-filter ()
  (customer
   :type (or string customer null)
   :documentation "Limit the scope of the alert to this customer ID.")
  (type
   :reader filter-type
   :type string
   :initform "customer"
   :documentation "The type of the filter."))

(defmethod initialize-instance :after ((instance billing-alert-usage-threshold-filter)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:customer
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%customer)
                   (make-instance 'customer :data value)))))))))
