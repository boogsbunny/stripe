(in-package #:stripe)

(define-object billing-meter ()
  "A billing meter is a resource that allows you to track usage of a
particular event. For example, you might create a billing meter to
track the number of API calls made by a particular user. You can then
attach the billing meter to a price and attach the price to a
subscription to charge the user for the number of API calls they make.

Related guide: [Usage based billing](https://docs.stripe.com/billing/subscriptions/usage-based)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "billing.meter"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created in Unix epoch seconds.")
  (customer-mapping
   :type (or billing-meter-customer-mapping null))
  (default-aggregation
   :type billing-meter-default-aggregation)
  (display-name
   :type string
   :documentation "The meter's name.")
  (event-name
   :type string
   :documentation "The name of the meter event to record usage for.
Corresponds with the `event_name` field on meter events.")
  (event-time-window
   :type (or string null)
   :documentation "The time window to pre-aggregate meter events for,
if any. One of `day` or `hour`.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (status
   :type string
   :documentation "The meter's status. One of `active` or `inactive`.")
  (status-transitions
   :type billing-meter-status-transitions)
  (updated
   :type time:timestamp
   :documentation "Time at which the object was last updated. Measured
in seconds since the Unix epoch.")
  (value-settings
   :type billing-meter-value-settings))

(defmethod initialize-instance :after ((instance billing-meter) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:customer-mapping
           (unless (eql 'null value)
             (setf (slot-value instance '%customer-mapping)
                   (make-instance 'billing-meter-customer-mapping :data value))))
          (:default-aggregation
           (unless (eql 'null value)
             (setf (slot-value instance '%default-aggregation)
                   (make-instance 'billing-meter-default-aggregation :data value))))
          (:status-transitions
           (unless (eql 'null value)
             (setf (slot-value instance '%status-transitions)
                   (make-instance 'billing-meter-status-transitions :data value))))
          (:updated
           (setf (slot-value instance '%updated) (decode-timestamp value)))
          (:value-settings
           (unless (eql 'null value)
             (setf (slot-value instance '%value-settings)
                   (make-instance 'billing-meter-value-settings :data value)))))))))

(define-object billing-meter-customer-mapping ()
  (event-payload-key
   :type string
   :documentation "The key in the meter event payload for customer
mapping.")
  (type
   :reader customer-mapping-type
   :type string
   :initform "by_id"
   :documentation "The method for mapping a meter event to a customer."))

(define-object billing-meter-default-aggregation ()
  (formula
   :type string
   :documentation "Specifies how events are aggregated. One of `count`
or `sum`."))

(define-object billing-meter-status-transitions ()
  (deactivated-at
   :type (or time:timestamp null)
   :documentation "The time the meter was deactivated, if any, in Unix
epoch seconds."))

(defmethod initialize-instance :after ((instance billing-meter-status-transitions)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:deactivated-at
           (setf (slot-value instance '%deactivated-at) (decode-timestamp value))))))))

(define-object billing-meter-value-settings ()
  (event-payload-key
   :type string
   :documentation "The key in the meter event payload to use as the
value for this meter."))
