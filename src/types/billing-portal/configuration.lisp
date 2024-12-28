(in-package #:stripe)

(define-object billing-portal-configuration ()
  "A portal configuration describes the functionality and behavior of
a portal session."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "billing_portal.configuration"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (active
   :type boolean
   :documentation "Whether the configuration is active and can be used
to create portal sessions.")
  ;; TODO:
  (application
   :type (or string application deleted-application null)
   :documentation "ID of the Connect Application that created the
configuration.")
  (business-profile
   :type configuration-business-profile)
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (default-return-url
   :type (or string null)
   :documentation "The default URL to redirect customers to when they
click on the portal's link to return to your website. This can be
[overriden](https://stripe.com/docs/api/customer_portal/sessions/create#create_portal_session-return_url)
when creating the session.")
  (features
   :type configuration-features)
  (is-default
   :type boolean
   :documentation "Whether the configuration is the default. If `true`,
this configuration can be managed in the Dashboard and portal sessions
will use this configuration unless it is overriden when creating the
session.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (login-page
   :type configuration-login-page)
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (updated
   :type time:timestamp
   :documentation "Time at which the object was last updated. Measured
in seconds since the Unix epoch."))

(defmethod initialize-instance :after ((instance billing-portal-configuration)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:application
           (unless (eql 'null value)
             (setf (slot-value instance '%application)
                   (make-instance (if (gethash :deleted value)
                                      'deleted-application
                                      'application)
                                  :data value))))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:updated
           (setf (slot-value instance '%updated) (decode-timestamp value)))
          (:business-profile
           (unless (eql 'null value)
             (setf (slot-value instance '%business-profile)
                   (make-instance 'configuration-business-profile :data value))))
          (:features
           (unless (eql 'null value)
             (setf (slot-value instance '%features)
                   (make-instance 'configuration-features :data value))))
          (:login-page
           (unless (eql 'null value)
             (setf (slot-value instance '%login-page)
                   (make-instance 'configuration-login-page :data value)))))))))

(define-object configuration-business-profile ()
  (headline
   :type (or string null)
   :documentation "The messaging shown to customers in the portal.")
  (privacy-policy-url
   :type (or string null)
   :documentation "A link to the business's publicly available privacy
policy.")
  (terms-of-service-url
   :type (or string null)
   :documentation "A link to the business's publicly available terms of
service.")
  (:list-type nil))

(define-object configuration-features ()
  (customer-update
   :type features-customer-update)
  (invoice-history
   :type features-invoice-history)
  (payment-method-update
   :type features-payment-method-update)
  (subscription-cancel
   :type features-subscription-cancel)
  (subscription-update
   :type features-subscription-update)
  (:list-type nil))

(defmethod initialize-instance :after ((instance configuration-features)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:customer-update
           (unless (eql 'null value)
             (setf (slot-value instance '%customer-update)
                   (make-instance 'features-customer-update :data value))))
          (:invoice-history
           (unless (eql 'null value)
             (setf (slot-value instance '%invoice-history)
                   (make-instance 'features-invoice-history :data value))))
          (:payment-method-update
           (unless (eql 'null value)
             (setf (slot-value instance '%payment-method-update)
                   (make-instance 'features-payment-method-update :data value))))
          (:subscription-cancel
           (unless (eql 'null value)
             (setf (slot-value instance '%subscription-cancel)
                   (make-instance 'features-subscription-cancel :data value))))
          (:subscription-update
           (unless (eql 'null value)
             (setf (slot-value instance '%subscription-update)
                   (make-instance 'features-subscription-update :data value)))))))))

(define-object features-customer-update ()
  (allowed-updates
   :type (vector string)
   :documentation "The types of customer updates that are supported.
One of `address`, `email`, `name`, `phone`, `shipping`, `tax_id`. When
empty, customers are not updateable.")
  (enabled
   :type boolean
   :documentation "Whether the feature is enabled.")
  (:list-type nil))

(define-object features-invoice-history ()
  (enabled
   :type boolean
   :documentation "Whether the feature is enabled.")
  (:list-type nil))

(define-object features-payment-method-update ()
  (enabled
   :type boolean
   :documentation "Whether the feature is enabled.")
  (:list-type nil))

(define-object features-subscription-cancel ()
  (cancellation-reason
   :type cancellation-reason)
  (enabled
   :type boolean
   :documentation "Whether the feature is enabled.")
  (mode
   :type string
   :documentation "Whether to cancel subscriptions immediately or at
the end of the billing period. One of `at_period_end` or `immediately`.")
  (proration-behavior
   :type string
   :documentation "How to handle prorations when canceling. One of
`always_invoice`, `create_prorations`, or `none`.")
  (:list-type nil))

(defmethod initialize-instance :after ((instance features-subscription-cancel)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:cancellation-reason
           (unless (eql 'null value)
             (setf (slot-value instance '%cancellation-reason)
                   (make-instance 'cancellation-reason :data value)))))))))

(define-object cancellation-reason ()
  (enabled
   :type boolean
   :documentation "Whether the feature is enabled.")
  (options
   :type (vector string)
   :documentation "Which cancellation reasons will be given as options.
One of `customer_service`, `low_quality`, `missing_features`, `other`,
`switched_service`, `too_complex`, `too_expensive`, `unused`.")
  (:list-type nil))

(define-object subscription-update ()
  (default-allowed-updates
   :type (vector string)
   :documentation "The types of subscription updates supported.
One of: `price`, `promotion_code`, or `quantity`. When empty,
subscriptions are not updateable.")
  (enabled
   :type boolean
   :documentation "Whether the feature is enabled.")
  (products
   :type (or (vector subscription-update-product) null)
   :documentation "The list of up to 10 products that support
subscription updates.")
  (proration-behavior
   :type string
   :documentation "Determines how to handle prorations resulting from
subscription updates. Valid values are `always_invoice`,
`create_prorations`, or `none`. Defaults to a value of `none` if you
don't set it during creation.")
  (schedule-at-period-end
   :type (or schedule-at-period-end null))
  (:list-type nil))

(defmethod initialize-instance :after ((instance subscription-update) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:products
           (unless (eql 'null value)
             (setf (slot-value instance '%products)
                   (map 'vector
                        (lambda (product)
                          (make-instance 'subscription-update-product :data product))
                        value))))
          (:schedule-at-period-end
           (unless (eql 'null value)
             (setf (slot-value instance '%schedule-at-period-end)
                   (make-instance 'schedule-at-period-end :data value)))))))))

(define-object subscription-update-product ()
  (prices
   :type (vector string)
   :documentation "The list of price IDs which, when subscribed to, a
subscription can be updated.")
  (product
   :type string
   :documentation "The product ID.")
  (:list-type nil))

(define-object schedule-at-period-end ()
  (conditions
   :type (vector schedule-condition)
   :documentation "List of conditions. When any condition is true, an
update will be scheduled at the end of the current period.")
  (:list-type nil))

(defmethod initialize-instance :after ((instance schedule-at-period-end)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:conditions
           (unless (eql 'null value)
             (setf (slot-value instance '%conditions)
                   (map 'vector
                        (lambda (condition)
                          (make-instance 'schedule-condition :data condition))
                        value)))))))))

(define-object schedule-condition ()
  (type
   :reader condition-type
   :type string
   :documentation "The type of condition. One of
`decreasing_item_amount` or `shortening_interval`.")
  (:list-type nil))

(define-object login-page ()
  (enabled
   :type boolean
   :documentation "If `true`, a shareable `url` will be generated that
will take your customers to a hosted login page for the customer
portal. If `false`, the previously generated `url`, if any, will be
deactivated.")
  (url
   :type (or string null)
   :documentation "A shareable URL to the hosted portal login page.
Your customers will be able to log in with their
[email](https://stripe.com/docs/api/customers/object#customer_object-email)
and receive a link to their customer portal.")
  (:list-type nil))

(define-object api-login-page ()
  "The hosted login page for this configuration. Learn more about the
portal login page in our
[integration docs](https://stripe.com/docs/billing/subscriptions/integrating-customer-portal#share)."
  (enabled
   :type boolean
   :documentation "Set to `true` to generate a shareable URL
`login_page.url` that will take your customers to a hosted login page
for the customer portal. Set to `false` to deactivate the
`login_page.url`.")
  (:list-type nil))
