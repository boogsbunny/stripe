(in-package #:stripe)

(define-object customer-session ()
  "A Customer Session allows you to grant Stripe's frontend SDKs (like
Stripe.js) client-side access control over a Customer.

Related guides: [Customer Session with the Payment Element](https://stripe.com/payments/accept-a-payment-deferred?platform=web&type=payment#save-payment-methods),
[Customer Session with the Pricing Table](https://stripe.com/payments/checkout/pricing-table#customer-session),
[Customer Session with the Buy Button](https://stripe.com/payment-links/buy-button#pass-an-existing-customer)."
  (object
   :type string
   :initform "customer_session"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (client-secret
   :type string
   :documentation "The client secret of this Customer Session. Used on
the client to set up secure access to the given `customer`.

The client secret can be used to provide access to `customer` from your
frontend. It should not be stored, logged, or exposed to anyone other
than the relevant customer. Make sure that you have TLS enabled on any
page that includes the client secret.")
  (components
   :type (or customer-session-components null)
   :documentation "Configuration for the components supported by this
Customer Session.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (customer
   :type (or string customer)
   :documentation "The Customer the Customer Session was created for.")
  (expires-at
   :type time:timestamp
   :documentation "The timestamp at which this Customer Session will
expire.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode."))

(defmethod initialize-instance :after ((instance customer-session) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:components
           (when value
             (setf (slot-value instance '%components)
                   (make-instance 'customer-session-components :data value))))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:expires-at
           (setf (slot-value instance '%expires-at) (decode-timestamp value))))))))

(define-object customer-session-components ()
  (buy-button
   :type customer-session-components-buy-button
   :documentation "This hash contains whether the buy button is
enabled.")
  (payment-element
   :type customer-session-components-payment-element
   :documentation "This hash contains whether the Payment Element is
enabled and the features it supports.")
  (pricing-table
   :type customer-session-components-pricing-table
   :documentation "This hash contains whether the pricing table is
enabled."))

(defmethod initialize-instance :after ((instance customer-session-components)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:buy-button
           (when value
             (setf (slot-value instance '%buy-button)
                   (make-instance 'customer-session-components-buy-button :data value))))
          (:payment-element
           (when value
             (setf (slot-value instance '%payment-element)
                   (make-instance 'customer-session-components-payment-element :data value))))
          (:pricing-table
           (when value
             (setf (slot-value instance '%pricing-table)
                   (make-instance 'customer-session-components-pricing-table :data value)))))))))

(define-object customer-session-components-buy-button ()
  (enabled
   :type boolean
   :documentation "Whether the buy button is enabled."))

(define-object customer-session-components-payment-element ()
  (enabled
   :type boolean
   :documentation "Whether the Payment Element is enabled.")
  (features
   :type (or customer-session-components-payment-element-features null)
   :documentation "This hash defines whether the Payment Element
supports certain features."))

(defmethod initialize-instance :after ((instance customer-session-components-payment-element)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:features
           (when value
             (setf (slot-value instance '%features)
                   (make-instance 'customer-session-components-payment-element-features
                                  :data value)))))))))

(define-object customer-session-components-payment-element-features ()
  (payment-method-allow-redisplay-filters
   :type (vector string)
   :documentation "A list of
[`allow_redisplay`](https://docs.stripe.com/api/payment_methods/object#payment_method_object-allow_redisplay)
values that controls which saved payment methods the Payment Element
displays by filtering to only show payment methods with an
`allow_redisplay` value that is present in this list.

If not specified, defaults to [\"always\"]. In order to display all
saved payment methods, specify [\"always\"], \"limited\",
\"unspecified\"].")
  (payment-method-redisplay
   :type string
   :documentation "Controls whether or not the Payment Element shows
saved payment methods. One of `enabled` or `disabled`. This parameter
defaults to `disabled`.")
  (payment-method-redisplay-limit
   :type (or integer null)
   :documentation "Determines the max number of saved payment methods
for the Payment Element to display. This parameter defaults to `3`.")
  (payment-method-remove
   :type string
   :documentation "Controls whether the Payment Element displays the
option to remove a saved payment method. One of `enabled` or
`disabled`. This parameter defaults to `disabled`.

Allowing buyers to remove their saved payment methods impacts
subscriptions that depend on that payment method. Removing the payment
method detaches the
[`customer` object](https://docs.stripe.com/api/payment_methods/object#payment_method_object-customer)
from that [PaymentMethod](https://docs.stripe.com/api/payment_methods).")
  (payment-method-save
   :type string
   :documentation "Controls whether the Payment Element displays a
checkbox offering to save a new payment method. One of `enabled` or
`disabled`. This parameter defaults to `disabled`.

If a customer checks the box, the
[`allow_redisplay`](https://docs.stripe.com/api/payment_methods/object#payment_method_object-allow_redisplay)
value on the PaymentMethod is set to `'always'` at confirmation time.
For PaymentIntents, the
[`setup_future_usage`](https://docs.stripe.com/api/payment_intents/object#payment_intent_object-setup_future_usage)
value is also set to the value defined in `payment_method_save_usage`.")
  (payment-method-save-usage
   :type (or string null)
   :documentation "When using PaymentIntents and the customer checks
the save checkbox, this field determines the
[`setup_future_usage`](https://docs.stripe.com/api/payment_intents/object#payment_intent_object-setup_future_usage)
value used to confirm the PaymentIntent.

One of `off_session` or `on_session`.

When using SetupIntents, directly configure the
[`usage`](https://docs.stripe.com/api/setup_intents/object#setup_intent_object-usage)
value on SetupIntent creation."))

(define-object customer-session-components-pricing-table ()
  (enabled
   :type boolean
   :documentation "Whether the pricing table is enabled."))
