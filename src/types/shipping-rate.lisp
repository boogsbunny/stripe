(in-package #:stripe)

(define-object shipping-rate ()
  "Shipping rates describe the price of shipping presented to your
customers and applied to a purchase. For more information, see
[Charge for shipping](https://stripe.com/docs/payments/during-payment/charge-shipping)."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "shipping_rate"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (active
   :type boolean
   :documentation "Whether the shipping rate can be used for new
purchases. Defaults to `true`.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (delivery-estimate
   :type (or shipping-rate-delivery-estimate null)
   :documentation "The estimated range for how long shipping will take,
meant to be displayable to the customer. This will appear on
CheckoutSessions.")
  (display-name
   :type (or string null)
   :documentation "The name of the shipping rate, meant to be
displayable to the customer. This will appear on CheckoutSessions.")
  (fixed-amount
   :type (or shipping-rate-fixed-amount null))
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (tax-behavior
   :type (or string null)
   :documentation "Specifies whether the rate is considered inclusive
of taxes or exclusive of taxes. One of `inclusive`, `exclusive`, or
`unspecified`.")
  (tax-code
   :type (or string tax-code null)
   :documentation "A [tax code](https://stripe.com/docs/tax/tax-categories)
ID. The Shipping tax code is `txcd_92010001`.")
  (type
   :reader shipping-rate-type
   :type string
   :initform "fixed_amount"
   :documentation "The type of calculation to use on the shipping rate.")
  (:list-type t))

(defmethod initialize-instance :after ((instance shipping-rate) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:delivery-estimate
           (unless (eql 'null value)
             (setf (slot-value instance '%delivery-estimate)
                   (make-instance 'shipping-rate-delivery-estimate :data value))))
          (:fixed-amount
           (unless (eql 'null value)
             (setf (slot-value instance '%fixed-amount)
                   (make-instance 'shipping-rate-fixed-amount :data value))))
          (:tax-code
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%tax-code)
                   (make-instance 'tax-code :data value)))))))))

(define-object shipping-rate-delivery-estimate ()
  (maximum
   :type (or shipping-rate-delivery-estimate-maximum null)
   :documentation "The upper bound of the estimated range. If empty,
represents no upper bound i.e., infinite.")
  (minimum
   :type (or shipping-rate-delivery-estimate-minimum null)
   :documentation "The lower bound of the estimated range. If empty,
represents no lower bound."))

(defmethod initialize-instance :after ((instance shipping-rate-delivery-estimate)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:maximum
           (unless (eql 'null value)
             (setf (slot-value instance '%maximum)
                   (make-instance 'shipping-rate-delivery-estimate-maximum :data value))))
          (:minimum
           (unless (eql 'null value)
             (setf (slot-value instance '%minimum)
                   (make-instance 'shipping-rate-delivery-estimate-minimum :data value)))))))))

(define-object shipping-rate-delivery-estimate-maximum ()
  (unit
   :type string
   :documentation "A unit of time. One of `business_day`, `day`,
`hour`, `month`, or `week`.")
  (value
   :type integer
   :documentation "Must be greater than 0."))

(define-object shipping-rate-delivery-estimate-minimum ()
  (unit
   :type string
   :documentation "A unit of time. One of `business_day`, `day`,
`hour`, `month`, or `week`.")
  (value
   :type integer
   :documentation "Must be greater than 0."))

(define-object shipping-rate-fixed-amount ()
  (amount
   :type integer
   :documentation "A non-negative integer in cents representing how
much to charge.")
  (currency
   :type string
   :documentation "Three-letter [ISO currency code](https://www.iso.org/iso-4217-currency-codes.html),
in lowercase. Must be a
[supported currency](https://stripe.com/docs/currencies).")
  (currency-options
   :type (or (hash-table :key-type string :value-type shipping-rate-fixed-amount-currency-options)
             null)
   :documentation "Shipping rates defined in each available currency
option. Each key must be a three-letter
[ISO currency code](https://www.iso.org/iso-4217-currency-codes.html)
and a [supported currency](https://stripe.com/docs/currencies)."))

(defmethod initialize-instance :after ((instance shipping-rate-fixed-amount)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:currency-options
           (unless (eql 'null value)
             (let ((options (make-hash-table :test 'equal)))
               (maphash (lambda (k v)
                          (setf (gethash k options)
                                (make-instance 'shipping-rate-fixed-amount-currency-options
                                               :data v)))
                        value)
               (setf (slot-value instance '%currency-options) options)))))))))

(define-object shipping-rate-fixed-amount-currency-options ()
  (amount
   :type integer
   :documentation "A non-negative integer in cents representing how
much to charge.")
  (tax-behavior
   :type string
   :documentation "Specifies whether the rate is considered inclusive
of taxes or exclusive of taxes. One of `inclusive`, `exclusive`, or
`unspecified`."))
