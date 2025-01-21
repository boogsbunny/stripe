(in-package #:stripe)

(define-object tax-rate ()
  "Tax rates can be applied to
[invoices](https://stripe.com/docs/billing/invoices/tax-rates),
[subscriptions](https://stripe.com/docs/billing/subscriptions/taxes)
and [Checkout Sessions](https://stripe.com/docs/payments/checkout/set-up-a-subscription#tax-rates)
to collect tax.

Related guide: [Tax rates](https://stripe.com/docs/billing/taxes/tax-rates)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "tax_rate"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (active
   :type boolean
   :documentation "Defaults to `true`. When set to `false`, this tax
rate cannot be used with new applications or Checkout Sessions, but
will still work for subscriptions and invoices that already have it
set.")
  (country
   :type (or string null)
   :documentation "Two-letter country code
([ISO 3166-1 alpha-2](https://en.wikipedia.org/wiki/ISO_3166-1_alpha-2)).")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the tax rate for
your internal use only. It will not be visible to your customers.")
  (display-name
   :type string
   :documentation "The display name of the tax rates as it will appear
to your customer on their receipt email, PDF, and the hosted invoice
page.")
  (effective-percentage
   :type (or real null)
   :documentation "Actual/effective tax rate percentage out of 100. For
tax calculations with automatic_tax[enabled]=true, this percentage
reflects the rate actually used to calculate tax based on the product's
taxability and whether the user is registered to collect taxes in the
corresponding jurisdiction.")
  (flat-amount
   :type (or tax-rate-flat-amount null)
   :documentation "The amount of the tax rate when the `rate_type` is
`flat_amount`. Tax rates with `rate_type` `percentage` can vary based
on the transaction, resulting in this field being `null`. This field
exposes the amount and currency of the flat tax rate.")
  (inclusive
   :type boolean
   :documentation "This specifies if the tax rate is inclusive or
exclusive.")
  (jurisdiction
   :type (or string null)
   :documentation "The jurisdiction for the tax rate. You can use this
label field for tax reporting purposes. It also appears on your
customer's invoice.")
  (jurisdiction-level
   :type (or string null)
   :documentation "The level of the jurisdiction that imposes this tax
rate. One of `city`, `country`, `county`, `district`, `multiple`, or
`state`.")
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
  (percentage
   :type real
   :documentation "Tax rate percentage out of 100. For tax calculations
with automatic_tax[enabled]=true, this percentage includes the
statutory tax rate of non-taxable jurisdictions.")
  (rate-type
   :type (or string null)
   :documentation "Indicates the type of tax rate applied to the
taxable amount. One of `flat_amount` or `percentage`.")
  (state
   :type (or string null)
   :documentation "[ISO 3166-2 subdivision code](https://en.wikipedia.org/wiki/ISO_3166-2:US),
without country prefix. For example, 'NY' for New York, United States.")
  (tax-type
   :type (or string null)
   :documentation "The high-level tax type, such as `vat` or
`sales_tax`. One of `amusement_tax`, `communications_tax`, `gst`,
`hst`, `igst`, `jct`, `lease_tax`, `pst`, `qst`, `retail_delivery_fee`,
`rst`, `sales_tax`, or `vat`.")
  (:list-type t))

(defmethod initialize-instance :after ((instance tax-rate) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:flat-amount
           (unless (eql 'null value)
             (setf (slot-value instance '%flat-amount)
                   (make-instance 'tax-rate-flat-amount :data value)))))))))

(define-object tax-rate-flat-amount ()
  (amount
   :type integer
   :documentation "Amount of the tax when the `rate_type` is
`flat_amount`. This positive integer represents how much to charge in
the smallest currency unit (e.g., 100 cents to charge $1.00 or 100 to
charge ¥100, a zero-decimal currency). The amount value supports up to
eight digits (e.g., a value of 99999999 for a USD charge of
$999,999.99).")
  (currency
   :type string
   :documentation "Three-letter ISO currency code, in lowercase."))
