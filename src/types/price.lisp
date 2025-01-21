(in-package #:stripe)

(define-object price ()
  "Prices define the unit cost, currency, and (optional) billing cycle
for both recurring and one-time purchases of products.
[Products](https://stripe.com/docs/api#products) help you track
inventory or provisioning, and prices help you track payment terms.
Different physical goods or levels of service should be represented by
products, and pricing options should be represented by prices. This
approach lets you change prices without having to change your
provisioning scheme.

For example, you might have a single 'gold' product that has prices for
$10/month, $100/year, and €9 once.

Related guides: [Set up a subscription](https://stripe.com/docs/billing/subscriptions/set-up-subscription),
[create an invoice](https://stripe.com/docs/billing/invoices/create),
and more about [products and prices](https://stripe.com/docs/products-prices/overview)."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "price"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (active
   :type boolean
   :documentation "Whether the price can be used for new purchases.")
  (billing-scheme
   :type price-billing-scheme
   :documentation "Describes how to compute the price per period.
Either `per_unit` or `tiered`. `per_unit` indicates that the fixed
amount (specified in `unit_amount` or `unit_amount_decimal`) will be
charged per unit in `quantity` (for prices with `usage_type=licensed`),
or per unit of total usage (for prices with `usage_type=metered`).
`tiered` indicates that the unit pricing will be computed using a
tiering strategy as defined using the `tiers` and `tiers_mode`
attributes.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (currency
   :type string
   :documentation "Three-letter
[ISO currency code](https://www.iso.org/iso-4217-currency-codes.html),
in lowercase. Must be a
[supported currency](https://stripe.com/docs/currencies).")
  (currency-options
   :type (or (hash-table :key-type string :value-type price-currency-options) null)
   :documentation "Prices defined in each available currency option.
Each key must be a three-letter
[ISO currency code](https://www.iso.org/iso-4217-currency-codes.html)
and a [supported currency](https://stripe.com/docs/currencies).")
  (custom-unit-amount
   :type (or price-custom-unit-amount null)
   :documentation "When set, provides configuration for the amount to
be adjusted by the customer during Checkout Sessions and Payment Links.")
  (deleted
   :type (or boolean null)
   :documentation "Always true for a deleted object.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (lookup-key
   :type (or string null)
   :documentation "A lookup key used to retrieve prices dynamically
from a static string. This may be up to 200 characters.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (nickname
   :type (or string null)
   :documentation "A brief description of the price, hidden from
customers.")
  (product
   :type (or string product deleted-product)
   :documentation "The ID of the product this price is associated with.")
  (recurring
   :type (or price-recurring null)
   :documentation "The recurring components of a price such as
`interval` and `usage_type`.")
  (tax-behavior
   :type (or string null)
   :documentation "Only required if a
[default tax behavior](https://stripe.com/docs/tax/products-prices-tax-categories-tax-behavior#setting-a-default-tax-behavior-(recommended))
was not provided in the Stripe Tax settings. Specifies whether the
price is considered inclusive of taxes or exclusive of taxes. One of
`inclusive`, `exclusive`, or `unspecified`. Once specified as either
`inclusive` or `exclusive`, it cannot be changed.")
  (tiers
   :type (or (vector price-tier) null)
   :documentation "Each element represents a pricing tier. This
parameter requires `billing_scheme` to be set to `tiered`. See also the
documentation for `billing_scheme`.")
  (tiers-mode
   :type (or string null)
   :documentation "Defines if the tiering price should be `graduated`
or `volume` based. In `volume`-based tiering, the maximum quantity
within a period determines the per unit price. In `graduated` tiering,
pricing can change as the quantity grows. One of `graduated` or
`volume`.")
  (transform-quantity
   :type (or price-transform-quantity null)
   :documentation "Apply a transformation to the reported usage or set
quantity before computing the amount billed. Cannot be combined with
`tiers`.")
  (type
   :reader price-type
   :type string
   :documentation "One of `one_time` or `recurring` depending on
whether the price is for a one-time purchase or a recurring
(subscription) purchase.")
  (unit-amount-decimal
   :type (or string null)
   :documentation "The unit amount in cents (or local equivalent) to be
charged, represented as a decimal string with at most 12 decimal
places. Only set if `billing_scheme=per_unit`.")
  (:list-type t))

(defmethod initialize-instance :after ((instance price) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:billing-scheme
           (unless (eql 'null value)
             (setf (slot-value instance '%billing-scheme)
                   (make-instance 'price-billing-scheme :data value))))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:currency-options
           (unless (eql 'null value)
             (let ((options (make-hash-table :test 'equal)))
               (maphash (lambda (k v)
                          (setf (gethash k options)
                                (make-instance 'price-currency-options :data v)))
                        value)
               (setf (slot-value instance '%currency-options) options))))
          (:custom-unit-amount
           (unless (eql 'null value)
             (setf (slot-value instance '%custom-unit-amount)
                   (make-instance 'price-custom-unit-amount :data value))))
          (:product
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%product)
                   (make-instance (if (gethash :deleted value)
                                      'deleted-product
                                      'product)
                                  :data value))))
          (:recurring
           (unless (eql 'null value)
             (setf (slot-value instance '%recurring)
                   (make-instance 'price-recurring :data value))))
          (:tiers
           (unless (eql 'null value)
             (setf (slot-value instance '%tiers)
                   (map 'vector
                        (lambda (tier)
                          (make-instance 'price-tier :data tier))
                        value))))
          (:transform-quantity
           (unless (eql 'null value)
             (setf (slot-value instance '%transform-quantity)
                   (make-instance 'price-transform-quantity :data value)))))))))

(define-object deleted-price ()
  "The DeletedPrice object."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "price"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (deleted
   :type boolean
   :initform t
   :documentation "Always true for a deleted object.")
  (:list-type t))

(define-object price-billing-scheme ()
  (type
   :reader billing-scheme-type
   :type string
   :documentation "One of `per_unit` or `tiered`."))

(define-object price-currency-options ()
  (custom-unit-amount
   :type (or price-currency-options-custom-unit-amount null)
   :documentation "When set, provides configuration for the amount to
be adjusted by the customer during Checkout Sessions and Payment Links.")
  (tax-behavior
   :type (or string null)
   :documentation "Only required if a
[default tax behavior](https://stripe.com/docs/tax/products-prices-tax-categories-tax-behavior#setting-a-default-tax-behavior-(recommended))
was not provided in the Stripe Tax settings. Specifies whether the
price is considered inclusive of taxes or exclusive of taxes. One of
`inclusive`, `exclusive`, or `unspecified`. Once specified as either
`inclusive` or `exclusive`, it cannot be changed.")
  (tiers
   :type (or (vector price-currency-options-tier) null)
   :documentation "Each element represents a pricing tier. This
parameter requires `billing_scheme` to be set to `tiered`. See also the
documentation for `billing_scheme`.")
  (unit-amount
   :type (or integer null)
   :documentation "The unit amount in cents (or local equivalent) to be
charged, represented as a whole integer if possible. Only set if
`billing_scheme=per_unit`.")
  (unit-amount-decimal
   :type (or string null)
   :documentation "The unit amount in cents (or local equivalent) to be
charged, represented as a decimal string with at most 12 decimal
places. Only set if `billing_scheme=per_unit`."))

(defmethod initialize-instance :after ((instance price-currency-options)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:custom-unit-amount
           (unless (eql 'null value)
             (setf (slot-value instance '%custom-unit-amount)
                   (make-instance 'price-currency-options-custom-unit-amount :data value))))
          (:tiers
           (unless (eql 'null value)
             (setf (slot-value instance '%tiers)
                   (map 'vector
                        (lambda (tier)
                          (make-instance 'price-currency-options-tier :data tier))
                        value)))))))))

(define-object price-currency-options-custom-unit-amount ()
  (maximum
   :type (or integer null)
   :documentation "The maximum unit amount the customer can specify for
this item.")
  (minimum
   :type (or integer null)
   :documentation "The minimum unit amount the customer can specify for
this item.")
  (preset
   :type (or integer null)
   :documentation "The starting unit amount which can be updated by the
customer."))

(define-object price-currency-options-tier ()
  (flat-amount
   :type (or integer null)
   :documentation "Price for the entire tier.")
  (flat-amount-decimal
   :type (or string null)
   :documentation "Same as `flat_amount`, but contains a decimal value
with at most 12 decimal places.")
  (unit-amount
   :type (or integer null)
   :documentation "Per unit price for units relevant to the tier.")
  (unit-amount-decimal
   :type (or string null)
   :documentation "Same as `unit_amount`, but contains a decimal value
with at most 12 decimal places.")
  (up-to
   :type (or integer null)
   :documentation "Up to and including to this quantity will be
contained in the tier."))

(define-object price-custom-unit-amount ()
  (maximum
   :type (or integer null)
   :documentation "The maximum unit amount the customer can specify.")
  (minimum
   :type (or integer null)
   :documentation "The minimum unit amount the customer can specify for
this item. Must be at least the minimum charge amount.")
  (preset
   :type (or integer null)
   :documentation "The starting unit amount which can be updated by the
 customer."))

(define-object price-recurring ()
  (aggregate-usage
   :type (or string null)
   :documentation "Specifies a usage aggregation strategy for prices of
`usage_type=metered`. Defaults to `sum`. One of `last_during_period`,
`last_ever`, `max`, or `sum`.")
  (interval
   :type string
   :documentation "The frequency at which a subscription is billed. One
of `day`, `week`, `month` or `year`.")
  (interval-count
   :type integer
   :documentation "The number of intervals (specified in the `interval`
attribute) between subscription billings. For example, `interval=month`
and `interval_count=3` bills every 3 months.")
  (meter
   :type (or string null)
   :documentation "The meter tracking the usage of a metered price.")
  (trial-period-days
   :type (or integer null)
   :documentation "Default number of trial days when subscribing a
customer to this price using
[`trial_from_plan=true`](https://stripe.com/docs/api#create_subscription-trial_from_plan).")
  (usage-type
   :type string
   :documentation "Configures how the quantity per period should be
determined. Can be either `metered` or `licensed`. `licensed`
automatically bills the `quantity` set when adding it to a
subscription. `metered` aggregates the total usage based on usage
records. Defaults to `licensed`."))

(define-object price-tier ()
  (flat-amount
   :type (or integer null)
   :documentation "Price for the entire tier.")
  (flat-amount-decimal
   :type (or string null)
   :documentation "Same as `flat_amount`, but contains a decimal value
with at most 12 decimal places.")
  (unit-amount
   :type (or integer null)
   :documentation "Per unit price for units relevant to the tier.")
  (unit-amount-decimal
   :type (or string null)
   :documentation "Same as `unit_amount`, but contains a decimal value
with at most 12 decimal places.")
  (up-to
   :type (or integer null)
   :documentation "Up to and including to this quantity will be
contained in the tier."))

(define-object price-transform-quantity ()
  (divide-by
   :type integer
   :documentation "Divide usage by this number.")
  (round
   :reader quantity-round
   :type string
   :documentation "After division, either round the result `up` or
`down`."))
