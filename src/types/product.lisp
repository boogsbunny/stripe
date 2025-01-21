(in-package #:stripe)

(define-object product ()
  "Products describe the specific goods or services you offer to your
customers. For example, you might offer a Standard and Premium version
of your goods or service; each version would be a separate Product.
They can be used in conjunction with [Prices]
(https://stripe.com/docs/api#prices) to configure pricing in Payment
Links, Checkout, and Subscriptions.

Related guides: [Set up a subscription]
(https://stripe.com/docs/billing/subscriptions/set-up-subscription),
[share a Payment Link](https://stripe.com/docs/payment-links),
[accept payments with Checkout]
(https://stripe.com/docs/payments/accept-a-payment#create-product-prices-upfront),
and more about [Products and Prices]
(https://stripe.com/docs/products-prices/overview)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "product"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (active
   :type boolean
   :documentation "Whether the product is currently available for
purchase.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (default-price
   :type (or string price null)
   :documentation "The ID of the
[Price](https://stripe.com/docs/api/prices) object that is the default
price for this product.")
  (deleted
   :type (or boolean null)
   :documentation "Always true for a deleted object.")
  (description
   :type (or string null)
   :documentation "The product's description, meant to be displayable
to the customer. Use this field to optionally store a long form
explanation of the product being sold for your own rendering purposes.")
  (images
   :type (vector string)
   :documentation "A list of up to 8 URLs of images for this product,
meant to be displayable to the customer.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (marketing-features
   :type (vector product-marketing-feature)
   :documentation "A list of up to 15 marketing features for this
product. These are displayed in
[pricing tables](https://stripe.com/docs/payments/checkout/pricing-table).")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (name
   :type string
   :documentation "The product's name, meant to be displayable to the
customer.")
  (package-dimensions
   :type (or product-package-dimensions null)
   :documentation "The dimensions of this product for shipping
purposes.")
  (shippable
   :type (or boolean null)
   :documentation "Whether this product is shipped (i.e.,
physical goods).")
  (statement-descriptor
   :type (or string null)
   :documentation "Extra information about a product which will appear
on your customer's credit card statement. In the case that multiple
products are billed at once, the first statement descriptor will be
used. Only used for subscription payments.")
  (tax-code
   :type (or string tax-code null)
   :documentation "A [tax code](https://stripe.com/docs/tax/tax-categories)
ID.")
  (type
   :reader product-type
   :type string
   :documentation "The type of the product. The product is either of
type `good`, which is eligible for use with Orders and SKUs, or
`service`, which is eligible for use with Subscriptions and Plans.")
  (unit-label
   :type (or string null)
   :documentation "A label that represents units of this product. When
set, this will be included in customers' receipts, invoices, Checkout,
and the customer portal.")
  (updated
   :type time:timestamp
   :documentation "Time at which the object was last updated. Measured
in seconds since the Unix epoch.")
  (url
   :type (or string null)
   :documentation "A URL of a publicly-accessible webpage for this
product.")
  (:list-type t))

(defmethod initialize-instance :after ((instance product) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:default-price
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%default-price)
                   (make-instance 'price :data value))))
          (:marketing-features
           (unless (eql 'null value)
             (setf (slot-value instance '%marketing-features)
                   (map 'vector
                        (lambda (feature)
                          (make-instance 'product-marketing-feature :data feature))
                        value))))
          (:package-dimensions
           (unless (eql 'null value)
             (setf (slot-value instance '%package-dimensions)
                   (make-instance 'product-package-dimensions :data value))))
          (:tax-code
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%tax-code)
                   (make-instance 'tax-code :data value))))
          (:updated
           (setf (slot-value instance '%updated) (decode-timestamp value))))))))

(define-object deleted-product ()
  "The DeletedProduct object."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "product"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (deleted
   :type boolean
   :initform t
   :documentation "Always true for a deleted object."))

(define-object product-marketing-feature ()
  (name
   :type (or string null)
   :documentation "The marketing feature name. Up to 80 characters
long."))

(define-object product-package-dimensions ()
  (height
   :type double-float
   :documentation "Height, in inches.")
  (length
   :reader package-length
   :type double-float
   :documentation "Length, in inches.")
  (weight
   :type double-float
   :documentation "Weight, in ounces.")
  (width
   :type double-float
   :documentation "Width, in inches."))
