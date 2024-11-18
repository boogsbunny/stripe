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
  id
  active
  created
  description
  images
  name
  package-dimensions
  shippable
  statement-descriptor
  unit-label
  updated
  url)

(define-object package-dimensions ()
  height
  (length :reader package-length)
  weight
  width)

(defmethod initialize-instance :after ((instance product) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:package-dimensions
           (unless (eql 'null value)
             (setf (slot-value instance '%package-dimensions)
                   (make-instance 'package-dimensions :data value))))
          (:updated
           (setf (slot-value instance '%updated) (decode-timestamp value))))))))
