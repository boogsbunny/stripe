(in-package #:stripe)

(define-object line-item ()
  "A line item."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "item"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (amount-discount
   :type integer
   :documentation "Total discount amount applied. If no discounts were
applied, defaults to 0.")
  (amount-subtotal
   :type integer
   :documentation "Total before any discounts or taxes are applied.")
  (amount-tax
   :type integer
   :documentation "Total tax amount applied. If no tax was applied,
defaults to 0.")
  (amount-total
   :type integer
   :documentation "Total after discounts and taxes.")
  (currency
   :type string
   :documentation "Three-letter [ISO currency code](https://www.iso.org/iso-4217-currency-codes.html),
in lowercase.
Must be a [supported currency](https://stripe.com/docs/currencies).")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the object. Often
useful for displaying to users. Defaults to product name.")
  (discounts
   :type (or (vector line-item-discount) null)
   :documentation "The discounts applied to the line item.")
  (price
   :type (or price null)
   :documentation "The price used to generate the line item.")
  (quantity
   :type (or integer null)
   :documentation "The quantity of products being purchased.")
  (taxes
   :type (or (vector line-item-tax) null)
   :documentation "The taxes applied to the line item.")
  (:list-type t))

(defmethod initialize-instance :after ((instance line-item) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:discounts
           (unless (eql 'null value)
             (setf (slot-value instance '%discounts)
                   (map 'vector
                        (lambda (discount)
                          (make-instance 'line-item-discount :data discount))
                        value))))
          (:price
           (unless (eql 'null value)
             (setf (slot-value instance '%price)
                   (make-instance 'price :data value))))
          (:taxes
           (unless (eql 'null value)
             (setf (slot-value instance '%taxes)
                   (map 'vector
                        (lambda (tax)
                          (make-instance 'line-item-tax :data tax))
                        value)))))))))

(define-object line-item-discount ()
  (amount
   :type integer
   :documentation "The amount discounted.")
  (discount
   :type discount
   :documentation "A discount represents the actual application of a
[coupon](https://stripe.com/docs/api#coupons) or
[promotion code](https://stripe.com/docs/api#promotion_codes).

It contains information about when the discount began, when it will
end, and what it is applied to.

Related guide: [Applying discounts to subscriptions](https://stripe.com/docs/billing/subscriptions/discounts)"))

(defmethod initialize-instance :after ((instance line-item-discount) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:discount
           (unless (eql 'null value)
             (setf (slot-value instance '%discount)
                   (make-instance 'discount :data value)))))))))

(define-object line-item-tax ()
  (amount
   :type integer
   :documentation "Amount of tax applied for this rate.")
  (rate
   :type tax-rate
   :documentation "Tax rates can be applied to
[invoices](https://stripe.com/docs/billing/invoices/tax-rates),
[subscriptions](https://stripe.com/docs/billing/subscriptions/taxes)
and [Checkout Sessions](https://stripe.com/docs/payments/checkout/set-up-a-subscription#tax-rates)
to collect tax.

Related guide: [Tax rates](https://stripe.com/docs/billing/taxes/tax-rates)")
  (taxability-reason
   :type (or string null)
   :documentation "The reasoning behind this tax, for example, if the
product is tax exempt. The possible values for this field may be
extended as new tax rules are supported.

One of:

`customer_exempt`, `not_collecting`, `not_subject_to_tax`,
`not_supported`,`portion_product_exempt`, `portion_reduced_rated`,
`portion_standard_rated`, `product_exempt`, `product_exempt_holiday`,
`proportionally_rated`, `reduced_rated`, `reverse_charge`,
`standard_rated`, `taxable_basis_reduced`, or `zero_rated`.")
  (taxable-amount
   :type (or integer null)
   :documentation "The amount on which tax is calculated, in cents
(or local equivalent)."))

(defmethod initialize-instance :after ((instance line-item-tax) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:rate
           (unless (eql 'null value)
             (setf (slot-value instance '%rate)
                   (make-instance 'tax-rate :data value)))))))))
