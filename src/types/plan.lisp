(in-package #:stripe)

(define-object plan ()
  "You can now model subscriptions more flexibly using the [Prices API]
(https://stripe.com/docs/api#prices). It replaces the Plans API and is
backwards compatible to simplify your migration.

Plans define the base price, currency, and billing cycle for recurring
purchases of products. [Products](https://stripe.com/docs/api#products)
help you track inventory or provisioning, and plans help you track
pricing. Different physical goods or levels of service should be
represented by products, and pricing options should be represented by
plans. This approach lets you change prices without having to change
your provisioning scheme.

For example, you might have a single "gold" product that has plans for
$10/month, $100/year, €9/month, and €90/year.

Related guides: [Set up a subscription]
(https://stripe.com/docs/billing/subscriptions/set-up-subscription)
and more about [products and prices]
(https://stripe.com/docs/products-prices/overview)."
  id
  active
  aggregate-usage
  amount
  billing-scheme
  created
  currency
  interval
  nickname
  product
  trial-period-days
  usage-type)

(defmethod initialize-instance :after ((instance plan) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value))))))))
