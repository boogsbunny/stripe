(in-package #:stripe)

(define-object coupon ()
  "A coupon contains information about a percent-off or amount-off
discount you might want to apply to a customer. Coupons may be applied
to [subscriptions](https://stripe.com/docs/api#subscriptions),
[invoices](https://stripe.com/docs/api#invoices), [checkout sessions]
(https://stripe.com/docs/api/checkout/sessions), [quotes]
(https://stripe.com/docs/api#quotes), and more. Coupons do not work
with conventional one-off [charges]
(https://stripe.com/docs/api#create_charge) or [payment intents]
(https://stripe.com/docs/api/payment_intents)."
  id
  amount-off
  created
  currency
  duration
  duration-in-months
  max-redemptions
  name
  percent-off
  redeem-by
  times-redeemed
  valid
  (:list-type t))

(defmethod initialize-instance :after ((instance coupon) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created)
                 (decode-timestamp value)))
          (:redeem-by
           (setf (slot-value instance '%redeem-by)
                 (decode-timestamp value))))))))
