(in-package #:stripe)

(define-object discount ()
  "A discount represents the actual application of a [coupon]
(https://stripe.com/docs/api#coupons) or [promotion code]
(https://stripe.com/docs/api#promotion_codes). It contains information
about when the discount began, when it will end, and what it is applied
to.

Related guide: [Applying discounts to subscriptions]
(https://stripe.com/docs/billing/subscriptions/discounts)"
  coupon
  customer
  end
  start
  subscription
  (:list-type t))

(defmethod initialize-instance :after ((instance discount) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:end
           (setf (slot-value instance '%end) (decode-timestamp value)))
          (:start
           (setf (slot-value instance '%start) (decode-timestamp value))))))))
