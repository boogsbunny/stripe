(in-package #:stripe)

(define-object coupon ()
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
  valid)

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