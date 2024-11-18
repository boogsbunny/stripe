(in-package #:stripe)

(define-object refund ()
  "Refund objects allow you to refund a previously created charge that
isn't refunded yet. Funds are refunded to the credit or debit card
that's initially charged.

Related guide: [Refunds](https://stripe.com/docs/refunds)"
  id
  amount
  balance-transaction
  charge
  created
  currency
  failure-balance-transaction
  failure-reason
  reason
  receipt-number
  status)

(defmethod initialize-instance :after ((instance refund) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value))))))))
