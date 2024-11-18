(in-package #:stripe)

(define-object payout ()
  "A `Payout` object is created when you receive funds from Stripe, or
when you initiate a payout to either a bank account or debit card of a
[connected Stripe account]
(https://stripe.com/docs/connect/bank-debit-card-payouts). You can
retrieve individual payouts,and list all payouts. Payouts are made on
[varying schedules]
(https://stripe.com/docs/connect/manage-payout-schedule), depending on
your country and industry.

Related guide: [Receiving payouts](https://stripe.com/docs/payouts)"
  id
  amount
  arrival-date
  automatic
  balance-transaction
  created
  currency
  description
  destination
  failure-balance-transaction
  failure-code
  failure-message
  (method :reader payout-method)
  source-type
  statement-descriptor
  status
  (type :reader payout-type))

(defmethod initialize-instance :after ((instance payout) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:arrival-date
           (setf (slot-value instance '%arrival-date) (decode-timestamp value)))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value))))))))
