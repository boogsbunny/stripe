(in-package #:stripe)

(define-object balance-transaction ()
  "Balance transactions represent funds moving through your Stripe
account.

Stripe creates them for every type of transaction that enters or leaves
your Stripe account balance.

Related guide: [Balance transaction types]
(https://stripe.com/docs/reports/balance-transaction-types)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "balance_transaction"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (amount
   :type integer
   :documentation "Gross amount of this transaction (in cents (or local
equivalent)). A positive value represents funds charged to another
party, and a negative value represents funds sent to another party.")
  (available-on
   :type time:timestamp
   :documentation "The date that the transaction's net funds become
available in the Stripe balance.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (currency
   :type string
   :documentation "Three-letter [ISO currency code](https://www.iso.org/iso-4217-currency-codes.html),
in lowercase. Must be a [supported currency](https://stripe.com/docs/currencies).")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the object. Often
useful for displaying to users.")
  (exchange-rate
   :type (or integer null)
   :documentation "If applicable, this transaction uses an exchange
rate. If money converts from currency A to currency B, then the
`amount` in currency A, multipled by the `exchange_rate`, equals the
`amount` in currency B. For example, if you charge a customer 10.00
EUR, the PaymentIntent's `amount` is `1000` and `currency` is `eur`.
If this converts to 12.34 USD in your Stripe account, the
BalanceTransaction's `amount` is `1234`, its `currency` is `usd`, and
the `exchange_rate` is `1.234`.")
  (fee
   :type integer
   :documentation "Fees (in cents (or local equivalent)) paid for this
transaction. Represented as a positive integer when assessed.")
  (fee-details
   :type fee-detail-collection
   :documentation "Detailed breakdown of fees (in cents (or local
equivalent)) paid for this transaction.")
  (net
   :type integer
   :documentation "Net impact to a Stripe balance (in cents (or local
equivalent)). A positive value represents incrementing a Stripe
balance, and a negative value decrementing a Stripe balance. You can
calculate the net impact of a transaction on a balance by `amount` -
`fee`.")
  (reporting-category
   :type string
   :documentation "Learn more about how [reporting categories](https://stripe.com/docs/reports/reporting-categories)
can help you understand balance transactions from an accounting
perspective.")
  ;; TODO: add balance transaction source type
  (source
   :type (or string null)
   :documentation "This transaction relates to the Stripe object.")
  (status
   :type string
   :documentation "The transaction's net funds status in the Stripe
balance, which are either `available` or `pending`.")
  (type
   :reader transaction-type
   :type string
   :documentation "Transaction type: `adjustment`, `advance`,
`advance_funding`, `anticipation_repayment`, `application_fee`,
`application_fee_refund`, `charge`, `climate_order_purchase`,
`climate_order_refund`, `connect_collection_transfer`, `contribution`,
`issuing_authorization_hold`, `issuing_authorization_release`,
`issuing_dispute`, `issuing_transaction`, `obligation_outbound`,
`obligation_reversal_inbound`, `payment`, `payment_failure_refund`,
`payment_network_reserve_hold`, `payment_network_reserve_release`,
`payment_refund`, `payment_reversal`, `payment_unreconciled`, `payout`,
`payout_cancel`, `payout_failure`, `refund`, `refund_failure`,
`reserve_transaction`, `reserved_funds`, `stripe_fee`, `stripe_fx_fee`,
`tax_fee`, `topup`, `topup_reversal`, `transfer`, `transfer_cancel`,
`transfer_failure`, or `transfer_refund`.

Learn more about [balance transaction types and what they represent](https://stripe.com/docs/reports/balance-transaction-types).
To classify transactions for accounting purposes, consider
`reporting_category` instead.")
  (:list-type t))

(define-object fee-detail (balance-transaction)
  (amount
   :type integer
   :documentation "Amount of the fee, in cents.")
  (application
   :type (or string null)
   :documentation "ID of the Connect application that earned the fee.")
  (currency
   :type string
   :documentation "Three-letter [ISO currency code](https://www.iso.org/iso-4217-currency-codes.html),
in lowercase. Must be a [supported currency](https://stripe.com/docs/currencies).")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the object. Often
useful for displaying to users.")
  (type
   :reader fee-type
   :type string
   :documentation "Type of the fee, one of: `application_fee`,
`payment_method_passthrough_fee`, `stripe_fee` or `tax`.")
  (:list-type t))

(defmethod initialize-instance :after ((instance balance-transaction) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more key value)
          (next-entry)
        (unless more (return))
        (case key
          (:available-on
           (setf (slot-value instance '%available-on) (decode-timestamp value)))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:fee-details
           (setf (slot-value instance '%fee-details)
                 (when value
                   (map 'list
                        (lambda (fee-data)
                          (make-instance 'fee :data fee-data))
                        value)))))))))
