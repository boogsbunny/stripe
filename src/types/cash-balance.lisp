(in-package #:stripe)

(define-object cash-balance ()
  "A customer's `Cash balance` represents real funds. Customers can add
funds to their cash balance by sending a bank transfer. These funds can
be used for payment and can eventually be paid out to your bank
account."
  (object
   :type string
   :initform "cash_balance"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (available
   :type (or (hash-table :key-type string :value-type integer) null)
   :documentation "A hash of all cash balances available to this
customer. You cannot delete a customer with any cash balances, even if
the balance is 0. Amounts are represented in the [smallest currency
unit](https://stripe.com/docs/currencies#zero-decimal).")
  (customer
   :type string
   :documentation "The ID of the customer whose cash balance this
object represents.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (settings
   :type cash-balance-settings))

(define-object cash-balance-settings ()
  (reconciliation-mode
   :type string
   :documentation "The configuration for how funds that land in the
customer cash balance are reconciled. One of `automatic` or `manual`.")
  (using-merchant-default
   :type boolean
   :documentation "A flag to indicate if reconciliation mode returned
is the user's default or is specific to this customer cash balance."))

(defmethod initialize-instance :after ((instance cash-balance) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:settings
           (setf (slot-value instance '%settings)
                 (make-instance 'cash-balance-settings :data value))))))))

(define-object cash-balance-settings-request ()
  (reconciliation-mode
   :type string
   :documentation "Controls how funds transferred by the customer are
applied to payment intents and invoices. Valid options are `automatic`,
`manual`, or `merchant_default`. For more information about these
reconciliation modes, see [Reconciliation]
(https://docs.stripe.com/payments/customer-balance/reconciliation)."))
