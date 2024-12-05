(in-package #:stripe)

(define-object cash-balance-transaction ()
  "Customers with certain payments enabled have a cash balance,
representing funds that were paid by the customer to a merchant, but
have not yet been allocated to a payment. Cash Balance Transactions
represent when funds are moved into or out of this balance. This
includes funding by the customer, allocation to payments, and refunds
to the customer."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "customer_cash_balance_transaction"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (adjusted-for-overdraft
   :type (or adjusted-for-overdraft null)
   :documentation "If this is a `type=adjusted_for_overdraft`
transaction, contains information about what caused the overdraft,
which triggered this transaction.")
  (applied-to-payment
   :type (or applied-to-payment null)
   :documentation "If this is a `type=applied_to_payment` transaction,
contains information about how funds were applied.")
  (created
   :type local-time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (currency
   :type string
   :documentation "Three-letter [ISO currency code]
(https://www.iso.org/iso-4217-currency-codes.html), in lowercase. Must
be a [supported currency](https://stripe.com/docs/currencies).")
  (customer
   :type (or string customer)
   :documentation "The customer whose available cash balance changed as
a result of this transaction.")
  (ending-balance
   :type integer
   :documentation "The total available cash balance for the specified
currency after this transaction was applied. Represented in the
[smallest currency unit](https://stripe.com/docs/currencies#zero-decimal).")
  (funded
   :type (or funded null)
   :documentation "If this is a `type=funded` transaction, contains
information about the funding.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (net-amount
   :type integer
   :documentation "The amount by which the cash balance changed,
represented in the [smallest currency unit]
(https://stripe.com/docs/currencies#zero-decimal). A positive value
represents funds being added to the cash balance, a negative value
represents funds being removed from the cash balance.")
  (refunded-from-payment
   :type (or refunded-from-payment null)
   :documentation "If this is a `type=refunded_from_payment`
transaction, contains information about the source of the refund.")
  (transferred-to-balance
   :type (or transferred-to-balance null)
   :documentation "If this is a `type=transferred_to_balance`
transaction, contains the balance transaction linked to the transfer.")
  (type
   :reader cash-balance-transaction-type
   :type string
   :documentation "The type of the cash balance transaction. New types
may be added in future. See [Customer Balance]
(https://stripe.com/docs/payments/customer-balance#types) to learn more
about these types. One of `adjusted_for_overdraft`,
`applied_to_payment`, `funded`, `funding_reversed`,
`refunded_from_payment`, `return_canceled`, `return_initiated`,
`transferred_to_balance`, or `unapplied_from_payment`.")
  (unapplied-from-payment
   :type (or unapplied-from-payment null)
   :documentation "If this is a `type=unapplied_from_payment`
transaction, contains information about how funds were unapplied."))

(define-object adjusted-for-overdraft ()
  (balance-transaction
   :type (or string balance-transaction)
   :documentation "The [Balance Transaction]
(https://stripe.com/docs/api/balance_transactions/object) that
corresponds to funds taken out of your Stripe balance.")
  (linked-transaction
   :type (or string cash-balance-transaction)
   :documentation "The [Cash Balance Transaction]
(https://stripe.com/docs/api/cash_balance_transactions/object) that
brought the customer balance negative, triggering the clawback of
funds."))

(define-object applied-to-payment ()
  (payment-intent
   :type (or string payment-intent)
   :documentation "The [Payment Intent]
(https://stripe.com/docs/api/payment_intents/object) that funds were
applied to."))

(define-object funded ()
  (bank-transfer
   :type bank-transfer
   :documentation "Information about the bank transfer that funded the
customer's cash balance."))

(define-object refunded-from-payment ()
  (refund
   :type (or string refund)
   :documentation "The Refund that moved these funds into the
customer's cash balance."))

(define-object transferred-to-balance ()
  (balance-transaction
   :type (or string balance-transaction)
   :documentation "The Balance Transaction that corresponds to funds
transferred to your Stripe balance."))

(define-object unapplied-from-payment ()
  (payment-intent
   :type (or string payment-intent)
   :documentation "The Payment Intent that funds were unapplied from."))

(define-object bank-transfer ()
  (eu-bank-transfer
   :type (or eu-bank-transfer null)
   :documentation "EU-specific details of the bank transfer.")
  (gb-bank-transfer
   :type (or gb-bank-transfer null)
   :documentation "UK-specific details of the bank transfer.")
  (jp-bank-transfer
   :type (or jp-bank-transfer null)
   :documentation "Japan-specific details of the bank transfer.")
  (reference
   :type (or string null)
   :documentation "The user-supplied reference field on the bank
transfer.")
  (type
   :reader bank-transfer-type
   :type string
   :documentation "The funding method type used to fund the customer
balance. Permitted values include: `eu_bank_transfer`,
`gb_bank_transfer`, `jp_bank_transfer`, `mx_bank_transfer`, or
`us_bank_transfer`.")
  (us-bank-transfer
   :type (or us-bank-transfer null)
   :documentation "US-specific details of the bank transfer."))

(define-object eu-bank-transfer ()
  (bic
   :type (or string null)
   :documentation "The BIC of the bank of the sender of the funding.")
  (iban-last4
   :type (or string null)
   :documentation "The last 4 digits of the IBAN of the sender of the
funding.")
  (sender-name
   :type (or string null)
   :documentation "The full name of the sender, as supplied by the
sending bank."))

(define-object gb-bank-transfer ()
  (account-number-last4
   :type (or string null)
   :documentation "The last 4 digits of the account number of the
sender of the funding.")
  (sender-name
   :type (or string null)
   :documentation "The full name of the sender, as supplied by the
sending bank.")
  (sort-code
   :type (or string null)
   :documentation "The sort code of the bank of the sender of the
funding"))

(define-object jp-bank-transfer ()
  (sender-bank
   :type (or string null)
   :documentation "The name of the bank of the sender of the funding.")
  (sender-branch
   :type (or string null)
   :documentation "The name of the bank branch of the sender of the
funding.")
  (sender-name
   :type (or string null)
   :documentation "The full name of the sender, as supplied by the
sending bank."))

(define-object us-bank-transfer ()
  (network
   :type (or string null)
   :documentation "The banking network used for this funding. One of
`ach`, `domestic_wire_us`, or `swift`.")
  (sender-name
   :type (or string null)
   :documentation "The full name of the sender, as supplied by the
sending bank."))
