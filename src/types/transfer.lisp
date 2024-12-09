(in-package #:stripe)

(define-object transfer ()
  "A `Transfer` object is created when you move funds between Stripe
accounts as part of Connect.

Before April 6, 2017, transfers also represented movement of funds
from a Stripe account to a card or bank account. This behavior has
since been split out into a [Payout]
(https://stripe.com/docs/api#payout_object) object, with corresponding
payout endpoints. For more information, read about the [transfer/payout
split](https://stripe.com/docs/transfer-payout-split).

Related guide: [Creating separate charges and transfers]
(https://stripe.com/docs/connect/separate-charges-and-transfers)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "transfer"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (amount
   :type integer
   :documentation "Amount in cents (or local equivalent) to be
transferred.")
  (amount-reversed
   :type integer
   :documentation "Amount in cents (or local equivalent) reversed (can
be less than the amount attribute on the transfer if a partial
reversal was issued).")
  (balance-transaction
   :type (or string balance-transaction null)
   :documentation "Balance transaction that describes the impact of
this transfer on your account balance.")
  (created
   :type local-time:timestamp
   :documentation "Time that this record of the transfer was first
created.")
  (currency
   :type string
   :documentation "Three-letter [ISO currency code]
(https://www.iso.org/iso-4217-currency-codes.html), in lowercase. Must
be a [supported currency](https://stripe.com/docs/currencies).")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the object. Often
useful for displaying to users.")
  (destination
   :type (or string account null)
   :documentation "ID of the Stripe account the transfer was sent to.")
  (destination-payment
   :type (or string charge null)
   :documentation "If the destination is a Stripe account, this will be
the ID of the payment that the destination account received for the
transfer.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (reversals
   :type list-transfer-reversal
   :documentation "A list of reversals that have been applied to the
transfer.")
  (reversed
   :type boolean
   :documentation "Whether the transfer has been fully reversed. If the
transfer is only partially reversed, this attribute will still be
false.")
  (source-transaction
   :type (or string charge null)
   :documentation "ID of the charge that was used to fund the transfer.
If null, the transfer was funded from the available balance.")
  (source-type
   :type (or string null)
   :initform nil
   :documentation "The source balance this transfer came from. One of
`card`, `fpx`, or `bank_account`.")
  (transfer-group
   :type (or string null)
   :documentation "A string that identifies this transaction as part of
a group. See the [Connect documentation]
(https://stripe.com/docs/connect/separate-charges-and-transfers#transfer-options)
for details."))
