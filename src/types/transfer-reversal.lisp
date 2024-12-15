(in-package #:stripe)

(define-object transfer-reversal ()
  "[Stripe Connect](https://stripe.com/docs/connect) platforms can
reverse transfers made to a connected account, either entirely or
partially, and can also specify whether to refund any related
application fees. Transfer reversals add to the platform's balance and
subtract from the destination account's balance.

Reversing a transfer that was made for a [destination charge]
(https://stripe.com/docs/connect/destination-charges) is allowed only
up to the amount of the charge. It is possible to reverse a
[transfer_group](https://stripe.com/docs/connect/separate-charges-and-transfers#transfer-options)
transfer only if the destination account has enough balance to cover
the reversal.

Related guide: [Reverse transfers]
(https://stripe.com/docs/connect/separate-charges-and-transfers#reverse-transfers)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "transfer_reversal"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (amount
   :type integer
   :documentation "Amount, in cents (or local equivalent).")
  (balance-transaction
   :type (or string balance-transaction null)
   :documentation "Balance transaction that describes the impact on
your account balance.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (currency
   :type string
   :documentation "Three-letter [ISO currency code]
(https://www.iso.org/iso-4217-currency-codes.html), in lowercase. Must
be a [supported currency](https://stripe.com/docs/currencies).")
  (destination-payment-refund
   :type (or string refund null)
   :documentation "Linked payment refund for the transfer reversal.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (source-refund
   :type (or string refund null)
   :documentation "ID of the refund responsible for the transfer
reversal.")
  (transfer
   :type (or string transfer)
   :documentation "ID of the transfer that was reversed."))
