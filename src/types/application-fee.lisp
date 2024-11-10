(in-package #:stripe)

(define-object application-fee ()
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "application_fee"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (account
   :type (or string account-list)
   :documentation "ID of the Stripe account this fee was taken from.")
  (amount
   :type integer
   :documentation "Amount earned, in cents (or local equivalent).")
  (amount-refunded
   :type integer
   :documentation "Amount in cents (or local equivalent) refunded (can
be less than the amount attribute on the fee if a partial refund was
issued).")
  (application
   :type (or string application-list)
   :documentation "ID of the Connect application that earned the fee.")
  (balance-transaction
   :type (or string balance-transaction-list null)
   :documentation "Balance transaction that describes the impact of
this collected application fee on your account balance (not including
refunds).")
  (charge
   :type (or string charge-list)
   :documentation "ID of the charge that the application fee was taken from.")
  (created
   :type local-time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (currency
   :type string
   :documentation "Three-letter [ISO currency code]
(https://www.iso.org/iso-4217-currency-codes.html), in lowercase. Must
be a [supported currency](https://stripe.com/docs/currencies).")
  (fee-source
   :type (or fee-source-list null)
   :documentation "Polymorphic source of the application fee. Includes
the ID of the object the application fee was created from.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (originating-transaction
   :type (or string charge-list null)
   :documentation "ID of the corresponding charge on the platform
account, if this fee was the result of a charge using the `destination`
parameter.")
  (refunded
   :type boolean
   :documentation "Whether the fee has been fully refunded. If the fee
is only partially refunded, this attribute will still be false.")
  (refunds
   :documentation "A list of refunds that have been applied to the
fee."))

(define-object fee-source ()
  (charge
   :type string
   :initform nil
   :documentation "Charge ID that created this application fee.")
  (payout
   :type string
   :initform nil
   :documentation "Payout ID that created this application fee.")
  (type
   :type string
   :documentation "Type of object that created the application fee,
either `charge` or `payout`."))
