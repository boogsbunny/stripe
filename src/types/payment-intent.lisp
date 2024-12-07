(in-package :stripe)

(define-object payment-intent ()
  "A PaymentIntent guides you through the process of collecting a
payment from your customer. We recommend that you create exactly one
PaymentIntent for each order or customer session in your system. You
can reference the PaymentIntent later to see the history of payment
attempts for a particular session.

A PaymentIntent transitions through [multiple statuses]
(https://stripe.com/docs/payments/intents#intent-statuses) throughout
its lifetime as it interfaces with Stripe.js to perform authentication
flows and ultimately creates at most one successful charge.

Related guide: [Payment Intents API]
(https://stripe.com/docs/payments/payment-intents)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "payment_intent"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (amount
   :type integer
   :documentation "Amount intended to be collected by this
PaymentIntent. A positive integer representing how much to charge in
the [smallest currency unit]
(https://stripe.com/docs/currencies#zero-decimal) (e.g., 100 cents to
charge $1.00 or 100 to charge ¥100, a zero-decimal currency). The
minimum amount is $0.50 US or [equivalent in charge currency]
(https://stripe.com/docs/currencies#minimum-and-maximum-charge-amounts).
The amount value supports up to eight digits (e.g., a value of 99999999
for a USD charge of $999,999.99).")
  (amount-capturable
   :type integer
   :documentation "Amount that can be captured from this
PaymentIntent.")
  (amount-details)
  (amount-received
   :type integer
   :documentation "Amount that this PaymentIntent collects.")
  (application
   :type (or string application null)
   :documentation "ID of the Connect application that created the
PaymentIntent.")
  (application-fee-amount
   :type (or integer null)
   :documentation "The amount of the application fee (if any) that will
be requested to be applied to the payment and transferred to the
application owner's Stripe account. The amount of the application fee
collected will be capped at the total payment amount. For more
information, see the PaymentIntents [use case for connected accounts]
(https://stripe.com/docs/payments/connected-accounts).")
  (automatic-payment-methods
   :documentation "The amount of the application fee (if any) that will
be requested to be applied to the payment and transferred to the
application owner's Stripe account. The amount of the application fee
collected will be capped at the total payment amount. For more
information, see the PaymentIntents [use case for connected accounts]
(https://stripe.com/docs/payments/connected-accounts).")
  (canceled-at
   :type (or integer null)
   :documentation "Populated when `status` is `canceled`, this is the
time at which the PaymentIntent was canceled. Measured in seconds since
the Unix epoch.")
  (cancellation-reason
   :documentation "Reason for cancellation of this PaymentIntent,
either user-provided (`duplicate`, `fraudulent`,
`requested_by_customer`, or `abandoned`) or generated by Stripe
internally (`failed_invoice`, `void_invoice`, or `automatic`).")
  (capture-method
   :documentation "Controls when the funds will be captured from the
customer's account.")
  (client-secret
   :type (or string null)
   :documentation "The client secret of this PaymentIntent. Used for
client-side retrieval using a publishable key.

The client secret can be used to complete a payment from your frontend.
It should not be stored, logged, or exposed to anyone other than the
customer. Make sure that you have TLS enabled on any page that includes
the client secret.

Refer to our docs to [accept a payment]
(https://stripe.com/docs/payments/accept-a-payment?ui=elements) and
learn about how `client_secret` should be handled.")
  (confirmation-method
   :documentation "Describes whether we can confirm this PaymentIntent
automatically, or if it requires customer action to confirm the
payment.")
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
   :type (or string customer deleted-customer null)
   :documentation "ID of the Customer this PaymentIntent belongs to,
if one exists.

Payment methods attached to other Customers cannot be used with this
PaymentIntent.

If [setup_future_usage]
(https://stripe.com/docs/api#payment_intent_object-setup_future_usage)
is set and this PaymentIntent's payment method is not `card_present`,
then the payment method attaches to the Customer after the
PaymentIntent has been confirmed and any required actions from the user
are complete. If the payment method is `card_present` and isn't a
digital wallet, then a [generated_card]
(https://docs.stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card is created and attached to the
Customer instead.")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the object. Often
useful for displaying to users.")
  (invoice
   :type (or string invoice null)
   :documentation "ID of the invoice that created this PaymentIntent,
if it exists.")
  (last-payment-error
   :documentation "The payment error encountered in the previous
PaymentIntent confirmation. It will be cleared if the PaymentIntent is
later updated for any reason.")
  (latest-charge
   :type (or string charge null)
   :documentation "ID of the latest [Charge object]
(https://stripe.com/docs/api/charges) created by this PaymentIntent.
This property is `null` until PaymentIntent confirmation is attempted.")
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
  (next-action
   :documentation "If present, this property tells you what actions
you need to take in order for your customer to fulfill a payment using
the provided source.")
  (on-behalf-of
   :type (or string account null)
   :documentation "The account (if any) for which the funds of the
PaymentIntent are intended. See the PaymentIntents [use case for
connected accounts]
(https://stripe.com/docs/payments/connected-accounts) for details.")
  (payment-method
   :documentation "ID of the payment method used in this PaymentIntent."))
