(in-package #:stripe)

(define-query create-refund (:type refund)
  "When you create a new refund, you must specify a Charge or a
PaymentIntent object on which to create it.

Creating a new refund will refund a charge that has previously been
created but not yet refunded. Funds will be refunded to the credit or
debit card that was originally charged.

You can optionally refund only part of a charge. You can do so multiple
times, until the entire charge has been refunded.

Once entirely refunded, a charge can’t be refunded again. This method
will raise an error when called on an already-refunded charge, or when
trying to refund more money than is left on a charge."
  (:post "refunds")
  (amount
   :type integer
   :documentation "Amount to be used for this test cash balance
transaction. A positive integer representing how much to fund in the
smallest currency unit (e.g., 100 cents to fund $1.00 or 100 to fund
¥100, a zero-decimal currency).")
  (charge
   :type string
   :documentation "The identifier of the charge to refund.")
  (metadata
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format. Individual keys can be unset by posting
an empty value to them. All keys can be unset by posting an empty value
o `metadata`.")
  (payment-intent
   :type string
   :documentation "The identifier of the PaymentIntent to refund.")
  (reason
   :type string
   :documentation "String indicating the reason for the refund. If set,
possible values are `duplicate`, `fraudulent`, and
`requested_by_customer`. If you believe the charge to be `fraudulent`,
specifying fraudulent as the reason will add the associated card and
email to your [block lists](https://docs.stripe.com/radar/lists), and
will also help us improve our fraud detection algorithms.")
  (instructions-email
   :type string
   :documentation "For payment methods without native refund support
(e.g., Konbini, PromptPay), use this email from the customer to receive
refund instructions.")
  (origin
   :type string
   :documentation "String indicating the origin of a refund. It’s used
when the refund originates from a
[Customer Balance](https://docs.stripe.com/payments/customer-balance/refunding#create-return-dashboard--api)
instead of from a Charge or PaymentIntent. If this value is provided, a
Charge or PaymentIntent identifier is not required. One of `customer_balance`.")
  (refund-application-fee
   :type boolean
   :documentation "Boolean indicating whether the application fee
should be refunded when refunding this charge. If a full charge refund
is given, the full application fee will be refunded. Otherwise, the
application fee will be refunded in an amount proportional to the
amount of the charge refunded. An application fee can be refunded only
by the application that created the charge.")
  (reverse-transfer
   :type boolean
   :documentation "Boolean indicating whether the transfer should be
reversed when refunding this charge. The transfer will be reversed
proportionally to the amount being refunded (either the entire or
partial amount).A transfer can be reversed only by the application that
created the charge."))

(define-query update-refund (:type refund)
  "Updates the refund that you specify by setting the values of the
passed parameters. Any parameters that you don’t provide remain
unchanged.

This request only accepts metadata as an argument."
  (:post
   "refunds/~a"
   (id
    :type string
    :documentation "The ID of the refund to update."))
  (metadata
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format. Individual keys can be unset by posting
an empty value to them. All keys can be unset by posting an empty value
o `metadata`."))

(define-query retrieve-refund (:type refund)
  "Retrieves the details of an existing refund."
  (:get
   "refunds/~a"
   (id
    :type string
    :documentation "The ID of the refund to retrieve.")))

(define-query list-refunds (:type vector)
  "Returns a list of all refunds you created. We return the refunds in
sorted order, with the most recent refunds appearing first. The 10 most
recent refunds are always available by default on the Charge object."
  (:get "refunds")
  (charge
   :type string
   :documentation "The identifier of the charge to refund.")
  (payment-intent
   :type string
   :documentation "The identifier of the PaymentIntent to refund.")
  (created
   :type api-date-filter
   :documentation "Only return file links that were created during the
given date interval.")
  (ending-before
   :type string
   :documentation "A cursor for use in pagination. `ending_before` is
an object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, starting with `obj_bar`,
your subsequent call can include `ending_before=obj_bar` in order to
fetch the previous page of the list.")
  (limit
   :type integer
   :documentation "A limit on the number of objects to be returned.
Limit can range between 1 and 100, and the default is 10.")
  (starting-after
   :type string
   :documentation "A cursor for use in pagination. `starting_after` is
an object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, ending with `obj_foo`,
your subsequent call can include `starting_after=obj_foo` in order to
fetch the next page of the list."))

(define-query cancel-refund (:type refund)
  "Cancels a refund with a status of `requires_action`.

You can’t cancel refunds in other states. Only refunds for payment
methods that require customer action can enter the `requires_action`
state."
  (:get
   "refunds/~a/cancel"
   (id
    :type string
    :documentation "The ID of the refund to cancel.")))
