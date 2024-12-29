(in-package #:stripe)

(define-query update-charge (:type charge)
  (:post "charges/~a"
         (id
          :type string
          :documentation "The ID of the charge to update."))
  (customer
   :documentation "The ID of an existing customer that will be
associated with this request. This field may only be updated if there
is no existing associated customer with this charge.")
  (description
   :type string
   :documentation "An arbitrary string which you can attach to a charge
object. It is displayed when in the web interface alongside the charge.
Note that if you use Stripe to send automatic email receipts to your
customers, your receipt emails will include the `description` of the
charge(s) that they are describing.")
  (metadata
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format. Individual keys can be unset by posting
an empty value to them. All keys can be unset by posting an empty value
o `metadata`.")
  (receipt-email
   :type string
   :documentation "This is the email address that the receipt for this
charge will be sent to. If this field is updated, then a new email
receipt will be sent to the updated address.")
  (shipping
   :type shipping
   :documentation "Shipping information for the charge. Helps prevent
fraud on charges for physical goods.")
  ;; TODO: specify type
  (fraud-details
   :documentation "A set of key-value pairs you can attach to a charge
giving information about its riskiness. If you believe a charge is
fraudulent, include a `user_report` key with a value of `fraudulent`.
If you believe a charge is safe, include a `user_report` key with a
value of `safe`. Stripe will use the information you send to improve
our fraud detection algorithms.")
  (transfer-group
   :type string
   :documentation "A string that identifies this transaction as part of
a group. `transfer_group` may only be provided if it has not been set.
See the [Connect documentation](https://docs.stripe.com/connect/separate-charges-and-transfers#transfer-options)
for details."))

(define-query retrieve-charge (:type charge)
  "Retrieves the details of a charge that has previously been created.
Supply the unique charge ID that was returned from your previous
request, and Stripe will return the corresponding charge information.
The same information is returned when creating or refunding the charge."
  (:get "charges/~a"
        (id
         :type string
         :documentation "The ID of the charge to retrieve.")))

(define-query list-charges (:type vector)
  "Returns a list of charges you’ve previously created. The charges are
returned in sorted order, with the most recent charges appearing first."
  (:get "charges")
  (customer
   :type string
   :documentation "Only return charges for the customer specified by
this customer ID.")
  (created
   :type api-date-filter
   :documentation "Only return charges that were created during the
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
  (payment-intent
   :type string
   :documentation "Only return charges that were created by the
PaymentIntent specified by this PaymentIntent ID.")
  (starting-after
   :type string
   :documentation "A cursor for use in pagination. `starting_after` is
an object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, ending with `obj_foo`,
your subsequent call can include `starting_after=obj_foo` in order to
fetch the next page of the list.")
  (transfer-group
   :type string
   :documetation "Only return charges for this transfer group, limited
to 100."))

(define-query capture-charge (:type charge)
  "Capture the payment of an existing, uncaptured charge that was
created with the capture option set to false.

Uncaptured payments expire a set number of days after they are created
([7 by default](https://docs.stripe.com/charges/placing-a-hold)), after
which they are marked as refunded and capture attempts will fail.

Don’t use this method to capture a PaymentIntent-initiated charge. Use
[Capture a PaymentIntent](https://docs.stripe.com/api/payment_intents/capture)."
  (:post "charges/~a/capture"
         (id
          :type string
          :documentation "The ID of the charge to capture."))
  (amount
    :type integer
    :documentation "The amount to capture, which must be less than or
equal to the original amount. Any additional amount will be
automatically refunded.")
  (receipt-email
   :type string
   :documentation "The email address to send this charge’s receipt to.
This will override the previously-specified email address for this
charge, if one was set. Receipts will not be sent in test mode.")
  (statement-descriptor
   :type string
   :documentation "For a non-card charge, text that appears on the
customer’s statement as the statement descriptor. This value overrides
the account’s default statement descriptor. For information about
requirements, including the 22-character limit, see the
[Statement Descriptor docs](https://docs.stripe.com/get-started/account/statement-descriptors).")
  (statement-descriptor-suffix
   :type string
   :documentation "Provides information about a card charge.
Concatenated to the account’s
[statement descriptor prefix](https://docs.stripe.com/get-started/account/statement-descriptors#static)
to form the complete statement descriptor that appears on the
customer’s statement. If the account has no prefix value, the suffix is
concatenated to the account’s statement descriptor.")
  (application-fee-amount
   :type integer
   :documentation "An application fee amount to add on to this charge,
which must be less than or equal to the original amount.")
  ;; TODO: specify type
  (transfer-data
   :documentation "An optional dictionary including the account to
automatically transfer to as part of a destination charge.
[See the Connect documentation](https://docs.stripe.com/connect/destination-charges)
for details.")
  (transfer-group
   :type string
   :documentation "A string that identifies this transaction as part of
a group. transfer_group may only be provided if it has not been set.
See the [Connect documentation](https://docs.stripe.com/connect/separate-charges-and-transfers#transfer-options)
for details."))

(define-query search-charge (:type vector)
  "Search for charges you’ve previously created using Stripe’s
[Search Query Language](https://docs.stripe.com/search#search-query-language).
Don’t use search in read-after-write flows where strict consistency is
necessary. Under normal operating conditions, data is searchable in
less than a minute. Occasionally, propagation of new or updated data
can be up to an hour behind during outages. Search functionality is not
available to merchants in India."
  (:get "charges/search")
  (query
   :type string
   :required t
   :documentation "The search query string. See
[search query language](https://docs.stripe.com/search#search-query-language)
and the list of supported
[query fields for charges](https://docs.stripe.com/search#query-fields-for-charges).")
  (limit
   :type integer
   :documentation "A limit on the number of objects to be returned.
Limit can range between 1 and 100, and the default is 10.")
  (page
   :type string
   :documentation "A cursor for pagination across multiple pages of
results. Don’t include this parameter on the first call. Use the
next_page value returned in a previous response to request subsequent
results."))
