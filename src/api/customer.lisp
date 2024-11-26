(in-package #:stripe)

(define-query create-customer (:type customer)
  (:post "customers")
  (address
   :documentation "The customer's address. *Required if calculating
taxes*

The `country` and `postal-code` fields are required if calculating
taxes.")
  (balance
   :type integer
   :documentation "An integer amount in cents that represents the
customer’s current balance, which affect the customer’s future
invoices. A negative amount represents a credit that decreases the
amount due on an invoice; a positive amount increases the amount due on
an invoice.")
  (cash-balance
   :documentation "Balance information and default balance settings for
this customer.")
  (coupon
   :type string
   :documentation "If you provide a coupon code, the customer will have
a discount applied on all recurring charges. Charges you create through
the API will not have the discount.")
  (description
   :type string
   :documentation "An arbitrary string that you can attach to a customer
object. It is displayed alongside the customer in the dashboard.")
  (email
   :type string
   :documentation "Customer’s email address. It’s displayed alongside
the customer in your dashboard and can be useful for searching and
tracking. This may be up to 512 characters.")
  (invoice-prefix
   :type string
   :documentation "The prefix for the customer used to generate unique
invoice numbers. Must be 3–12 uppercase letters or numbers.")
  (invoice-settings
   :documentation "Default invoice settings for this customer.")
  (metadata
   :documentation "Set of key-value pairs that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format. Individual keys can be unset by posting
an empty value to them. All keys can be unset by posting an empty value
to metadata.")
  (next-invoice-sequence
   :type integer
   :documentation "The sequence to be used on the customer’s next
invoice. Defaults to 1.")
  (name
   :type string
   :documentation "The customer’s full name or business name.")
  (payment-method
   :type string
   :documentation "The ID of the PaymentMethod to attach to the
customer.")
  (phone
   :type string
   :documentation "The customer’s phone number.")
  (preferred-locales
   :type (vector string)
   :documentation "Customer’s preferred languages, ordered by
preference.")
  (promotion-code
   :type string
   :documentation "The ID of a promotion code to apply to the customer.
The customer will have a discount applied on all recurring payments.
Charges you create through the API will not have the discount.")
  (shipping
   :documentation "The customer’s shipping information. Appears on
invoices emailed to this customer.

The `address` and `name` field are required.")
  (source
   :type string
   :documentation "When using payment sources created via the Token or
Sources APIs, passing source will create a new source object, make it
the new customer default source, and delete the old customer default if
one exists. If you want to add additional sources instead of replacing
the existing default, use the card creation API. Whenever you attach a
card to a customer, Stripe will automatically validate the card.")
  (tax
   :documentation "Tax details about the customer. *Recommended if
calculating taxes*")
  (tax-exempt
   :type string
   :documentation "The customer’s tax exemption. One of `none`,
`exempt`, or `reverse`.")
  (tax-id-data
   :documentation "The customer’s tax IDs.")
  (test-clock
   :type string
   :documentation "ID of the test clock to attach to the customer."))

(define-query retrieve-customer (:type customer)
  "Retrieve a customer by ID.

Returns a Customer object. For deleted customers, returns partial
information with deleted property set to true.

Example:

  (retrieve-customer :customer \"cus_123\")"
  (:get
   "customers/~a"
   (id
    :type string
    :documentation "The ID of the customer to retrieve.")))

(define-query update-customer (:type customer)
  "Updates the specified customer by setting the values of the
parameters passed. Any parameters not provided will be left unchanged.
For example, if you pass the source parameter, that becomes the
customer’s active source (e.g., a card) to be used for all charges in
the future. When you update a customer to a new valid card source by
passing the source parameter: for each of the customer’s current
subscriptions, if the subscription bills automatically and is in the
past_due state, then the latest open invoice for the subscription with
automatic collection enabled will be retried. This retry will not count
as an automatic retry, and will not affect the next regularly scheduled
payment for the invoice. Changing the default_source for a customer
will not trigger this behavior.

This request accepts mostly the same arguments as the customer creation call."
  (:post "customers/~a" customer)
  address
  balance
  coupon
  default-source
  description
  email
  name
  phone
  shipping
  source)

(define-query delete-customer ()
  "Delete a customer by ID.

Permanently deletes a customer and cancels all active subscriptions.
This action cannot be undone. Deleted customers can still be retrieved
for historical tracking, but no further operations are allowed.

Returns an object with deleted set to true on success.

Example:
  (delete-customer :customer \"cus_123\")"
  (:delete
   "customers/~a"
   (id
    :type string
    :documentation "The ID of the customer to delete.")))

(define-query list-customers (:type vector)
  "List all customers, sorted by creation date (most recent first).

Returns a paginated list of customer objects."
  (:get "customers")
  (created
   :type customer-date-filter
   :documentation "Only return customers that were created during the
given date interval.")
  (email
   :type string
   :documentation "A case-sensitive filter on the list based on the
customer’s email field. The value must be a string.")
  (ending-before
   :type string
   :documentation "A cursor for use in pagination. ending_before is an
object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, starting with obj_bar,
your subsequent call can include ending_before=obj_bar in order to
fetch the previous page of the list.")
  (limit
   :type integer
   :documentation "A limit on the number of objects to be returned.
Limit can range between 1 and 100, and the default is 10.")
  (starting-after
   :type string
   :documentation "A cursor for use in pagination. starting_after is an
object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, ending with obj_foo, your
subsequent call can include starting_after=obj_foo in order to fetch
the next page of the list.")
  (test-clock
   :type string
   :documentation "Provides a list of customers that are associated
with the specified test clock. The response will not include customers
with test clocks if this parameter is not set."))
