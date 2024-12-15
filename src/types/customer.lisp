(in-package #:stripe)

(define-object customer ()
  "This object represents a customer of your business. Use it to
[create recurring charges](https://stripe.com/docs/invoicing/customer),
[save payment](https://stripe.com/docs/payments/save-during-payment)
and contact information, and track payments that belong to the same
customer."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "customer"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (address
   :type (or address null)
   :documentation "The customer's address.")
  (balance
   :type integer
   :documentation "The current balance, if any, that's stored on the
customer. If negative, the customer has credit to apply to their next
invoice. If positive, the customer has an amount owed that's added to
their next invoice. The balance only considers amounts that Stripe
hasn't successfully applied to any invoice. It doesn't reflect unpaid
invoices. This balance is only taken into account after invoices
finalize.")
  (cash-balance
   :type (or cash-balance null)
   :documentation "The current funds being held by Stripe on behalf of
the customer. You can apply these funds towards payment intents when
the source is \"cash_balance\". The `settings[reconciliation_mode]` field
describes if these funds apply to these payment intents manually or
automatically.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (currency
   :type (or string null)
   :documentation "Three-letter [ISO code for the currency]
(https://stripe.com/docs/currencies) the customer can be charged in
for recurring billing purposes.")
  (default-source
   :type (or string customer-source null)
   :documentation "ID of the default payment source for the customer.

If you use payment methods created through the PaymentMethods API, see
the [invoice_settings.default_payment_method](https://stripe.com/docs/api/customers/object#customer_object-invoice_settings-default_payment_method)
field instead.")
  (deleted
   :documentation "Indicates whether the object is deleted. Presence
indicates deletion.")
  (delinquent
   :type (or boolean null)
   :documentation "Tracks the most recent state change on any invoice
belonging to the customer. Paying an invoice or marking it
uncollectible via the API will set this field to false. An automatic
payment failure or passing the `invoice.due_date` will set this field
to `true`.

If an invoice becomes uncollectible by [dunning]
(https://stripe.com/docs/billing/automatic-collection), `delinquent`
doesn't reset to `false`.

If you care whether the customer has paid their most recent
subscription invoice, use `subscription.status` instead. Paying or
marking uncollectible any customer invoice regardless of whether it is
the latest invoice for a subscription will always set this field to
`false`.")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the object. Often
useful for displaying to users.")
  (discount
   :type (or discount null)
   :documentation "Describes the current discount active on the
customer, if there is one.")
  (email
   :type (or string null)
   :documentation "The customer's email address.")
  (invoice-credit-balance
   :type (or (hash-table :key-type string :value-type integer) null)
   :documentation "The current multi-currency balances, if any, that's
stored on the customer. If positive in a currency, the customer has a
credit to apply to their next invoice denominated in that currency. If
negative, the customer has an amount owed that's added to their next
invoice denominated in that currency. These balances don't apply to
unpaid invoices. They solely track amounts that Stripe hasn't
successfully applied to any invoice. Stripe only applies a balance in
a specific currency to an invoice after that invoice (which is in the
same currency) finalizes.")
  (invoice-prefix
   :type (or string null)
   :documentation "The prefix for the customer used to generate unique
invoice numbers.")
  (invoice-settings
   :type customer-invoice-settings)
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about
the object in a structured format.")
  (name
   :type (or string null)
   :documentation "The customer's full name or business name.")
  (next-invoice-sequence
   :type (or integer null)
   :documentation "The suffix of the customer's next invoice number
(for example, 0001). When the account uses account level sequencing,
this parameter is ignored in API requests and the field omitted in API
responses.")
  (phone
   :type (or string null)
   :documentation "The customer's phone number.")
  (preferred-locales
   :type (or (vector string) null)
   :documentation "The customer's preferred locales (languages),
ordered by preference.")
  (shipping
   :type (or shipping null)
   :documentation "Mailing and shipping address for the customer.
Appears on invoices emailed to this customer.")
  (sources
   :type (or list-customer null)
   :documentation "The customer's payment sources, if any.")
  (subscriptions
   :type (or list-subscription null)
   :documentation "The customer's current subscriptions, if any.")
  (tax
   :type (or customer-tax null))
  (tax-exempt
   :type (or string null)
   :documentation "Describes the customer's tax exemption status, which
is `none`, `exempt`, or `reverse`. When set to `reverse`, invoice and
receipt PDFs include the following text: **\"Reverse charge\"**.")
  (tax-ids
   :type (or list-customer-tax-id null)
   :documentation "The customer's tax IDs.")
  (test-clock
   :type (or string test-clock null)
   :documentation "ID of the test clock that this customer belongs to."))

(define-object customer-invoice-settings ()
  (custom-fields
   :type (or (vector customer-invoice-settings-custom-field) null)
   :documentation "Default custom fields to be displayed on invoices
for this customer.")
  (default-payment-method
   :type (or string payment-method null)
   :documentation "ID of a payment method that's attached to the
customer, to be used as the customer's default payment method for
subscriptions and invoices.")
  (footer
   :type (or string null)
   :documentation "Default footer to be displayed on invoices for this
customer.")
  (rendering-options
   :type (or customer-invoice-settings-rendering-options null)
   :documentation "Default options for invoice PDF rendering for this
customer."))

(define-object customer-invoice-settings-custom-field ()
  (name
   :type string
   :documentation "The name of the custom field.")
  (value
   :type string
   :documentation "The value of the custom field."))

(define-object customer-invoice-settings-rendering-options ()
  (amount-tax-display
   :type (or string null)
   :documentation "How line-item prices and amounts will be displayed
with respect to tax on invoice PDFs.")
  (template
   :type (or string null)
   :documentation "ID of the invoice rendering template to be used for
this customer's invoices. If set, the template will be used on all
invoices for this customer unless a template is set directly on the
invoice."))

(deftype customer-source ()
  '(or account bank-account card source))

(defclass list-customer-source (stripe-object)
  ((%object
    :reader object
    :initarg :object
    :type string
    :initform "list")
   (%data
    :reader data
    :initarg :data
    :type (vector customer-source)
    :documentation "The array of objects contained in the list.")
   (%has-more
    :reader has-more
    :initarg :has-more
    :type boolean
    :documentation "Indicates whether there are more items beyond the
ones in this list.")
   (%url
    :reader url
    :initarg :url
    :type string
    :documentation "The URL where this list can be accessed.")))

(define-object customer-tax ()
  (automatic-tax
   :type string
   :documentation "Surfaces if automatic tax computation is possible
given the current customer location information. One of `failed`,
`not_collecting`, `supported`, or `unrecognized_location`.")
  (ip-address
   :type (or string null)
   :documentation "A recent IP address of the customer used for tax
reporting and tax location inference.")
  (location
   :type (or customer-tax-location null)
   :documentation "The customer's location as identified by Stripe Tax."))

(define-object customer-tax-location ()
  (country
   :type string
   :documentation "The customer's country as identified by Stripe Tax.")
  (source
   :type string
   :documentation "The data source used to infer the customer's
location. One of `billing_address`, `ip_address`, `payment_method`,
or `shipping_destination`.")
  (state
   :documentation "The customer's state, county, province, or region
as identified by Stripe Tax."))

(define-object deleted-customer ()
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "customer"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (deleted
   :type boolean
   :initform t
   :documentation "Always true for a deleted object."))

(defmethod initialize-instance :after ((instance customer) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created)
                 (decode-timestamp value)))
          (:address
           (unless (eql 'null value)
             (setf (slot-value instance '%address)
                   (make-instance 'address :data value))))
          (:cash-balance
           (when value
             (setf (slot-value instance '%cash-balance)
                   (make-instance 'cash-balance :data value))))
          (:default-source
           (when (and value (not (eql value 'null)) (not (stringp value)))
             (let ((type (gethash :object value)))
               (setf (slot-value instance '%default-source)
                     (make-instance
                      (case type
                        ("account" 'account)
                        ("bank_account" 'bank-account)
                        ("card" 'card)
                        ("source" 'source)
                        (t (error "Unknown source type: ~A" type)))
                      :data value)))))
          (:discount
           (unless (eql 'null value)
             (setf (slot-value instance '%discount)
                   (make-instance 'discount :data value))))
          (:invoice-settings
           (when value
             (setf (slot-value instance '%invoice-settings)
                   (make-instance 'customer-invoice-settings :data value))))
          (:preferred-locales
           (when value
             (setf (slot-value instance '%preferred-locales)
                   (coerce value 'vector))))
          (:shipping
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping)
                   (make-instance 'shipping :data value))))
          (:sources
           (when value
             (let ((source-data (gethash :data value)))
               (setf (slot-value instance '%sources)
                     (make-instance 'list-customer-source
                                    :object "list"
                                    :data (map 'vector
                                               (lambda (item)
                                                 (let ((type (gethash :object item)))
                                                   (make-instance
                                                    (case type
                                                      ("account" 'account)
                                                      ("bank_account" 'bank-account)
                                                      ("card" 'card)
                                                      ("source" 'source)
                                                      (t (error "Unknown source type: ~A" type)))
                                                    :data item)))
                                               source-data)
                                    :has-more (gethash :has_more value)
                                    :url (gethash :url value))))))
          (:subscriptions
           (unless (eql 'null value)
             (setf (slot-value instance '%subscriptions)
                   (make-instance 'list-subscription :data value))))
          (:tax
           (when value
             (setf (slot-value instance '%tax)
                   (make-instance 'customer-tax :data value))))
          (:tax-ids
           (unless (eql 'null value)
             (setf (slot-value instance '%tax-ids)
                   (make-instance 'list-customer-tax-id :data value))))
          (:test-clock
           (when (and value
                      (not (eql value 'null))
                      (not (stringp value)))
             (setf (slot-value instance '%test-clock)
                   (make-instance 'test-clock :data value)))))))))
