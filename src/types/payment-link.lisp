(in-package #:stripe)

(define-object payment-link ()
  "A payment link is a shareable URL that will take your customers to
a hosted payment page. A payment link can be shared and used multiple
times.

When a customer opens a payment link it will open a new
[checkout session](https://stripe.com/docs/api/checkout/sessions)
to render the payment page. You can use
[checkout session events](https://stripe.com/docs/api/events/types#event_types-checkout.session.completed)
to track payments through payment links.

Related guide: [Payment Links API](https://stripe.com/docs/payment-links)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "payment_link"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (active
   :type boolean
   :documentation "Whether the payment link's `url` is active. If
`false`, customers visiting the URL will be shown a page saying that
the link has been deactivated.")
  (after-completion
   :type payment-link-after-completion)
  (allow-promotion-codes
   :type boolean
   :documentation "Whether user redeemable promotion codes are enabled.")
  (application
   :type (or string application deleted-application null)
   :documentation "The ID of the Connect application that created the
Payment Link.")
  (application-fee-amount
   :type (or integer null)
   :documentation "The amount of the application fee (if any) that will
be requested to be applied to the payment and transferred to the
application owner's Stripe account.")
  (application-fee-percent
   :type (or real null)
   :documentation "The percentage of the subscription invoice total
that will be transferred to the application owner's Stripe account.")
  (automatic-tax
   :type payment-link-automatic-tax)
  (billing-address-collection
   :type string
   :documentation "Configuration for collecting the customer's billing
address. Defaults to `auto`. One of `auto` or `required`.")
  (consent-collection
   :type (or payment-link-consent-collection null)
   :documentation "When set, provides configuration to gather active
consent from customers.")
  (currency
   :type string
   :documentation "Three-letter [ISO currency code](https://www.iso.org/iso-4217-currency-codes.html),
in lowercase. Must be a
[supported currency](https://stripe.com/docs/currencies).")
  (custom-fields
   :type (vector payment-link-custom-field)
   :documentation "Collect additional information from your customer
using custom fields. Up to 3 fields are supported.")
  (custom-text
   :type payment-link-custom-text)
  (customer-creation
   :type string
   :documentation "Configuration for Customer creation during checkout.
One of `always` or `if_required`.")
  (inactive-message
   :type (or string null)
   :documentation "The custom message to be displayed to a customer
when a payment link is no longer active.")
  (invoice-creation
   :type (or payment-link-invoice-creation null)
   :documentation "Configuration for creating invoice for payment mode
payment links.")
  (line-items
   :type (or list-line-item null)
   :documentation "The line items representing what is being sold.")
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
  (on-behalf-of
   :type (or string account null)
   :documentation "The account on behalf of which to charge.")
  (payment-intent-data
   :type (or payment-link-payment-intent-data null)
   :documentation "Indicates the parameters to be passed to
PaymentIntent creation during checkout.")
  (payment-method-collection
   :type string
   :documentation "Configuration for collecting a payment method during
checkout. Defaults to `always`. One of `always` or `if_required`.")
  (payment-method-types
   :type (or (vector string) null)
   :documentation "The list of payment method types that customers can
use. When `null`, Stripe will dynamically show relevant payment methods
you've enabled in your
[payment method settings](https://dashboard.stripe.com/settings/payment_methods).

One of `affirm`, `afterpay_clearpay`, `alipay`, `alma`,
`au_becs_debit`, `bacs_debit`, `bancontact`, `blik`, `boleto`, `card`,
`cashapp`, `eps`, `fpx`, `giropay`, `grabpay`, `ideal`, `klarna`,
`konbini`, `link`, `mobilepay`, `multibanco`, `oxxo`, `p24`, `paynow`,
`paypal`, `pix`, `promptpay`, `sepa_debit`, `sofort`, `swish`, `twint`,
`us_bank_account`, `wechat_pay`, or `zip`.")
  (phone-number-collection
   :type payment-link-phone-number-collection)
  (restrictions
   :type (or payment-link-restrictions null)
   :documentation "Settings that restrict the usage of a payment link.")
  (shipping-address-collection
   :type (or payment-link-shipping-address-collection null)
   :documentation "Configuration for collecting the customer's shipping
address.")
  (shipping-options
   :type (vector payment-link-shipping-option)
   :documentation "The shipping rate options applied to the session.")
  (submit-type
   :type string
   :documentation "Indicates the type of transaction being performed
which customizes relevant text on the page, such as the submit button.
One of `auto`, `book`, `donate`, or `pay`.")
  (subscription-data
   :type (or payment-link-subscription-data null)
   :documentation "When creating a subscription, the specified
configuration data will be used. There must be at least one line item
with a recurring price to use `subscription_data`.")
  (tax-id-collection
   :type payment-link-tax-id-collection)
  (transfer-data
   :type (or payment-link-transfer-data null)
   :documentation "The account (if any) the payments will be attributed
to for tax reporting, and where funds from each payment will be
transferred to.")
  (url
   :type string
   :documentation "The public URL that can be shared with customers.")
  (:list-type t))

(defmethod initialize-instance :after ((instance payment-link) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:after-completion
           (unless (eql 'null value)
             (setf (slot-value instance '%after-completion)
                   (make-instance 'payment-link-after-completion :data value))))
          (:application
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%application)
                   (make-instance (if (gethash :deleted value)
                                      'deleted-application
                                      'application)
                                  :data value))))
          (:automatic-tax
           (unless (eql 'null value)
             (setf (slot-value instance '%automatic-tax)
                   (make-instance 'payment-link-automatic-tax :data value))))
          (:consent-collection
           (unless (eql 'null value)
             (setf (slot-value instance '%consent-collection)
                   (make-instance 'payment-link-consent-collection :data value))))
          (:custom-fields
           (unless (eql 'null value)
             (setf (slot-value instance '%custom-fields)
                   (map 'vector
                        (lambda (field)
                          (make-instance 'payment-link-custom-field :data field))
                        value))))
          (:custom-text
           (unless (eql 'null value)
             (setf (slot-value instance '%custom-text)
                   (make-instance 'payment-link-custom-text :data value))))
          (:invoice-creation
           (unless (eql 'null value)
             (setf (slot-value instance '%invoice-creation)
                   (make-instance 'payment-link-invoice-creation :data value))))
          (:line-items
           (when value
             (let ((items-data (gethash :data value)))
               (setf (slot-value instance '%line-items)
                     (make-instance 'list-line-items
                                    :object "list"
                                    :data (map 'vector
                                               (lambda (item)
                                                 (make-instance 'line-item :data item))
                                               items-data)
                                    :has-more (gethash :has-more value)
                                    :url (gethash :url value))))))
          (:on-behalf-of
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%on-behalf-of)
                   (make-instance 'account :data value))))
          (:payment-intent-data
           (unless (eql 'null value)
             (setf (slot-value instance '%payment-intent-data)
                   (make-instance 'payment-link-payment-intent-data :data value))))
          (:phone-number-collection
           (unless (eql 'null value)
             (setf (slot-value instance '%phone-number-collection)
                   (make-instance 'payment-link-phone-number-collection :data value))))
          (:restrictions
           (unless (eql 'null value)
             (setf (slot-value instance '%restrictions)
                   (make-instance 'payment-link-restrictions :data value))))
          (:shipping-address-collection
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping-address-collection)
                   (make-instance 'payment-link-shipping-address-collection :data value))))
          (:shipping-options
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping-options)
                   (map 'vector
                        (lambda (option)
                          (make-instance 'payment-link-shipping-option :data option))
                        value))))
          (:subscription-data
           (unless (eql 'null value)
             (setf (slot-value instance '%subscription-data)
                   (make-instance 'payment-link-subscription-data :data value))))
          (:tax-id-collection
           (unless (eql 'null value)
             (setf (slot-value instance '%tax-id-collection)
                   (make-instance 'payment-link-tax-id-collection :data value))))
          (:transfer-data
           (unless (eql 'null value)
             (setf (slot-value instance '%transfer-data)
                   (make-instance 'payment-link-transfer-data :data value)))))))))

(define-object payment-link-after-completion ()
  (hosted-confirmation
   :type (or payment-link-after-completion-hosted-confirmation null))
  (redirect
   :type (or payment-link-after-completion-redirect null))
  (type
   :reader after-completion-type
   :type string
   :documentation "The specified behavior after the purchase is
complete. One of `hosted_confirmation` or `redirect`."))

(defmethod initialize-instance :after ((instance payment-link-after-completion)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:hosted-confirmation
           (unless (eql 'null value)
             (setf (slot-value instance '%hosted-confirmation)
                   (make-instance 'payment-link-after-completion-hosted-confirmation :data value))))
          (:redirect
           (unless (eql 'null value)
             (setf (slot-value instance '%redirect)
                   (make-instance 'payment-link-after-completion-redirect :data value)))))))))

(define-object payment-link-after-completion-hosted-confirmation ()
  (custom-message
   :type (or string null)
   :documentation "The custom message that is displayed to the customer
after the purchase is complete."))

(define-object payment-link-after-completion-redirect ()
  (url
   :type string
   :documentation "The URL the customer will be redirected to after the
purchase is complete."))

(define-object payment-link-automatic-tax ()
  (enabled
   :type boolean
   :documentation "If `true`, tax will be calculated automatically
using the customer's location.")
  (liability
   :type (or payment-link-automatic-tax-liability null)
   :documentation "The account that's liable for tax. If set, the
business address and tax registrations required to perform the tax
calculation are loaded from this account. The tax transaction is
returned in the report of the connected account."))

(defmethod initialize-instance :after ((instance payment-link-automatic-tax)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:liability
           (unless (eql 'null value)
             (setf (slot-value instance '%liability)
                   (make-instance 'payment-link-automatic-tax-liability :data value)))))))))

(define-object payment-link-automatic-tax-liability ()
  (account
   :type (or string account null)
   :documentation "The connected account being referenced when `type`
is `account`.")
  (type
   :reader liability-type
   :type string
   :documentation "Type of the account referenced. One of `account` or
`self`."))

(defmethod initialize-instance :after ((instance payment-link-automatic-tax-liability)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:account
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%account)
                   (make-instance 'account :data value)))))))))

(define-object payment-link-consent-collection ()
  (payment-method-reuse-agreement
   :type (or payment-link-consent-collection-payment-method-reuse-agreement null)
   :documentation "Settings related to the payment method reuse text
shown in the Checkout UI.")
  (promotions
   :type (or string null)
   :documentation "If set to `auto`, enables the collection of customer
consent for promotional communications. One of `auto` or `none`.")
  (terms-of-service
   :type (or string null)
   :documentation "If set to `required`, it requires cutomers to accept
the terms of service before being able to pay. If set to `none`,
customers won't be shown a checkbox to accept the terms of service.
One of `none` or `required`."))

(defmethod initialize-instance :after ((instance payment-link-consent-collection)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:payment-method-reuse-agreement
           (unless (eql 'null value)
             (setf (slot-value instance '%payment-method-reuse-agreement)
                   (make-instance 'payment-link-consent-collection-payment-method-reuse-agreement
                                  :data value)))))))))

(define-object payment-link-consent-collection-payment-method-reuse-agreement ()
  (position
   :reader agreement-position
   :type string
   :documentation "Determines the position and visibility of the
payment method reuse agreement in the UI. One of `auto` or `hidden`."))

(define-object payment-link-custom-field ()
  (dropdown
   :type (or payment-link-custom-field-dropdown null))
  (key
   :type string
   :documentation "String of your choice that your integration can use
to reconcile this field. Must be unique to this field, alphanumeric,
and up to 200 characters.")
  (label
   :type payment-link-custom-field-label)
  (numeric
   :type (or payment-link-custom-field-numeric null))
  (optional
   :type boolean
   :documentation "Whether the customer is required to complete the
field before completing the Checkout Session. Defaults to `false`.")
  (text
   :type (or payment-link-custom-field-text null))
  (type
   :reader custom-field-type
   :type string
   :documentation "The type of the field. One of `dropdown`, `numeric`,
or `text`."))

(defmethod initialize-instance :after ((instance payment-link-custom-field)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:dropdown
           (unless (eql 'null value)
             (setf (slot-value instance '%dropdown)
                   (make-instance 'payment-link-custom-field-dropdown :data value))))
          (:label
           (unless (eql 'null value)
             (setf (slot-value instance '%label)
                   (make-instance 'payment-link-custom-field-label :data value))))
          (:numeric
           (unless (eql 'null value)
             (setf (slot-value instance '%numeric)
                   (make-instance 'payment-link-custom-field-numeric :data value))))
          (:text
           (unless (eql 'null value)
             (setf (slot-value instance '%text)
                   (make-instance 'payment-link-custom-field-text :data value)))))))))

(define-object payment-link-custom-field-dropdown ()
  (options
   :type (vector payment-link-custom-field-dropdown-option)
   :documentation "The options available for the customer to select. Up
to 200 options allowed."))

(defmethod initialize-instance :after ((instance payment-link-custom-field-dropdown)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:options
           (unless (eql 'null value)
             (setf (slot-value instance '%options)
                   (map 'vector
                        (lambda (option)
                          (make-instance 'payment-link-custom-field-dropdown-option :data option))
                        value)))))))))

(define-object payment-link-custom-field-dropdown-option ()
  (label
   :type string
   :documentation "The label for the option, displayed to the customer.
Up to 10 characters.")
  (value
   :type string
   :documentation "The value for this option, not displayed to the
customer, used by your integration to reconcile the option selected by
the customer. Must be unique to this option, alphanumeric, and up to
100 characters."))

(define-object payment-link-custom-field-label ()
  (custom
   :type (or string null)
   :documentation "Custom text for the label, displayed to the
customer. Up to 50 characters.")
  (type
   :reader label-type
   :type string
   :initform "custom"
   :documentation "The type of the label."))

(define-object payment-link-custom-field-numeric ()
  (maximum-length
   :type (or integer null)
   :documentation "The maximum character length constraint for the
customer's input.")
  (minimum-length
   :type (or integer null)
   :documentation "The minimum character length requirement for the
customer's input."))

(define-object payment-link-custom-field-text ()
  (maximum-length
   :type (or integer null)
   :documentation "The maximum character length constraint for the
customer's input.")
  (minimum-length
   :type (or integer null)
   :documentation "The minimum character length requirement for the
customer's input."))

(define-object payment-link-custom-text ()
  (after-submit
   :type (or payment-link-custom-text-after-submit null)
   :documentation "Custom text that should be displayed after the
payment confirmation button.")
  (shipping-address
   :type (or payment-link-custom-text-shipping-address null)
   :documentation "Custom text that should be displayed alongside
shipping address collection.")
  (submit
   :type (or payment-link-custom-text-submit null)
   :documentation "Custom text that should be displayed alongside the
payment confirmation button.")
  (terms-of-service-acceptance
   :type (or payment-link-custom-text-terms-of-service-acceptance null)
   :documentation "Custom text that should be displayed in place of the
default terms of service agreement text."))

(defmethod initialize-instance :after ((instance payment-link-custom-text)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:after-submit
           (unless (eql 'null value)
             (setf (slot-value instance '%after-submit)
                   (make-instance 'payment-link-custom-text-after-submit :data value))))
          (:shipping-address
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping-address)
                   (make-instance 'payment-link-custom-text-shipping-address :data value))))
          (:submit
           (unless (eql 'null value)
             (setf (slot-value instance '%submit)
                   (make-instance 'payment-link-custom-text-submit :data value))))
          (:terms-of-service-acceptance
           (unless (eql 'null value)
             (setf (slot-value instance '%terms-of-service-acceptance)
                   (make-instance 'payment-link-custom-text-terms-of-service-acceptance
                                  :data value)))))))))

(define-object payment-link-custom-text-after-submit ()
  (message
   :type string
   :documentation "Text may be up to 1200 characters in length."))

(define-object payment-link-custom-text-shipping-address ()
  (message
   :type string
   :documentation "Text may be up to 1200 characters in length."))

(define-object payment-link-custom-text-submit ()
  (message
   :type string
   :documentation "Text may be up to 1200 characters in length."))

(define-object payment-link-custom-text-terms-of-service-acceptance ()
  (message
   :type string
   :documentation "Text may be up to 1200 characters in length."))

(define-object payment-link-invoice-creation ()
  (enabled
   :type boolean
   :documentation "Enable creating an invoice on successful payment.")
  (invoice-data
   :type (or payment-link-invoice-creation-invoice-data null)
   :documentation "Configuration for the invoice. Default invoice
values will be used if unspecified."))

(defmethod initialize-instance :after ((instance payment-link-invoice-creation)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:invoice-data
           (unless (eql 'null value)
             (setf (slot-value instance '%invoice-data)
                   (make-instance 'payment-link-invoice-creation-invoice-data :data value)))))))))

(define-object payment-link-invoice-creation-invoice-data ()
  (account-tax-ids
   :type (or (vector (or string tax-id deleted-tax-id)) null)
   :documentation "The account tax IDs associated with the invoice.")
  (custom-fields
   :type (or (vector payment-link-invoice-creation-invoice-data-custom-field) null)
   :documentation "A list of up to 4 custom fields to be displayed on
the invoice.")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the object. Often
useful for displaying to users.")
  (footer
   :type (or string null)
   :documentation "Footer to be displayed on the invoice.")
  (issuer
   :type (or payment-link-invoice-creation-invoice-data-issuer null)
   :documentation "The connected account that issues the invoice. The
invoice is presented with the branding and support information of the
specified account.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (rendering-options
   :type (or payment-link-invoice-creation-invoice-data-rendering-options null)
   :documentation "Options for invoice PDF rendering."))

(defmethod initialize-instance :after ((instance payment-link-invoice-creation-invoice-data)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:account-tax-ids
           (unless (eql 'null value)
             (setf (slot-value instance '%account-tax-ids)
                   (map 'vector
                        (lambda (tax-id)
                          (make-instance (if (gethash :deleted tax-id)
                                             'deleted-tax-id
                                             'tax-id)
                                         :data tax-id))
                        value))))
          (:custom-fields
           (unless (eql 'null value)
             (setf (slot-value instance '%custom-fields)
                   (map 'vector
                        (lambda (field)
                          (make-instance 'payment-link-invoice-creation-invoice-data-custom-field
                                         :data field))
                        value))))
          (:issuer
           (unless (eql 'null value)
             (setf (slot-value instance '%issuer)
                   (make-instance 'payment-link-invoice-creation-invoice-data-issuer :data value))))
          (:rendering-options
           (unless (eql 'null value)
             (setf (slot-value instance '%rendering-options)
                   (make-instance 'payment-link-invoice-creation-invoice-data-rendering-options
                                  :data value)))))))))

(define-object payment-link-invoice-creation-invoice-data-custom-field ()
  (name
   :type string
   :documentation "The name of the custom field.")
  (value
   :type string
   :documentation "The value of the custom field."))

(define-object payment-link-invoice-creation-invoice-data-issuer ()
  (account
   :type (or string account null)
   :documentation "The connected account being referenced when `type`
is `account`.")
  (type
   :reader issuer-type
   :type string
   :documentation "Type of the account referenced. One of `account` or
`self`."))

(defmethod initialize-instance :after ((instance payment-link-invoice-creation-invoice-data-issuer)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:account
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%account)
                   (make-instance 'account :data value)))))))))

(define-object payment-link-invoice-creation-invoice-data-rendering-options ()
  (amount-tax-display
   :type (or string null)
   :documentation "How line-item prices and amounts will be displayed
with respect to tax on invoice PDFs."))

(define-object payment-link-payment-intent-data ()
  (capture-method
   :type (or string null)
   :documentation "Indicates when the funds will be captured from the
customer's account. One of `automatic`, `automatic_async`, or `manual`.")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the object. Often
useful for displaying to users.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with the payment method collected during checkout. One of `off_session`
or `on_session`.")
  (statement-descriptor
   :type (or string null)
   :documentation "For a non-card payment, information about the charge
that appears on the customer's statement when this payment succeeds in
creating a charge.")
  (statement-descriptor-suffix
   :type (or string null)
   :documentation "For a card payment, information about the charge
that appears on the customer's statement when this payment succeeds in
creating a charge. Concatenated with the account's statement descriptor
prefix to form the complete statement descriptor.")
  (transfer-group
   :type (or string null)
   :documentation "A string that identifies the resulting payment as
part of a group. See the PaymentIntents
[use case for connected accounts](https://stripe.com/docs/connect/separate-charges-and-transfers)
for details."))

(define-object payment-link-phone-number-collection ()
  (enabled
   :type boolean
   :documentation "If `true`, a phone number will be collected during
checkout."))

(define-object payment-link-restrictions ()
  (completed-sessions
   :type payment-link-restrictions-completed-sessions))

(defmethod initialize-instance :after ((instance payment-link-restrictions)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:completed-sessions
           (unless (eql 'null value)
             (setf (slot-value instance '%completed-sessions)
                   (make-instance 'payment-link-restrictions-completed-sessions :data value)))))))))

(define-object payment-link-restrictions-completed-sessions ()
  (count
   :reader sessions-count
   :type integer
   :documentation "The current number of checkout sessions that have
been completed on the payment link which count towards the
`completed_sessions` restriction to be met.")
  (limit
   :type integer
   :documentation "The maximum number of checkout sessions that can be
completed for the `completed_sessions` restriction to be met."))

(define-object payment-link-shipping-address-collection ()
  (allowed-countries
   :type (vector string)
   :documentation "An array of two-letter ISO country codes
representing which countries Checkout should provide as options for
shipping locations. Unsupported country codes:
`AS, CX, CC, CU, HM, IR, KP, MH, FM, NF, MP, PW, SD, SY, UM, VI`.

One of `AC`, `AD`, `AE`, `AF`, `AG`, `AI`, `AL`, `AM`, `AO`, `AQ`,
`AR`, `AT`, `AU`, `AW`, `AX`, `AZ`, `BA`, `BB`, `BD`, `BE`, `BF`, `BG`,
`BH`, `BI`, `BJ`, `BL`, `BM`, `BN`, `BO`, `BQ`, `BR`, `BS`, `BT`, `BV`,
`BW`, `BY`, `BZ`, `CA`, `CD`, `CF`, `CG`, `CH`, `CI`, `CK`, `CL`, `CM`,
`CN`, `CO`, `CR`, `CV`, `CW`, `CY`, `CZ`, `DE`, `DJ`, `DK`, `DM`, `DO`,
`DZ`, `EC`, `EE`, `EG`, `EH`, `ER`, `ES`, `ET`, `FI`, `FJ`, `FK`, `FO`,
`FR`, `GA`, `GB`, `GD`, `GE`, `GF`, `GG`, `GH`, `GI`, `GL`, `GM`, `GN`,
`GP`, `GQ`, `GR`, `GS`, `GT`, `GU`, `GW`, `GY`, `HK`, `HN`, `HR`, `HT`,
`HU`, `ID`, `IE`, `IL`, `IM`, `IN`, `IO`, `IQ`, `IS`, `IT`, `JE`, `JM`,
`JO`, `JP`, `KE`, `KG`, `KH`, `KI`, `KM`, `KN`, `KR`, `KW`, `KY`, `KZ`,
`LA`, `LB`, `LC`, `LI`, `LK`, `LR`, `LS`, `LT`, `LU`, `LV`, `LY`, `MA`,
`MC`, `MD`, `ME`, `MF`, `MG`, `MK`, `ML`, `MM`, `MN`, `MO`, `MQ`, `MR`,
`MS`, `MT`, `MU`, `MV`, `MW`, `MX`, `MY`, `MZ`, `NA`, `NC`, `NE`, `NG`,
`NI`, `NL`, `NO`, `NP`, `NR`, `NU`, `NZ`, `OM`, `PA`, `PE`, `PF`, `PG`,
`PH`, `PK`, `PL`, `PM`, `PN`, `PR`, `PS`, `PT`, `PY`, `QA`, `RE`, `RO`,
`RS`, `RU`, `RW`, `SA`, `SB`, `SC`, `SE`, `SG`, `SH`, `SI`, `SJ`, `SK`,
`SL`, `SM`, `SN`, `SO`, `SR`, `SS`, `ST`, `SV`, `SX`, `SZ`, `TA`, `TC`,
`TD`, `TF`, `TG`, `TH`, `TJ`, `TK`, `TL`, `TM`, `TN`, `TO`, `TR`, `TT`,
`TV`, `TW`, `TZ`, `UA`, `UG`, `US`, `UY`, `UZ`, `VA`, `VC`, `VE`, `VG`,
`VN`, `VU`, `WF`, `WS`, `XK`, `YE`, `YT`, `ZA`, `ZM`, `ZW`, or `ZZ`."))

(define-object payment-link-shipping-option ()
  (shipping-amount
   :type integer
   :documentation "A non-negative integer in cents representing how
much to charge.")
  (shipping-rate
   :type (or string shipping-rate)
   :documentation "The ID of the Shipping Rate to use for this shipping
option."))

(defmethod initialize-instance :after ((instance payment-link-shipping-option)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:shipping-rate
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%shipping-rate)
                   (make-instance 'shipping-rate :data value)))))))))

(define-object payment-link-subscription-data ()
  (description
   :type (or string null)
   :documentation "The subscription's description, meant to be
displayable to the customer. Use this field to optionally store an
explanation of the subscription for rendering in Stripe surfaces and
certain local payment methods UIs.")
  (invoice-settings
   :type payment-link-subscription-data-invoice-settings)
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (trial-period-days
   :type (or integer null)
   :documentation "Integer representing the number of trial period days
before the customer is charged for the first time.")
  (trial-settings
   :type (or payment-link-subscription-data-trial-settings null)
   :documentation "Settings related to subscription trials."))

(defmethod initialize-instance :after ((instance payment-link-subscription-data)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:invoice-settings
           (unless (eql 'null value)
             (setf (slot-value instance '%invoice-settings)
                   (make-instance 'payment-link-subscription-data-invoice-settings :data value))))
          (:trial-settings
           (unless (eql 'null value)
             (setf (slot-value instance '%trial-settings)
                   (make-instance 'payment-link-subscription-data-trial-settings
                                  :data value)))))))))

(define-object payment-link-subscription-data-invoice-settings ()
  (issuer
   :type payment-link-subscription-data-invoice-settings-issuer))

(defmethod initialize-instance :after ((instance payment-link-subscription-data-invoice-settings)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:issuer
           (unless (eql 'null value)
             (setf (slot-value instance '%issuer)
                   (make-instance 'payment-link-subscription-data-invoice-settings-issuer
                                  :data value)))))))))

(define-object payment-link-subscription-data-invoice-settings-issuer ()
  (account
   :type (or string account null)
   :documentation "The connected account being referenced when `type`
is `account`.")
  (type
   :reader issuer-type
   :type string
   :documentation "Type of the account referenced. One of `account` or
`self`."))

(defmethod initialize-instance :after ((instance
                                        payment-link-subscription-data-invoice-settings-issuer)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:account
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%account)
                   (make-instance 'account :data value)))))))))

(define-object payment-link-subscription-data-trial-settings ()
  (end-behavior
   :type payment-link-subscription-data-trial-settings-end-behavior
   :documentation "Defines how a subscription behaves when a free trial
ends."))

(defmethod initialize-instance :after ((instance payment-link-subscription-data-trial-settings)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:end-behavior
           (unless (eql 'null value)
             (setf (slot-value instance '%end-behavior)
                   (make-instance 'payment-link-subscription-data-trial-settings-end-behavior
                                  :data value)))))))))

(define-object payment-link-subscription-data-trial-settings-end-behavior ()
  (missing-payment-method
   :type string
   :documentation "Indicates how the subscription should change when
the trial ends if the user did not provide a payment method. One of
`cancel`, `create_invoice`, or `pause`."))

(define-object payment-link-tax-id-collection ()
  (enabled
   :type boolean
   :documentation "Indicates whether tax ID collection is enabled for
the session.")
  (required
   :type string
   :documentation "The type of tax ID collection required. One of
`if_supported` or `never`."))

(define-object payment-link-transfer-data ()
  (amount
   :type (or integer null)
   :documentation "The amount in cents (or local equivalent) that will
be transferred to the destination account. By default, the entire
amount is transferred to the destination.")
  (destination
   :type (or string account)
   :documentation "The connected account receiving the transfer."))

(defmethod initialize-instance :after ((instance payment-link-transfer-data)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:destination
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%destination)
                   (make-instance 'account :data value)))))))))
