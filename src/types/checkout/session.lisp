(in-package #:stripe)

(define-object session ()
  "A Checkout Session represents your customer's session as they pay
for one-time purchases or subscriptions through
[Checkout](https://stripe.com/docs/payments/checkout) or
[Payment Links](https://stripe.com/docs/payments/payment-links). We
recommend creating a new Session each time your customer attempts to
pay.

Once payment is successful, the Checkout Session will contain a
reference to the [Customer](https://stripe.com/docs/api/customers), and
either the successful
[PaymentIntent](https://stripe.com/docs/api/payment_intents) or an
active [Subscription](https://stripe.com/docs/api/subscriptions).

You can create a Checkout Session on your server and redirect to its
URL to begin Checkout.

Related guide: [Checkout quickstart](https://stripe.com/docs/checkout/quickstart)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "checkout.session"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (after-expiration
   :type (or session-after-expiration null)
   :documentation "When set, provides configuration for actions to take
if this Checkout Session expires.")
  (allow-promotion-codes
   :type (or boolean null)
   :documentation "Enables user redeemable promotion codes.")
  (amount-subtotal
   :type (or integer null)
   :documentation "Total of all items before discounts or taxes are
applied.")
  (amount-total
   :type (or integer null)
   :documentation "Total of all items after discounts and taxes are
applied.")
  (automatic-tax
   :type session-automatic-tax)
  (billing-address-collection
   :type (or string null)
   :documentation "Describes whether Checkout should collect the
customer's billing address. Defaults to `auto`. One of `auto` or
`required`.")
  (cancel-url
   :type (or string null)
   :documentation "If set, Checkout displays a back button and
customers will be directed to this URL if they decide to cancel payment
and return to your website.")
  (client-reference-id
   :type (or string null)
   :documentation "A unique string to reference the Checkout Session.
This can be a customer ID, a cart ID, or similar, and can be used to
reconcile the Session with your internal systems.")
  (client-secret
   :type (or string null)
   :documentation "Client secret to be used when initializing Stripe.js
embedded checkout.")
  (consent
   :type (or session-consent null)
   :documentation "Results of `consent_collection` for this session.")
  ;; NOTE: named `session-consent-list` instead of `session-consent-collection` to avoid
  ;; name clash with collection type in macro of `define-object`.
  (consent-collection
   :type (or session-consent-list null)
   :documentation "When set, provides configuration for the Checkout
Session to gather active consent from customers.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (currency
   :type (or string null)
   :documentation "Three-letter [ISO currency code](https://www.iso.org/iso-4217-currency-codes.html),
in lowercase. Must be a
[supported currency](https://stripe.com/docs/currencies).")
  (currency-conversion
   :type (session-currency-conversion null)
   :documentation "Currency conversion details for
[Adaptive Pricing](https://docs.stripe.com/payments/checkout/adaptive-pricing)
sessions.")
  (custom-fields
   :type (vector session-custom-field)
   :documentation "Collect additional information from your customer
using custom fields. Up to 3 fields are supported.")
  (custom-text
   :type session-custom-text)
  (customer
   :type (or string customer deleted-customer null)
   :documentation "The ID of the customer for this Session. For
Checkout Sessions in `subscription` mode or Checkout Sessions with
`customer_creation` set as `always` in `payment` mode, Checkout will
create a new customer object based on information provided during the
payment flow unless an existing customer was provided when the Session
was created.")
  (customer-creation
   :type (or string null)
   :documentation "Configure whether a Checkout Session creates a
Customer when the Checkout Session completes. One of `always` or
`if_required`.")
  (customer-details
   :type (or session-customer-details null)
   :documentation "The customer details including the customer's tax
exempt status and the customer's tax IDs. Customer's address details
are not present on Sessions in `setup` mode.")
  (customer-email
   :type (or string null)
   :documentation "If provided, this value will be used when the
Customer object is created. If not provided, customers will be asked to
enter their email address. Use this parameter to prefill customer data
if you already have an email on file. To access information about the
customer once the payment flow is complete, use the `customer`
attribute.")
  (expires-at
   :type time:timestamp
   :documentation "The timestamp at which the Checkout Session will
expire.")
  (invoice
   :type (or string invoice null)
   :documentation "ID of the invoice created by the Checkout Session,
if it exists.")
  (invoice-creation
   :type (session-invoice-creation)
   :documentation "Details on the state of invoice creation for the
Checkout Session.")
  (line-items
   :type list-line-item
   :documentation "The line items purchased by the customer.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (locale
   :type (or string null)
   :documentation "The IETF language tag of the locale Checkout is
displayed in. If blank or `auto`, the browser's locale is used.

One of `auto`, `bg`, `cs`, `da`, `de`, `el`, `en`, `en-GB`, `es`,
`es-419`, `et`, `fi`, `fil`, `fr`, `fr-CA`, `hr`, `hu`, `id`, `it`,
`ja`, `ko`, `lt`, `lv`, `ms`, `mt`, `nb`, `nl`, `pl`, `pt`, `pt-BR`,
`ro`, `ru`, `sk`, `sl`, `sv`, `th`, `tr`, `vi`, `zh`, `zh-HK`, or
`zh-TW`.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (mode
   :type string
   :documentation "The mode of the Checkout Session. One of `payment`,
`setup`, or `subscription`.")
  (payment-intent
   :type (or string payment-intent null)
   :documentation "The ID of the PaymentIntent for Checkout Sessions in
`payment` mode. You can't confirm or cancel the PaymentIntent for a
Checkout Session. To cancel, [expire the Checkout Session](https://stripe.com/docs/api/checkout/sessions/expire)
instead.")
  (payment-link
   :type (or string payment-link null)
   :documentation "The ID of the Payment Link that created this
Session.")
  (payment-method-collection
   :type (or string null)
   :documentation "Configure whether a Checkout Session should collect
a payment method. Defaults to `always`. One of `always` or
`if_required`.")
  (payment-method-configuration-details
   :type (or session-payment-method-configuration-details null)
   :documentation "Information about the payment method configuration
used for this Checkout session if using dynamic payment methods.")
  (payment-method-options
   :type (or session-payment-method-options null)
   :documentation "Payment-method-specific configuration for the
PaymentIntent or SetupIntent of this CheckoutSession.")
  (payment-method-types
   :type (vector string)
   :documentation "A list of the types of payment methods (e.g. card)
this Checkout Session is allowed to accept.")
  (payment-status
   :type string
   :documentation "The payment status of the Checkout Session, one of
`paid`, `unpaid`, or `no_payment_required`. You can use this value to
decide when to fulfill your customer's order.")
  (phone-number-collection
   :type (or session-phone-number-collection null))
  (recovered-from
   :type (or string null)
   :documentation "The ID of the original expired Checkout Session that
triggered the recovery flow.")
  (redirect-on-completion
   :type (or string null)
   :documentation "This parameter applies to `ui_mode: embedded`. Learn
more about the [redirect behavior](https://stripe.com/docs/payments/checkout/custom-success-page?payment-ui=embedded-form)
of embedded sessions. Defaults to `always`. One of `always`,
`if_required`, or `never`.")
  (return-url
   :type (or string null)
   :documentation "Applies to Checkout Sessions with
`ui_mode: embedded`. The URL to redirect your customer back to after
they authenticate or cancel their payment on the payment method's app
or site.")
  (saved-payment-method-options
   :type (or session-saved-payment-method-options null)
   :documentation "Controls saved payment method settings for the
session. Only available in `payment` and `subscription` mode.")
  (setup-intent
   :type (or string setup-intent null)
   :documentation "The ID of the SetupIntent for Checkout Sessions in
`setup` mode. You can't confirm or cancel the SetupIntent for a
Checkout Session. To cancel, [expire the Checkout Session](https://stripe.com/docs/api/checkout/sessions/expire)
instead.")
  (shipping-address-collection
   :type (or session-shipping-address-collection null)
   :documentation "When set, provides configuration for Checkout to collect a shipping address from a customer.")
  (shipping-cost
   :type (or session-shipping-cost null)
   :documentation "The details of the customer cost of shipping,
including the customer chosen ShippingRate.")
  (shipping-details
   :type (or session-shipping-details null)
   :documentation "Shipping information for this Checkout Session.")
  (shipping-options
   :type (vector session-shipping-option)
   :documentation "The shipping rate options applied to this Session.")
  (status
   :type (or string null)
   :documentation "The status of the Checkout Session, one of `open`,
`complete`, or `expired`.")
  (submit-type
   :type (or string null)
   :documentation "Describes the type of transaction being performed by
Checkout in order to customize relevant text on the page, such as the
submit button. `submit_type` can only be specified on Checkout Sessions
in `payment` mode. If blank or `auto`, `pay` is used. One of `auto`,
`book`, `donate`, or `pay`.")
  (subscription
   :type (or string subscription null)
   :documentation "The ID of the subscription for Checkout Sessions in
`subscription` mode.")
  (success-url
   :type (or string null)
   :documentation "The URL the customer will be directed to after the
payment or subscription creation is successful.")
  (tax-id-collection
   :type (or session-tax-id-collection null))
  (total-details
   :type (or session-total-details null)
   :documentation "Tax and discount details for the computed total
amount.")
  (ui-mode
   :type (or string null)
   :documentation "The UI mode of the Session. Defaults to `hosted`.
One of `embedded` or `hosted`.")
  (url
   :type (or string null)
   :documentation "The URL to the Checkout Session. Redirect customers
to this URL to take them to Checkout. If you're using
[Custom Domains](https://stripe.com/docs/payments/checkout/custom-domains),
the URL will use your subdomain. Otherwise, it'll use
`checkout.stripe.com.`

This value is only present when the session is active.")
  (:list-type t))

(defmethod initialize-instance :after ((instance session) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:after-expiration
           (unless (eql 'null value)
             (setf (slot-value instance '%after-expiration)
                   (make-instance 'session-after-expiration :data value))))
          (:automatic-tax
           (setf (slot-value instance '%automatic-tax)
                 (make-instance 'session-automatic-tax :data value)))
          (:consent
           (unless (eql 'null value)
             (setf (slot-value instance '%consent)
                   (make-instance 'session-consent :data value))))
          (:consent-collection
           (unless (eql 'null value)
             (setf (slot-value instance '%consent-collection)
                   (make-instance 'session-consent-list :data value))))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:currency-conversion
           (unless (eql 'null value)
             (setf (slot-value instance '%currency-conversion)
                   (make-instance 'session-currency-conversion :data value))))
          (:custom-fields
           (setf (slot-value instance '%custom-fields)
                 (map 'vector
                      (lambda (field-data)
                        (make-instance 'session-custom-field :data field-data))
                      value)))
          (:custom-text
           (setf (slot-value instance '%custom-text)
                 (make-instance 'session-custom-text :data value)))
          (:customer
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%customer)
                   (make-instance (if (gethash :deleted value)
                                      'deleted-customer
                                      'customer)
                                  :data value))))
          (:customer-details
           (unless (eql 'null value)
             (setf (slot-value instance '%customer-details)
                   (make-instance 'session-customer-details :data value))))
          (:expires-at
           (setf (slot-value instance '%expires-at) (decode-timestamp value)))
          (:invoice
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%invoice)
                   (make-instance 'invoice :data value))))
          (:invoice-creation
           (setf (slot-value instance '%invoice-creation)
                 (make-instance 'session-invoice-creation :data value)))
          (:line-items
           (setf (slot-value instance '%line-items)
                 (make-instance 'list-line-item :data value)))
          (:payment-intent
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%payment-intent)
                   (make-instance 'payment-intent :data value))))
          (:payment-link
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%payment-link)
                   (make-instance 'payment-link :data value))))
          (:payment-method-configuration-details
           (unless (eql 'null value)
             (setf (slot-value instance '%payment-method-configuration-details)
                   (make-instance 'session-payment-method-configuration-details :data value))))
          (:payment-method-options
           (unless (eql 'null value)
             (setf (slot-value instance '%payment-method-options)
                   (make-instance 'session-payment-method-options :data value))))
          (:phone-number-collection
           (unless (eql 'null value)
             (setf (slot-value instance '%phone-number-collection)
                   (make-instance 'session-phone-number-collection :data value))))
          (:saved-payment-method-options
           (unless (eql 'null value)
             (setf (slot-value instance '%saved-payment-method-options)
                   (make-instance 'session-saved-payment-method-options :data value))))
          (:setup-intent
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%setup-intent)
                   (make-instance 'setup-intent :data value))))
          (:shipping-address-collection
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping-address-collection)
                   (make-instance 'session-shipping-address-collection :data value))))
          (:shipping-cost
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping-cost)
                   (make-instance 'session-shipping-cost :data value))))
          (:shipping-details
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping-details)
                   (make-instance 'session-shipping-details :data value))))
          (:shipping-options
           (setf (slot-value instance '%shipping-options)
                 (map 'vector
                      (lambda (option-data)
                        (make-instance 'session-shipping-option :data option-data))
                      value)))
          (:subscription
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%subscription)
                   (make-instance 'subscription :data value))))
          (:tax-id-collection
           (unless (eql 'null value)
             (setf (slot-value instance '%tax-id-collection)
                   (make-instance 'session-tax-id-collection :data value))))
          (:total-details
           (unless (eql 'null value)
             (setf (slot-value instance '%total-details)
                   (make-instance 'session-total-details :data value)))))))))

(define-object session-after-expiration ()
  (recovery
   :type (or session-after-expiration-recovery null)
   :documentation "When set, configuration used to recover the Checkout
Session on expiry."))

(defmethod initialize-instance :after ((instance session-after-expiration)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:recovery
           (unless (eql 'null value)
             (setf (slot-value instance '%recovery)
                   (make-instance 'session-after-expiration-recovery :data value)))))))))

(define-object session-after-expiration-recovery ()
  (allow-promotion-codes
   :type boolean
   :documentation "Enables user redeemable promotion codes on the
recovered Checkout Sessions. Defaults to `false`.")
  (enabled
   :type boolean
   :documentation "If `true`, a recovery url will be generated to
recover this Checkout Session if it expires before a transaction is
completed. It will be attached to the Checkout Session object upon
expiration.")
  (expires-at
   :type (or time:timestamp null)
   :documentation "The timestamp at which the recovery URL will expire.")
  (url
   :type (or string null)
   :documentation "URL that creates a new Checkout Session when clicked
that is a copy of this expired Checkout Session."))

(defmethod initialize-instance :after ((instance session-after-expiration-recovery)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:expires-at
           (setf (slot-value instance '%expires-at) (decode-timestamp value))))))))

(define-object session-automatic-tax ()
  (enabled
   :type boolean
   :documentation "Indicates whether automatic tax is enabled for the
session.")
  (liability
   :type (or session-automatic-tax-liability null)
   :documentation "The account that's liable for tax. If set, the
business address and tax registrations required to perform the tax
calculation are loaded from this account. The tax transaction is
returned in the report of the connected account.")
  (status
   :type (or string null)
   :documentation "The status of the most recent automated tax
calculation for this session. One of `complete`, `failed`, or
`required_location_inputs`."))

(defmethod initialize-instance :after ((instance session-automatic-tax)
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
                   (make-instance 'session-automatic-tax-liability :data value)))))))))

(define-object session-automatic-tax-liability ()
  (account
   :type (or string account null)
   :documentation "The connected account being referenced when `type`
is `account`.")
  (type
   :reader liability-type
   :type string
   :documentation "Type of the account referenced. One of `account` or
`self`."))

(defmethod initialize-instance :after ((instance session-automatic-tax-liability)
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

(define-object session-consent ()
  (promotions
   :type string
   :documentation "If `opt_in`, the customer consents to receiving
promotional communications from the merchant about this Checkout
Session. One of `opt_in` or `opt_out`.")
  (terms-of-service
   :type string
   :documentation "If `accepted`, the customer in this Checkout Session
has agreed to the merchant's terms of service."))

(define-object session-consent-list ()
  (payment-method-reuse-agreement
   :type (or string null)
   :documentation "If set to `hidden`, it will hide legal text related
to the reuse of a payment method. One of `auto` or `hidden`.")
  (promotions
   :type (or string null)
   :documentation "If set to `auto`, enables the collection of customer
consent for promotional communications. The Checkout Session will
determine whether to display an option to opt into promotional
communication from the merchant depending on the customer's locale.
Only available to US merchants. One of `auto` or `none`.")
  (terms-of-service
   :type (or string null)
   :documentation "If set to `required`, it requires customers to
accept the terms of service before being able to pay. One of `required`
or `none`."))

(define-object session-currency-conversion ()
  (amount-subtotal
   :type integer
   :documentation "Total of all items in source currency before
discounts or taxes are applied.")
  (amount-total
   :type integer
   :documentation "Total of all items in source currency after
discounts and taxes are applied.")
  (fx-rate
   :type string
   :documentation "Exchange rate used to convert source currency
amounts to customer currency amounts.")
  (source-currency
   :type string
   :documentation "Creation currency of the CheckoutSession before
localization"))

(define-object session-customer-details ()
  (address
   :type (or address null)
   :documentation "The customer's address after a completed Checkout
Session. Note: This property is populated only for sessions on or after
March 30, 2022.")
  (email
   :type (or string null)
   :documentation "The email associated with the Customer, if one
exists, on the Checkout Session after a completed Checkout Session or
at time of session expiry. Otherwise, if the customer has consented to
promotional content, this value is the most recent valid email provided
by the customer on the Checkout form.")
  (name
   :type (or string null)
   :documentation "The customer's name after a completed Checkout
Session. Note: This property is populated only for sessions on or after
March 30, 2022.")
  (phone
   :type (or string null)
   :documentation "The customer's phone number after a completed
Checkout Session.")
  (tax-exempt
   :type (or string null)
   :documentation "The customer's tax exempt status after a completed
Checkout Session. One of `exempt`, `none`, or `reverse`.")
  (tax-ids
   :type (vector session-customer-details-tax-id)
   :documentation "The customer's tax IDs after a completed session."))

(defmethod initialize-instance :after ((instance session-customer-details)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:address
           (unless (eql 'null value)
             (setf (slot-value instance '%address)
                   (make-instance 'address :data value))))
          (:tax-ids
           (setf (slot-value instance '%tax-ids)
                 (map 'vector
                      (lambda (tax-id-data)
                        (make-instance 'session-customer-details-tax-id
                                       :data tax-id-data))
                      value))))))))

(define-object session-customer-details-tax-id ()
  (type
   :reader tax-id-type
   :type string
   :documentation "The type of the tax ID, one of `ad_nrt`, `ar_cuit`,
`eu_vat`, `bo_tin`, `br_cnpj`, `br_cpf`, `cn_tin`, `co_nit`, `cr_tin`,
`do_rcn`, `ec_ruc`, `eu_oss_vat`, `hr_oib`, `pe_ruc`, `ro_tin`,
`rs_pib`, `sv_nit`, `uy_ruc`, `ve_rif`, `vn_tin`, `gb_vat`, `nz_gst`,
`au_abn`, `au_arn`, `in_gst`, `no_vat`, `no_voec`, `za_vat`, `ch_vat`,
`mx_rfc`, `sg_uen`, `ru_inn`, `ru_kpp`, `ca_bn`, `hk_br`, `es_cif`,
`tw_vat`, `th_vat`, `jp_cn`, `jp_rn`, `jp_trn`, `li_uid`, `my_itn`,
`us_ein`, `kr_brn`, `ca_qst`, `ca_gst_hst`, `ca_pst_bc`, `ca_pst_mb`,
`ca_pst_sk`, `my_sst`, `sg_gst`, `ae_trn`, `cl_tin`, `sa_vat`,
`id_npwp`, `my_frp`, `il_vat`, `ge_vat`, `ua_vat`, `is_vat`, `bg_uic`,
`hu_tin`, `si_tin`, `ke_pin`, `tr_tin`, `eg_tin`, `ph_tin`, `bh_vat`,
`kz_bin`, `ng_tin`, `om_vat`, `de_stn`, `ch_uid`, `tz_vat`, `uz_vat`,
`uz_tin`, `md_vat`, `ma_vat`, `by_tin`, or `unknown`.")
  (value
   :type string
   :documentation "The value of the tax ID."))

(define-object session-custom-field ()
  (dropdown
   :type (or session-custom-field-dropdown null))
  (key
   :type string
   :documentation "String of your choice that your integration can use
to reconcile this field. Must be unique to this field, alphanumeric,
and up to 200 characters.")
  (label
   :type session-custom-field-label)
  (numeric
   :type (or session-custom-field-numeric null))
  (optional
   :type boolean
   :documentation "Whether the customer is required to complete the
field before completing the Checkout Session. Defaults to `false`.")
  (text
   :type (or session-custom-field-text null))
  (type
   :reader custom-field-type
   :type string
   :documentation "The type of the field. One of `dropdown`, `numeric`, or `text`."))

(defmethod initialize-instance :after ((instance session-custom-field) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:dropdown
           (unless (eql 'null value)
             (setf (slot-value instance '%dropdown)
                   (make-instance 'session-custom-field-dropdown :data value))))
          (:label
           (setf (slot-value instance '%label)
                 (make-instance 'session-custom-field-label :data value)))
          (:numeric
           (unless (eql 'null value)
             (setf (slot-value instance '%numeric)
                   (make-instance 'session-custom-field-numeric :data value))))
          (:text
           (unless (eql 'null value)
             (setf (slot-value instance '%text)
                   (make-instance 'session-custom-field-text :data value)))))))))

(define-object session-custom-field-dropdown ()
  (default-value
   :type (or string null)
   :documentation "The value that will pre-fill on the payment page.")
  (options
   :type (vector session-custom-field-dropdown-option)
   :documentation "The options available for the customer to select. Up
to 200 options allowed.")
  (value
   :type (or string null)
   :documentation "The option selected by the customer. This will be
the `value` for the option."))

(defmethod initialize-instance :after ((instance session-custom-field-dropdown)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:options
           (setf (slot-value instance '%options)
                 (map 'vector
                      (lambda (option-data)
                        (make-instance 'session-custom-field-dropdown-option
                                       :data option-data))
                      value))))))))

(define-object session-custom-field-dropdown-option ()
  (label
   :type string
   :documentation "The label for the option, displayed to the customer.
Up to 100 characters.")
  (value
   :type string
   :documentation "The value for this option, not displayed to the
customer, used by your integration to reconcile the option selected by
the customer. Must be unique to this option, alphanumeric, and up to
100 characters."))

(define-object session-custom-field-label ()
  (custom
   :type (or string null)
   :documentation "Custom text for the label, displayed to the
customer. Up to 50 characters.")
  (type
   :reader label-type
   :type string
   :documentation "The type of the label. Currently only `custom`."))

(define-object session-custom-field-numeric ()
  (default-value
   :type (or string null)
   :documentation "The value that will pre-fill the field on the
payment page.")
  (maximum-length
   :type (or integer null)
   :documentation "The maximum character length constraint for the
customer's input.")
  (minimum-length
   :type (or integer null)
   :documentation "The minimum character length requirement for the
customer's input.")
  (value
   :type (or string null)
   :documentation "The value entered by the customer, containing only
digits."))

(define-object session-custom-field-text ()
  (default-value
   :type (or string null)
   :documentation "The value that will pre-fill the field on the
payment page.")
  (maximum-length
   :type (or integer null)
   :documentation "The maximum character length constraint for the
customer's input.")
  (minimum-length
   :type (or integer null)
   :documentation "The minimum character length requirement for the
customer's input.")
  (value
   :type (or string null)
   :documentation "The value entered by the customer."))

(define-object session-custom-text ()
  (after-submit
   :type (or session-custom-text-after-submit null)
   :documentation "Custom text that should be displayed after the
payment confirmation button.")
  (shipping-address
   :type (or session-custom-text-shipping-address null)
   :documentation "Custom text that should be displayed alongside
shipping address collection.")
  (submit
   :type (or session-custom-text-submit null)
   :documentation "Custom text that should be displayed alongside the
payment confirmation button.")
  (terms-of-service-acceptance
   :type (or session-custom-text-terms-of-service-acceptance null)
   :documentation "Custom text that should be displayed in place of the
default terms of service agreement text."))

(defmethod initialize-instance :after ((instance session-custom-text)
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
                   (make-instance 'session-custom-text-after-submit :data value))))
          (:shipping-address
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping-address)
                   (make-instance 'session-custom-text-shipping-address :data value))))
          (:submit
           (unless (eql 'null value)
             (setf (slot-value instance '%submit)
                   (make-instance 'session-custom-text-submit :data value))))
          (:terms-of-service-acceptance
           (unless (eql 'null value)
             (setf (slot-value instance '%terms-of-service-acceptance)
                   (make-instance 'session-custom-text-terms-of-service-acceptance
                                  :data value)))))))))

(define-object session-custom-text-after-submit ()
  (message
   :type string
   :documentation "Text may be up to 1200 characters in length."))

(define-object session-custom-text-shipping-address ()
  (message
   :type string
   :documentation "Text may be up to 1200 characters in length."))

(define-object session-custom-text-submit ()
  (message
   :type string
   :documentation "Text may be up to 1200 characters in length."))

(define-object session-custom-text-terms-of-service-acceptance ()
  (message
   :type string
   :documentation "Text may be up to 1200 characters in length."))

(define-object session-invoice-creation ()
  (enabled
   :type boolean
   :documentation "Indicates whether invoice creation is enabled for
the Checkout Session.")
  (invoice-data
   :type (or session-invoice-creation-invoice-data null)))

(defmethod initialize-instance :after ((instance session-invoice-creation)
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
                   (make-instance 'session-invoice-creation-invoice-data :data value)))))))))

(define-object session-invoice-creation-invoice-data ()
  (account-tax-ids
   :type (or (vector (or string tax-id deleted-tax-id)) null)
   :documentation "The account tax IDs associated with the invoice.")
  (custom-fields
   :type (or (vector session-invoice-creation-invoice-data-custom-field) null)
   :documentation "Custom fields displayed on the invoice.")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the object. Often
useful for displaying to users.")
  (footer
   :type (or string null)
   :documentation "Footer displayed on the invoice.")
  (issuer
   :type (or session-invoice-creation-invoice-data-issuer null)
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
   :type (or session-invoice-creation-invoice-data-rendering-options null)
   :documentation "Options for invoice PDF rendering."))

(defmethod initialize-instance :after ((instance session-invoice-creation-invoice-data)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:account-tax-ids
           (when value
             (setf (slot-value instance '%account-tax-ids)
                   (map 'vector
                        (lambda (tax-id-data)
                          (if (stringp tax-id-data)
                              tax-id-data
                              (make-instance
                               (if (gethash :deleted tax-id-data)
                                   'deleted-tax-id
                                   'tax-id)
                               :data tax-id-data)))
                        value))))
          (:custom-fields
           (when value
             (setf (slot-value instance '%custom-fields)
                   (map 'vector
                        (lambda (field-data)
                          (make-instance 'session-invoice-creation-invoice-data-custom-field
                                         :data field-data))
                        value))))
          (:issuer
           (unless (eql 'null value)
             (setf (slot-value instance '%issuer)
                   (make-instance 'session-invoice-creation-invoice-data-issuer
                                  :data value))))
          (:rendering-options
           (unless (eql 'null value)
             (setf (slot-value instance '%rendering-options)
                   (make-instance 'session-invoice-creation-invoice-data-rendering-options
                                  :data value)))))))))

(define-object session-invoice-creation-invoice-data-custom-field ()
  (name
   :type string
   :documentation "The name of the custom field.")
  (value
   :type string
   :documentation "The value of the custom field."))

(define-object session-invoice-creation-invoice-data-issuer ()
  (account
   :type (or string account null)
   :documentation "The connected account being referenced when `type`
is `account`.")
  (type
   :reader issuer-type
   :type string
   :documentation "Type of the account referenced. One of `account` or
`self`."))

(defmethod initialize-instance :after ((instance session-invoice-creation-invoice-data-issuer)
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

(define-object session-invoice-creation-invoice-data-rendering-options ()
  (amount-tax-display
   :type (or string null)
   :documentation "How line-item prices and amounts will be displayed
with respect to tax on invoice PDFs."))

(define-object session-payment-method-configuration-details ()
  (id
   :type string
   :documentation "ID of the payment method configuration used.")
  (parent
   :type (or string null)
   :documentation "ID of the parent payment method configuration used."))

(define-object session-payment-method-options ()
  (accs-debit
   :type (or payment-method-options-acss-debit null))
  (affirm
   :type (or payment-method-options-affirm null))
  (afterpay-clearpay
   :type (or payment-method-options-afterpay-clearpay null))
  (alipay
   :type (or payment-method-options-alipay null))
  (amazon-pay
   :type (or payment-method-options-amazon-pay null))
  (au-becs-debit
   :type (or payment-method-options-au-becs-debit null))
  (bacs-debit
   :type (or payment-method-options-bacs-debit null))
  (bancontact
   :type (or payment-method-options-bancontact null))
  (boleto
   :type (or payment-method-options-boleto null))
  (card
   :type (or payment-method-options-card null))
  (cashapp
   :type (or payment-method-options-cashapp null))
  (customer-balance
   :type (or payment-method-options-customer-balance null))
  (eps
   :type (or payment-method-options-eps null))
  (fpx
   :type (or payment-method-options-fpx null))
  (giropay
   :type (or payment-method-options-giropay null))
  (grabpay
   :type (or payment-method-options-grabpay null))
  (ideal
   :type (or payment-method-options-ideal null))
  (kakao-pay
   :type (or payment-method-options-kakao-pay null))
  (klarna
   :type (or payment-method-options-klarna null))
  (konbini
   :type (or payment-method-options-konbini null))
  (kr-card
   :type (or payment-method-options-kr-card null))
  (link
   :type (or payment-method-options-link null))
  (mobilepay
   :type (or payment-method-options-mobilepay null))
  (multibanco
   :type (or payment-method-options-multibanco null))
  (naver-pay
   :type (or payment-method-options-naver-pay null))
  (oxxo
   :type (or payment-method-options-oxxo null))
  (p24
   :type (or payment-method-options-p24 null))
  (payco
   :type (or payment-method-options-payco null))
  (paynow
   :type (or payment-method-options-paynow null))
  (paypal
   :type (or payment-method-options-paypal null))
  (pix
   :type (or payment-method-options-pix null))
  (revolut-pay
   :type (or payment-method-options-revolut-pay null))
  (samsung-pay
   :type (or payment-method-options-samsung-pay null))
  (sepa-debit
   :type (or payment-method-options-sepa-debit null))
  (sofort
   :type (or payment-method-options-sofort null))
  (swish
   :type (or payment-method-options-swish null))
  (us-bank-account
   :type (or payment-method-options-us-bank-account null)))

(defmethod initialize-instance :after ((instance session-payment-method-options)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:accs-debit
           (unless (eql 'null value)
             (setf (slot-value instance '%accs-debit)
                   (make-instance 'payment-method-options-acss-debit :data value))))
          (:affirm
           (unless (eql 'null value)
             (setf (slot-value instance '%affirm)
                   (make-instance 'payment-method-options-affirm :data value))))
          (:afterpay-clearpay
           (unless (eql 'null value)
             (setf (slot-value instance '%afterpay-clearpay)
                   (make-instance 'payment-method-options-afterpay-clearpay :data value))))
          (:alipay
           (unless (eql 'null value)
             (setf (slot-value instance '%alipay)
                   (make-instance 'payment-method-options-alipay :data value))))
          (:amazon-pay
           (unless (eql 'null value)
             (setf (slot-value instance '%amazon-pay)
                   (make-instance 'payment-method-options-amazon-pay :data value))))
          (:au-becs-debit
           (unless (eql 'null value)
             (setf (slot-value instance '%au-becs-debit)
                   (make-instance 'payment-method-options-au-becs-debit :data value))))
          (:bacs-debit
           (unless (eql 'null value)
             (setf (slot-value instance '%bacs-debit)
                   (make-instance 'payment-method-options-bacs-debit :data value))))
          (:bancontact
           (unless (eql 'null value)
             (setf (slot-value instance '%bancontact)
                   (make-instance 'payment-method-options-bancontact :data value))))
          (:boleto
           (unless (eql 'null value)
             (setf (slot-value instance '%boleto)
                   (make-instance 'payment-method-options-boleto :data value))))
          (:card
           (unless (eql 'null value)
             (setf (slot-value instance '%card)
                   (make-instance 'payment-method-options-card :data value))))
          (:cashapp
           (unless (eql 'null value)
             (setf (slot-value instance '%cashapp)
                   (make-instance 'payment-method-options-cashapp :data value))))
          (:customer-balance
           (unless (eql 'null value)
             (setf (slot-value instance '%customer-balance)
                   (make-instance 'payment-method-options-customer-balance :data value))))
          (:eps
           (unless (eql 'null value)
             (setf (slot-value instance '%eps)
                   (make-instance 'payment-method-options-eps :data value))))
          (:fpx
           (unless (eql 'null value)
             (setf (slot-value instance '%fpx)
                   (make-instance 'payment-method-options-fpx :data value))))
          (:giropay
           (unless (eql 'null value)
             (setf (slot-value instance '%giropay)
                   (make-instance 'payment-method-options-giropay :data value))))
          (:grabpay
           (unless (eql 'null value)
             (setf (slot-value instance '%grabpay)
                   (make-instance 'payment-method-options-grabpay :data value))))
          (:ideal
           (unless (eql 'null value)
             (setf (slot-value instance '%ideal)
                   (make-instance 'payment-method-options-ideal :data value))))
          (:kakao-pay
           (unless (eql 'null value)
             (setf (slot-value instance '%kakao-pay)
                   (make-instance 'payment-method-options-kakao-pay :data value))))
          (:klarna
           (unless (eql 'null value)
             (setf (slot-value instance '%klarna)
                   (make-instance 'payment-method-options-klarna :data value))))
          (:konbini
           (unless (eql 'null value)
             (setf (slot-value instance '%konbini)
                   (make-instance 'payment-method-options-konbini :data value))))
          (:kr-card
           (unless (eql 'null value)
             (setf (slot-value instance '%kr-card)
                   (make-instance 'payment-method-options-kr-card :data value))))
          (:link
           (unless (eql 'null value)
             (setf (slot-value instance '%link)
                   (make-instance 'payment-method-options-link :data value))))
          (:mobilepay
           (unless (eql 'null value)
             (setf (slot-value instance '%mobilepay)
                   (make-instance 'payment-method-options-mobilepay :data value))))
          (:multibanco
           (unless (eql 'null value)
             (setf (slot-value instance '%multibanco)
                   (make-instance 'payment-method-options-multibanco :data value))))
          (:naver-pay
           (unless (eql 'null value)
             (setf (slot-value instance '%naver-pay)
                   (make-instance 'payment-method-options-naver-pay :data value))))
          (:oxxo
           (unless (eql 'null value)
             (setf (slot-value instance '%oxxo)
                   (make-instance 'payment-method-options-oxxo :data value))))
          (:p24
           (unless (eql 'null value)
             (setf (slot-value instance '%p24)
                   (make-instance 'payment-method-options-p24 :data value))))
          (:payco
           (unless (eql 'null value)
             (setf (slot-value instance '%payco)
                   (make-instance 'payment-method-options-payco :data value))))
          (:paynow
           (unless (eql 'null value)
             (setf (slot-value instance '%paynow)
                   (make-instance 'payment-method-options-paynow :data value))))
          (:paypal
           (unless (eql 'null value)
             (setf (slot-value instance '%paypal)
                   (make-instance 'payment-method-options-paypal :data value))))
          (:pix
           (unless (eql 'null value)
             (setf (slot-value instance '%pix)
                   (make-instance 'payment-method-options-pix :data value))))
          (:revolut-pay
           (unless (eql 'null value)
             (setf (slot-value instance '%revolut-pay)
                   (make-instance 'payment-method-options-revolut-pay :data value))))
          (:samsung-pay
           (unless (eql 'null value)
             (setf (slot-value instance '%samsung-pay)
                   (make-instance 'payment-method-options-samsung-pay :data value))))
          (:sepa-debit
           (unless (eql 'null value)
             (setf (slot-value instance '%sepa-debit)
                   (make-instance 'payment-method-options-sepa-debit :data value))))
          (:sofort
           (unless (eql 'null value)
             (setf (slot-value instance '%sofort)
                   (make-instance 'payment-method-options-sofort :data value))))
          (:swish
           (unless (eql 'null value)
             (setf (slot-value instance '%swish)
                   (make-instance 'payment-method-options-swish :data value))))
          (:us-bank-account
           (unless (eql 'null value)
             (setf (slot-value instance '%us-bank-account)
                   (make-instance 'payment-method-options-us-bank-account :data value)))))))))

(define-object payment-method-options-acss-debit ()
  (currency
   :type (or string null)
   :documentation "Currency supported by the bank account. Returned
when the Session is in `setup` mode. One of `cad` or `usd`.")
  (mandate-options
   :type (or payment-method-options-acss-debit-mandate-options null))
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none`, `off_session`, or `on_session`.")
  (verification-method
   :type (or string null)
   :documentation "Bank account verification method. One of
`automatic`, `instant`, or `microdeposits`."))

(defmethod initialize-instance :after ((instance payment-method-options-acss-debit)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:mandate-options
           (unless (eql 'null value)
             (setf (slot-value instance '%mandate-options)
                   (make-instance 'payment-method-options-acss-debit-mandate-options
                                  :data value)))))))))

(define-object payment-method-options-acss-debit-mandate-options ()
  (custom-mandate-url
   :type (or string null)
   :documentation "A URL for custom mandate text.")
  (default-for
   :type (or (vector string) null)
   :documentation "List of Stripe products where this mandate can be
selected automatically. One of `invoice` or `subscription`.")
  (interval-description
   :type (or string null)
   :documentation "Description of the interval. Only required if the
'payment_schedule' parameter is 'interval' or 'combined'.")
  (payment-schedule
   :type (or string null)
   :documentation "Payment schedule for the mandate. One of `combined`,
`interval`, or `sporadic`.")
  (transaction-type
   :type (or string null)
   :documentation "Transaction type of the mandate. One of `business`
or `personal`."))

(define-object payment-method-options-affirm ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-afterpay-clearpay ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-alipay ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-amazon-pay ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none` or `off_session`."))

(define-object payment-method-options-au-becs-debit ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-bacs-debit ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none`, `off_session`, or `on_session`."))

(define-object payment-method-options-bancontact ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-boleto ()
  (expires-after-days
   :type integer
   :documentation "The number of calendar days before a Boleto voucher
expires. For example, if you create a Boleto voucher on Monday and you
set expires_after_days to 2, the Boleto voucher will expire on
Wednesday at 23:59 America/Sao_Paulo time.")
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none`, `off_session`, or `on_session`."))

(define-object payment-method-options-card ()
  (installments
   :type (or payment-method-options-card-installments null))
  (request-three-d-secure
   :type string
   :documentation "We strongly recommend that you rely on our SCA
Engine to automatically prompt your customers for authentication based
on risk level and
[other requirements](https://stripe.com/docs/strong-customer-authentication).
However, if you wish to request 3D Secure based on logic from your own
fraud engine, provide this option. If not provided, this value defaults
to `automatic`. Read our guide on
[manually requesting 3D Secure](https://stripe.com/docs/payments/3d-secure/authentication-flow#manual-three-ds)
for more information on how this configuration interacts with Radar and
our SCA Engine.

One of `any`, `automatic`, or `challenge`.")
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none`, `off_session`, or `on_session`.")
  (statement-descriptor-suffix-kana
   :type (or string null)
   :documentation "Provides information about a card payment that
customers see on their statements. Concatenated with the Kana prefix
(shortened Kana descriptor) or Kana statement descriptor that's set on
the account to form the complete statement descriptor. Maximum 22
characters. On card statements, the *concatenation* of both prefix and
suffix (including separators) will appear truncated to 22 characters.")
  (statement-descriptor-suffix-kanji
   :type (or string null)
   :documentation "Provides information about a card payment that
customers see on their statements. Concatenated with the Kanji prefix
(shortened Kanji descriptor) or Kanji statement descriptor that's set
on the account to form the complete statement descriptor. Maximum 17
characters. On card statements, the *concatenation* of both prefix and
suffix (including separators) will appear truncated to 17 characters."))

(defmethod initialize-instance :after ((instance payment-method-options-card)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:installments
           (unless (eql 'null value)
             (setf (slot-value instance '%installments)
                   (make-instance 'payment-method-options-card-installments :data value)))))))))

(define-object payment-method-options-card-installments ()
  (enabled
   :type (or boolean null)
   :documentation "Indicates if installments are enabled."))

(define-object payment-method-options-cashapp ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-customer-balance ()
  (bank-transfer
   :type (or payment-method-options-customer-balance-bank-transfer null))
  (funding-type
   :type (or string null)
   :documentation "The funding method type to be used when there are
not enough funds in the customer balance. Permitted values include:
`bank_transfer`.")
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(defmethod initialize-instance :after ((instance payment-method-options-customer-balance)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:bank-transfer
           (unless (eql 'null value)
             (setf (slot-value instance '%bank-transfer)
                   (make-instance 'payment-method-options-customer-balance-bank-transfer
                                  :data value)))))))))

(define-object payment-method-options-customer-balance-bank-transfer ()
  (eu-bank-transfer
   :type (or payment-method-options-customer-balance-bank-transfer-eu-bank-transfer null))
  (requested-address-types
   :type (or (vector string) null)
   :documentation "List of address types that should be returned in the
financial_addresses response. If not specified, all valid types will be
returned. Permitted values include: `sort_code`, `zengin`, `iban`, or
`spei`.")
  (type
   :reader bank-transfer-type
   :type (or string null)
   :documentation "The bank transfer type that this PaymentIntent is
allowed to use for funding Permitted values include:
`eu_bank_transfer`, `gb_bank_transfer`, `jp_bank_transfer`,
`mx_bank_transfer`, or `us_bank_transfer`."))

(defmethod initialize-instance :after ((instance
                                        payment-method-options-customer-balance-bank-transfer)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:eu-bank-transfer
           (unless (eql 'null value)
             (setf (slot-value instance '%eu-bank-transfer)
                   (make-instance
                    'payment-method-options-customer-balance-bank-transfer-eu-bank-transfer
                    :data value)))))))))

(define-object payment-method-options-customer-balance-bank-transfer-eu-bank-transfer ()
  (country
   :type string
   :documentation "The desired country code of the bank account
information. Permitted values include: `BE`, `DE`, `ES`, `FR`, `IE`, or
`NL`."))

(define-object payment-method-options-eps ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-fpx ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-giropay ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-grabpay ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-ideal ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-kakao-pay ()
  (capture-method
   :type (or string null)
   :documentation "Controls when the funds will be captured from the
customer's account. Must be `manual` if provided.")
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none` or `off_session`."))

(define-object payment-method-options-klarna ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none`, `off_session`, or `on_session`."))

(define-object payment-method-options-konbini ()
  (expires-after-days
   :type (or integer null)
   :documentation "The number of calendar days (between 1 and 60) after
which Konbini payment instructions will expire. For example, if a
PaymentIntent is confirmed with Konbini and `expires_after_days` set to
2 on Monday JST, the instructions will expire on Wednesday
23:59:59 JST.")
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-kr-card ()
  (capture-method
   :type (or string null)
   :documentation "Controls when the funds will be captured from the
customer's account. Must be `manual` if provided.")
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none` or `off_session`."))

(define-object payment-method-options-link ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none` or `off_session`."))

(define-object payment-method-options-mobilepay ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-multibanco ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-naver-pay ()
  (capture-method
   :type (or string null)
   :documentation "Controls when the funds will be captured from the
customer's account. Must be `manual` if provided."))

(define-object payment-method-options-oxxo ()
  (expires-after-days
   :type integer
   :documentation "The number of calendar days before an OXXO invoice
expires. For example, if you create an OXXO invoice on Monday and you
set expires_after_days to 2, the OXXO invoice will expire on Wednesday
at 23:59 America/Mexico_City time.")
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-p24 ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-payco ()
  (capture-method
   :type (or string null)
   :documentation "Controls when the funds will be captured from the
customer's account. Must be `manual` if provided."))

(define-object payment-method-options-paynow ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-paypal ()
  (capture-method
   :type (or string null)
   :documentation "Controls when the funds will be captured from the
customer's account. Must be `manual` if provided.")
  (preferred-locale
   :type (or string null)
   :documentation "Preferred locale of the PayPal checkout page that
the customer is redirected to.")
  (reference
   :type (or string null)
   :documentation "A reference of the PayPal transaction visible to
customer which is mapped to PayPal's invoice ID. This must be a
globally unique ID if you have configured in your PayPal settings to
block multiple payments per invoice ID.")
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none` or `off_session`."))

(define-object payment-method-options-pix ()
  (expires-after-seconds
   :type (or integer null)
   :documentation "The number of seconds after which Pix payment will
expire."))

(define-object payment-method-options-revolut-pay ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none` or `off_session`."))

(define-object payment-method-options-samsung-pay ()
  (capture-method
   :type (or string null)
   :documentation "Controls when the funds will be captured from the
customer's account. Must be `manual` if provided."))

(define-object payment-method-options-sepa-debit ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none`, `off_session`, or `on_session`."))

(define-object payment-method-options-sofort ()
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

Must be `none`."))

(define-object payment-method-options-swish ()
  (reference
   :type (or string null)
   :documentation "The order reference that will be displayed to
customers in the Swish application. Defaults to the `id` of the Payment
Intent."))

(define-object payment-method-options-us-bank-account ()
  (financial-connections
   :type (or payment-method-options-us-bank-account-financial-connections null))
  (setup-future-usage
   :type (or string null)
   :documentation "Indicates that you intend to make future payments
with this PaymentIntent's payment method.

If you provide a Customer with the PaymentIntent, you can use this
parameter to
[attach the payment method](https://stripe.com/payments/save-during-payment)
to the Customer after the PaymentIntent is confirmed and the customer
completes any required actions. If you don't provide a Customer, you
can still
[attach](https://stripe.com/api/payment_methods/attach)
the payment method to a Customer after the transaction completes.

If the payment method is `card_present` and isn't a digital wallet,
Stripe creates and attaches a
[generated_card](https://stripe.com/api/charges/object#charge_object-payment_method_details-card_present-generated_card)
payment method representing the card to the Customer instead.

When processing card payments, Stripe uses `setup_future_usage` to help
you comply with regional legislation and network rules, such as
[SCA](https://stripe.com/strong-customer-authentication).

One of `none`, `off_session`, or `on_session`.")
  (verification-method
   :type (or string null)
   :documentation "Bank account verification method. One of `automatic`
or `instant`."))

(defmethod initialize-instance :after ((instance payment-method-options-us-bank-account)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:financial-connections
           (unless (eql 'null value)
             (setf (slot-value instance '%financial-connections)
                   (make-instance 'payment-method-options-us-bank-account-financial-connections
                                  :data value)))))))))

(define-object payment-method-options-us-bank-account-financial-connections ()
  (filters
   :type (or payment-method-options-us-bank-account-financial-connections-filters null))
  (permissions
   :type (or (vector string) null)
   :documentation "The list of permissions to request. The
`payment_method` permission must be included. Permitted values include:
`balances`, `ownership`, `payment_method`, or `transactions`.")
  (prefetch
   :type (or (vector string) null)
   :documentation "Data features requested to be retrieved upon account
creation. Permitted values include: `balances`, `ownership`,
`transactions`.")
  (return-url
   :type (or string null)
   :documentation "For webview integrations only. Upon completing OAuth
login in the native browser, the user will be redirected to this URL to
return to your app."))

(defmethod initialize-instance :after ((instance
                                        payment-method-options-us-bank-account-financial-connections)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:filters
           (unless (eql 'null value)
             (setf (slot-value instance '%filters)
                   (make-instance
                    'payment-method-options-us-bank-account-financial-connections-filters
                    :data value)))))))))

(define-object payment-method-options-us-bank-account-financial-connections-filters ()
  (account-subcategories
   :type (or (vector string) null)
   :documentation "The account subcategories to use to filter for
possible accounts to link. Valid subcategories are `checking` and
`savings`."))

(define-object session-phone-number-collection ()
  (enabled
   :type boolean
   :documentation "Indicates whether phone number collection is enabled
for the session."))

(define-object session-saved-payment-method-options ()
  (allow-redisplay-filters
   :type (or (vector string) null)
   :documentation "Uses the `allow_redisplay` value of each saved
payment method to filter the set presented to a returning customer. By
default, only saved payment methods with 'allow_redisplay: always' are
shown in Checkout. One of `always`, `limited`, or `unspecified`.")
  (payment-method-remove
   :type (or string null)
   :documentation "Enable customers to choose if they wish to remove
their saved payment methods. Disabled by default. One of `disabled` or
`enabled`.")
  (payment-method-save
   :type (or string null)
   :documentation "Enable customers to choose if they wish to save
their payment method for future use. Disabled by default. One of
`disabled` or `enabled`."))

(define-object session-shipping-address-collection ()
  (allowed-countries
   :type (vector string)
   :documentation "An array of two-letter ISO country codes
representing which countries Checkout should provide as options for
shipping locations. Unsupported country codes: `AS, CX, CC, CU, HM, IR,
KP, MH, FM, NF, MP, PW, SD, SY, UM, VI`.

Permitted values include:

`AC`, `AD`, `AE`, `AF`, `AG`, `AI`, `AL`, `AM`, `AO`, `AQ`, `AR`, `AT`,
`AU`, `AW`, `AX`, `AZ`, `BA`, `BB`, `BD`, `BE`, `BF`, `BG`, `BH`, `BI`,
`BJ`, `BL`, `BM`, `BN`, `BO`, `BQ`, `BR`, `BS`, `BT`, `BV`, `BW`, `BY`,
`BZ`, `CA`, `CD`, `CF`, `CG`, `CH`, `CI`, `CK`, `CL`, `CM`, `CN`, `CO`,
`CR`, `CV`, `CW`, `CY`, `CZ`, `DE`, `DJ`, `DK`, `DM`, `DO`, `DZ`, `EC`,
`EE`, `EG`, `EH`, `ER`, `ES`, `ET`, `FI`, `FJ`, `FK`, `FO`, `FR`, `GA`,
`GB`, `GD`, `GE`, `GF`, `GG`, `GH`, `GI`, `GL`, `GM`, `GN`, `GP`, `GQ`,
`GR`, `GS`, `GT`, `GU`, `GW`, `GY`, `HK`, `HN`, `HR`, `HT`, `HU`, `ID`,
`IE`, `IL`, `IM`, `IN`, `IO`, `IQ`, `IS`, `IT`, `JE`, `JM`, `JO`, `JP`,
`KE`, `KG`, `KH`, `KI`, `KM`, `KN`, `KR`, `KW`, `KY`, `KZ`, `LA`, `LB`,
`LC`, `LI`, `LK`, `LR`, `LS`, `LT`, `LU`, `LV`, `LY`, `MA`, `MC`, `MD`,
`ME`, `MF`, `MG`, `MK`, `ML`, `MM`, `MN`, `MO`, `MQ`, `MR`, `MS`, `MT`,
`MU`, `MV`, `MW`, `MX`, `MY`, `MZ`, `NA`, `NC`, `NE`, `NG`, `NI`, `NL`,
`NO`, `NP`, `NR`, `NU`, `NZ`, `OM`, `PA`, `PE`, `PF`, `PG`, `PH`, `PK`,
`PL`, `PM`, `PN`, `PR`, `PS`, `PT`, `PY`, `QA`, `RE`, `RO`, `RS`, `RU`,
`RW`, `SA`, `SB`, `SC`, `SE`, `SG`, `SH`, `SI`, `SJ`, `SK`, `SL`, `SM`,
`SN`, `SO`, `SR`, `SS`, `ST`, `SV`, `SX`, `SZ`, `TA`, `TC`, `TD`, `TF`,
`TG`, `TH`, `TJ`, `TK`, `TL`, `TM`, `TN`, `TO`, `TR`, `TT`, `TV`, `TW`,
`TZ`, `UA`, `UG`, `US`, `UY`, `UZ`, `VA`, `VC`, `VE`, `VG`, `VN`, `VU`,
`WF`, `WS`, `XK`, `YE`, `YT`, `ZA`, `ZM`, `ZW`, and `ZZ`."))

(define-object session-shipping-cost ()
  (amount-subtotal
   :type integer
   :documentation "Total shipping cost before any discounts or taxes
are applied.")
  (amount-tax
   :type integer
   :documentation "Total tax amount applied due to shipping costs. If
no tax was applied, defaults to 0.")
  (amount-total
   :type integer
   :documentation "Total shipping cost after discounts and taxes are
applied.")
  (shipping-rate
   :type (or string shipping-rate null)
   :documentation "The ID of the ShippingRate for this order.")
  (taxes
   :type (or (vector session-shipping-cost-tax) null)
   :documentation "The taxes applied to the shipping rate."))

(defmethod initialize-instance :after ((instance session-shipping-cost) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:shipping-rate
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%shipping-rate)
                   (make-instance 'shipping-rate :data value))))
          (:taxes
           (when value
             (setf (slot-value instance '%taxes)
                   (map 'vector
                        (lambda (tax-data)
                          (make-instance 'session-shipping-cost-tax
                                         :data tax-data))
                        value)))))))))

(define-object session-shipping-cost-tax ()
  (amount
   :type integer
   :documentation "Amount of tax applied for this rate.")
  (rate
   :type tax-rate
   :documentation "Tax rates can be applied to
[invoices](https://stripe.com/docs/billing/invoices/tax-rates),
[subscriptions](https://stripe.com/docs/billing/subscriptions/taxes)
and [Checkout Sessions](https://stripe.com/docs/payments/checkout/set-up-a-subscription#tax-rates)
to collect tax.

Related guide: [Tax rates](https://stripe.com/docs/billing/taxes/tax-rates)")
  (taxability-reason
   :type (or string null)
   :documentation "The reasoning behind this tax, for example, if the
product is tax exempt. The possible values for this field may be
extended as new tax rules are supported.

One of `customer_exempt`, `not_collecting`, `not_subject_to_tax`,
`not_supported`, `portion_product_exempt`, `portion_reduced_rated`,
`portion_standard_rated`, `product_exempt`, `product_exempt_holiday`,
`proportionally_rated`, `reduced_rated`, `reverse_charge`,
`standard_rated`, `taxable_basis_reduced`, or `zero_rated`.")
  (taxable-amount
   :type (or integer null)
   :documentation "The amount on which tax is calculated, in cents (or
local equivalent)."))

(defmethod initialize-instance :after ((instance session-shipping-cost-tax)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:rate
           (setf (slot-value instance '%rate)
                 (make-instance 'tax-rate :data value))))))))

(define-object session-shipping-details ()
  (address
   :type (or address null))
  (carrier
   :type (or string null)
   :documentation "The delivery service that shipped a physical
product, such as Fedex, UPS, USPS, etc.")
  (name
   :type (or string null)
   :documentation "Recipient name.")
  (phone
   :type (or string null)
   :documentation "Recipient phone (including extension).")
  (tracking-number
   :type (or string null)
   :documentation "The tracking number for a physical product, obtained
from the delivery service. If multiple tracking numbers were generated
for this purchase, please separate them with commas."))

(defmethod initialize-instance :after ((instance session-shipping-details)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:address
           (unless (eql 'null value)
             (setf (slot-value instance '%address)
                   (make-instance 'address :data value)))))))))

(define-object session-shipping-option ()
  (shipping-amount
   :type integer
   :documentation "A non-negative integer in cents representing how
much to charge.")
  (shipping-rate
   :type (or string shipping-rate)
   :documentation "The shipping rate."))

(defmethod initialize-instance :after ((instance session-shipping-option)
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

(define-object session-tax-id-collection ()
  (enabled
   :type boolean
   :documentation "Indicates whether tax ID collection is enabled for
the session.")
  (required
   :type string
   :documentation "Indicates whether a tax ID is required on the
payment page. One of `if_supported` or `never`."))

(define-object session-total-details ()
  (amount-discount
   :type integer
   :documentation "This is the sum of all the discounts.")
  (amount-shipping
   :type (or integer null)
   :documentation "This is the sum of all the shipping amounts.")
  (amount-tax
   :type integer
   :documentation "This is the sum of all the tax amounts.")
  (breakdown
   :type (or session-total-details-breakdown null)))

(defmethod initialize-instance :after ((instance session-total-details) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:breakdown
           (unless (eql 'null value)
             (setf (slot-value instance '%breakdown)
                   (make-instance 'session-total-details-breakdown :data value)))))))))

(define-object session-total-details-breakdown ()
  (discounts
   :type (vector session-total-details-breakdown-discount)
   :documentation "The aggregated discounts.")
  (taxes
   :type (vector session-total-details-breakdown-tax)
   :documentation "The aggregated tax amounts by rate."))

(defmethod initialize-instance :after ((instance session-total-details-breakdown)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:discounts
           (setf (slot-value instance '%discounts)
                 (map 'vector
                      (lambda (discount-data)
                        (make-instance 'session-total-details-breakdown-discount
                                       :data discount-data))
                      value)))
          (:taxes
           (setf (slot-value instance '%taxes)
                 (map 'vector
                      (lambda (tax-data)
                        (make-instance 'session-total-details-breakdown-tax
                                       :data tax-data))
                      value))))))))

(define-object session-total-details-breakdown-discount ()
  (amount
   :type integer
   :documentation "The amount discounted.")
  (discount
   :type discount
   :documentation "A discount represents the actual application of a
[coupon](https://stripe.com/docs/api#coupons) or
[promotion code](https://stripe.com/docs/api#promotion_codes). It
contains information about when the discount began, when it will end,
and what it is applied to.

Related guide: [Applying discounts to subscriptions](https://stripe.com/docs/billing/subscriptions/discounts)"))

(defmethod initialize-instance :after ((instance session-total-details-breakdown-discount)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:discount
           (setf (slot-value instance '%discount)
                 (make-instance 'discount :data value))))))))

(define-object session-total-details-breakdown-tax ()
  (amount
   :type integer
   :documentation "Amount of tax applied for this rate.")
  (rate
   :type tax-rate
   :documentation "Tax rates can be applied to
[invoices](https://stripe.com/docs/billing/invoices/tax-rates),
[subscriptions](https://stripe.com/docs/billing/subscriptions/taxes)
and [Checkout Sessions](https://stripe.com/docs/payments/checkout/set-up-a-subscription#tax-rates)
to collect tax.

Related guide: [Tax rates](https://stripe.com/docs/billing/taxes/tax-rates)")
  (taxability-reason
   :type (or string null)
   :documentation "The reasoning behind this tax, for example, if the
product is tax exempt. The possible values for this field may be
extended as new tax rules are supported. One of `customer_exempt`,
`not_collecting`, `not_subject_to_tax`, `not_supported`,
`portion_product_exempt`, `portion_reduced_rated`,
`portion_standard_rated`, `product_exempt`, `product_exempt_holiday`,
`proportionally_rated`, `reduced_rated`, `reverse_charge`,
`standard_rated`, `taxable_basis_reduced`, or `zero_rated`.")
  (taxable-amount
   :type (or integer null)
   :documentation "The amount on which tax is calculated, in cents (or
local equivalent)."))

(defmethod initialize-instance :after ((instance session-total-details-breakdown-tax)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:rate
           (setf (slot-value instance '%rate)
                 (make-instance 'tax-rate :data value))))))))
