(in-package #:stripe)

(define-object setup-attempt ()
  "A SetupAttempt describes one attempted confirmation of a
SetupIntent, whether that confirmation is successful or unsuccessful.
You can use SetupAttempts to inspect details of a specific attempt at
setting up a payment method using a SetupIntent."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "setup_attempt"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (application
   :type (or string application null)
   :documentation "The value of [application](https://stripe.com/docs/api/setup_intents/object#setup_intent_object-application)
on the SetupIntent at the time of this confirmation.")
  (attach-to-self
   :type (or boolean null)
   :documentation "If present, the SetupIntent's payment method will be
attached to the in-context Stripe Account.

It can only be used for this Stripe Account's own money movement flows
like InboundTransfer and OutboundTransfers. It cannot be set to true
when setting up a PaymentMethod for a Customer, and defaults to false
when attaching a PaymentMethod to a Customer.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (customer
   :type (or string customer deleted-customer null)
   :documentation "The value of [customer](https://stripe.com/docs/api/setup_intents/object#setup_intent_object-customer)
on the SetupIntent at the time of this confirmation.")
  (flow-directions
   :type (or (vector string) null)
   :documentation "Indicates the directions of money movement for which
this payment method is intended to be used.

Include `inbound` if you intend to use the payment method as the origin
to pull funds from. Include `outbound` if you intend to use the payment
method as the destination to send funds to. You can include both if you
intend to use the payment method for both purposes.

One of `inbound` or `outbound`.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (on-behalf-of
   :type (or string account null)
   :documentation "The value of [on_behalf_of](https://stripe.com/docs/api/setup_intents/object#setup_intent_object-on_behalf_of)
on the SetupIntent at the time of this confirmation.")
  (payment-method
   :type (or string payment-method)
   :documentation "ID of the payment method used with this
SetupAttempt.")
  (setup-attempt-payment-method-details
   :type setup-attempt-payment-method-details)
  (setup-error
   :documentation "The error encountered during this attempt to confirm
the SetupIntent, if any.")
  (status
   :type string
   :documentation "Status of this SetupAttempt, one of
`requires_confirmation`, `requires_action`, `processing`, `succeeded`,
`failed`, or `abandoned`.")
  (usage
   :type string
   :documentation "The value of [usage](https://stripe.com/docs/api/setup_intents/object#setup_intent_object-usage)
on the SetupIntent at the time of this confirmation, one of
`off_session` or `on_session`.")
  (:list-type t))

(defmethod initialize-instance :after ((instance setup-attempt) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:application
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%application)
                   (make-instance 'application :data value))))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:customer
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%customer)
                   (make-instance (if (gethash :deleted value)
                                      'deleted-customer
                                      'customer)
                                  :data value))))
          (:payment-method
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%payment-method)
                   (make-instance 'payment-method :data value))))
          (:setup-attempt-payment-method-details
           (unless (eql 'null value)
             (setf (slot-value instance '%setup-attempt-payment-method-details)
                   (make-instance 'setup-attempt-payment-method-details
                                  :data value))))
          (:setup-error
           (unless (eql 'null value)
             (setf (slot-value instance '%setup-error)
                   (make-instance 'setup-error :data value)))))))))

(define-object setup-attempt-payment-method-details ()
  (acss-debit
   :type (or setup-attempt-payment-method-details-acss-debit null))
  (amazon-pay
   :type (or setup-attempt-payment-method-details-amazon-pay null))
  (au-becs-debit
   :type (or setup-attempt-payment-method-details-au-becs-debit null))
  (bacs-debit
   :type (or setup-attempt-payment-method-details-bacs-debit null))
  (bancontact
   :type (or setup-attempt-payment-method-details-bancontact null))
  (boleto
   :type (or setup-attempt-payment-method-details-boleto null))
  (card
   :type (or setup-attempt-payment-method-details-card null))
  (card-present
   :type (or setup-attempt-payment-method-details-card-present null))
  (cashapp
   :type (or setup-attempt-payment-method-details-cashapp null))
  (ideal
   :type (or setup-attempt-payment-method-details-ideal null))
  (kakao-pay
   :type (or setup-attempt-payment-method-details-kakao-pay null))
  (klarna
   :type (or setup-attempt-payment-method-details-klarna null))
  (kr-card
   :type (or setup-attempt-payment-method-details-kr-card null))
  (link
   :type (or setup-attempt-payment-method-details-link null))
  (paypal
   :type (or setup-attempt-payment-method-details-paypal null))
  (relovut-pay
   :type (or setup-attempt-payment-method-details-relovut-pay null))
  (sepa-debit
   :type (or setup-attempt-payment-method-details-sepa-debit null))
  (sofort
   :type (or setup-attempt-payment-method-details-sofort null))
  (type
   :reader payment-method-type
   :documentation "The type of the payment method used in the
SetupIntent (e.g., `card`). An additional hash is included on
`payment_method_details` with a name matching this value. It contains
confirmation-specific information for the payment method.")
  (us-bank-account
   :type (or setup-attempt-payment-method-details-us-bank-account null)))

(defmethod initialize-instance :after ((instance setup-attempt-payment-method-details)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:acss-debit
           (unless (eql 'null value)
             (setf (slot-value instance '%acss-debit)
                   (make-instance 'setup-attempt-payment-method-details-acss-debit
                                  :data value))))
          (:amazon-pay
           (unless (eql 'null value)
             (setf (slot-value instance '%amazon-pay)
                   (make-instance 'setup-attempt-payment-method-details-amazon-pay
                                  :data value))))
          (:au-becs-debit
           (unless (eql 'null value)
             (setf (slot-value instance '%au-becs-debit)
                   (make-instance 'setup-attempt-payment-method-details-au-becs-debit
                                  :data value))))
          (:bacs-debit
           (unless (eql 'null value)
             (setf (slot-value instance '%bacs-debit)
                   (make-instance 'setup-attempt-payment-method-details-bacs-debit
                                  :data value))))
          (:bancontact
           (unless (eql 'null value)
             (setf (slot-value instance '%bancontact)
                   (make-instance 'setup-attempt-payment-method-details-bancontact
                                  :data value))))
          (:card
           (unless (eql 'null value)
             (setf (slot-value instance '%card)
                   (make-instance 'setup-attempt-payment-method-details-card
                                  :data value))))
          (:card-present
           (unless (eql 'null value)
             (setf (slot-value instance '%card-present)
                   (make-instance 'setup-attempt-payment-method-details-card-present
                                  :data value))))
          (:ideal
           (unless (eql 'null value)
             (setf (slot-value instance '%ideal)
                   (make-instance 'setup-attempt-payment-method-details-ideal
                                  :data value))))
          (:klarna
           (unless (eql 'null value)
             (setf (slot-value instance '%klarna)
                   (make-instance 'setup-attempt-payment-method-details-klarna
                                  :data value))))
          (:link
           (unless (eql 'null value)
             (setf (slot-value instance '%link)
                   (make-instance 'setup-attempt-payment-method-details-link
                                  :data value))))
          (:paypal
           (unless (eql 'null value)
             (setf (slot-value instance '%paypal)
                   (make-instance 'setup-attempt-payment-method-details-paypal
                                  :data value))))
          (:sepa-debit
           (unless (eql 'null value)
             (setf (slot-value instance '%sepa-debit)
                   (make-instance 'setup-attempt-payment-method-details-sepa-debit
                                  :data value))))
          (:sofort
           (unless (eql 'null value)
             (setf (slot-value instance '%sofort)
                   (make-instance 'setup-attempt-payment-method-details-sofort
                                  :data value))))
          (:us-bank-account
           (unless (eql 'null value)
             (setf (slot-value instance '%us-bank-account)
                   (make-instance 'setup-attempt-payment-method-details-us-bank-account
                                  :data value)))))))))

(define-object setup-attempt-payment-method-details-acss-debit ())
(define-object setup-attempt-payment-method-details-amazon-pay ())
(define-object setup-attempt-payment-method-details-au-becs-debit ())
(define-object setup-attempt-payment-method-details-bacs-debit ())

(define-object setup-attempt-payment-method-details-bancontact ()
  (bank-code
   :type (or string null)
   :documentation "Bank code of bank associated with the bank account.")
  (bank-name
   :type (or string null)
   :documentation "Name of the bank associated with the bank account.")
  (bic
   :type (or string null)
   :documentation "Bank Identifier Code of the bank associated with
the bank account.")
  (generated-sepa-debit
   :type (or string payment-method null)
   :documentation "The ID of the SEPA Direct Debit PaymentMethod which
was generated by this SetupAttempt.")
  (generated-sepa-debit-mandate
   :type (or string payment-method null)
   :documentation "The mandate for the SEPA Direct Debit PaymentMethod
which was generated by this SetupAttempt.")
  (iban-last4
   :type (or string null)
   :documentation "Last four characters of the IBAN.")
  (preferred-language
   :type (or string null)
   :documentation "Preferred language of the Bancontact authorization
page that the customer is redirected to. Can be one of `en`, `de`,
`fr`, or `nl`.")
  (verified-name
   :type (or string null)
   :documentation "Owner's verified full name. Values are verified or
provided by Bancontact directly (if supported) at the time of
authorization or settlement. They cannot be set or mutated."))

(defmethod initialize-instance :after ((instance setup-attempt-payment-method-details-bancontact)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:generated-sepa-debit
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%generated-sepa-debit)
                   (make-instance 'payment-method :data value))))
          (:generated-sepa-debit-mandate
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%generated-sepa-debit-mandate)
                   (make-instance 'payment-method :data value)))))))))

(define-object setup-attempt-payment-method-details-boleto ())

(define-object setup-attempt-payment-method-details-card ()
  (brand
   :type (or string null)
   :documentation "Card brand. Can be `amex`, `diners`, `discover`,
`eftpos_au`, `jcb`, `mastercard`, `unionpay`, `visa`, or `unknown`.")
  (checks
   :type (or setup-attempt-payment-method-details-card-checks null)
   :documentation "Check results by Card networks on Card address and
CVC at the time of authorization.")
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the country of the
card. You could use this attribute to get a sense of the international
breakdown of cards you've collected.")
  (description
   :type (or string null)
   :documentation "A high-level description of the type of cards issued
in this range. (For internal use only and not typically available in
standard API requests.)")
  (exp-month
   :type (or integer null)
   :documentation "Two-digit number representing the card's expiration
month.")
  (exp-year
   :type (or integer null)
   :documentation "Four-digit number representing the card's
expiration year.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular card number. You
can use this attribute to check whether two customers who've signed up
with you are using the same card number, for example. For payment
methods that tokenize card information (Apple Pay, Google Pay), the
tokenized number might be provided instead of the underlying card
number.")
  (funding
   :type (or string null)
   :documentation "Card funding type. Can be `credit`, `debit`,
`prepaid`, or `unknown`.")
  (iin
   :type (or string null)
   :documentation "Issuer identification number of the card. (For
internal use only and not typically available in standard API
requests.)")
  (issuer
   :type (or string null)
   :documentation "The name of the card's issuing bank. (For internal
use only and not typically available in standard API requests.)")
  (last4
   :type (or string null)
   :documentation "The last four digits of the card.")
  (network
   :type (or string null)
   :documentation "Identifies which network this charge was processed
on. Can be `amex`, `cartes_bancaires`, `diners`, `discover`,
`eftpos_au`, `interac`, `jcb`, `mastercard`, `unionpay`, `visa`, or
`unknown`.")
  (three-d-secure
   :type (or setup-attempt-payment-method-details-card-three-d-secure null)
   :documentation "Populated if this authorization used 3D Secure
authentication.")
  (wallet
   :type (or setup-attempt-payment-method-details-card-wallet null)
   :documentation "If this Card is part of a card wallet, this contains
the details of the card wallet."))

(defmethod initialize-instance :after ((instance setup-attempt-payment-method-details-card)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:checks
           (unless (eql 'null value)
             (setf (slot-value instance '%checks)
                   (make-instance 'setup-attempt-payment-method-details-card-checks
                                  :data value))))
          (:three-d-secure
           (unless (eql 'null value)
             (setf (slot-value instance '%three-d-secure)
                   (make-instance 'setup-attempt-payment-method-details-card-three-d-secure
                                  :data value))))
          (:wallet
           (unless (eql 'null value)
             (setf (slot-value instance '%wallet)
                   (make-instance 'setup-attempt-payment-method-details-card-wallet
                                  :data value)))))))))

(define-object setup-attempt-payment-method-details-card-checks ()
  (address-line1-check
   :type (or string null)
   :documentation "If a address line1 was provided, results of the
check, one of `pass`, `fail`, `unavailable`, or `unchecked`.")
  (address-postal-code-check
   :type (or string null)
   :documentation "If a address postal code was provided, results of
the check, one of `pass`, `fail`, `unavailable`, or `unchecked`.")
  (cvc-check
   :type (or string null)
   :documentation "If a CVC was provided, results of the check, one of
`pass`, `fail`, `unavailable`, or `unchecked`."))

(define-object setup-attempt-payment-method-details-card-three-d-secure ()
  (authentication-flow
   :type (or string null)
   :documentation "For authenticated transactions: how the customer was
authenticated by the issuing bank. One of `challenge` or
`frictionless`.")
  (electronic-commerce-indicator
   :type (or string null)
   :documentation "The Electronic Commerce Indicator (ECI). A
protocol-level field indicating what degree of authentication was
performed. One of `01`, `02`, `05`, `06`, or `07`.")
  (result
   :type (or string null)
   :documentation "Indicates the outcome of 3D Secure authentication.
One of `attempt_acknowledged`, `authenticated`, `exempted`, `failed`,
`not_supported`, or `processing_error`.")
  (result-reason
   :type (or string null)
   :documentation "Additional information about why 3D Secure succeeded
or failed based on the `result`. One of `abandoned`, `bypassed`,
`canceled`, `card_not_enrolled`, `network_not_supported`,
`protocol_error`, or `rejected`.")
  (transaction-id
   :type (or string null)
   :documentation "The 3D Secure 1 XID or 3D Secure 2 Directory Server
Transaction ID (dsTransId) for this payment.")
  (version
   :type (or string null)
   :documentation "The version of 3D Secure that was used. One of
`1.0.2`, `2.1.0`, or `2.2.0`."))

(define-object setup-attempt-payment-method-details-card-wallet ()
  (apple-pay)
  (google-pay)
  (type
   :reader card-wallet-type
   :documentation "The type of the card wallet, one of `apple_pay`,
`google_pay`, or `link`. An additional hash is included on the Wallet
subhash with a name matching this value. It contains additional
information specific to the card wallet type."))

(define-object setup-attempt-payment-method-details-card-present ()
  (generated-card
   :type (or string payment-method null)
   :documentation "The ID of the Card PaymentMethod which was generated
by this SetupAttempt.")
  (offline
   :type (or setup-attempt-payment-method-details-card-present-offline null)
   :documentation "Details about payments collected offline."))

(defmethod initialize-instance :after ((instance setup-attempt-payment-method-details-card-present)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:generated-card
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%generated-card)
                   (make-instance 'payment-method :data value))))
          (:offline
           (unless (eql 'null value)
             (setf (slot-value instance '%offline)
                   (make-instance 'setup-attempt-payment-method-details-card-present-offline
                                  :data value)))))))))

(define-object setup-attempt-payment-method-details-card-present-offline ()
  (stored-at
   :type (or time:timestamp null)
   :documentation "Time at which the payment was collected while
offline.")
  (type
   :reader card-present-offline-type
   :documentation "The method used to process this payment method
offline. Only deferred is allowed."))

(defmethod initialize-instance :after ((instance
                                        setup-attempt-payment-method-details-card-present-offline)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:stored-at
           (setf (slot-value instance '%stored-at) (decode-timestamp value))))))))

(define-object setup-attempt-payment-method-details-cashapp ())

(define-object setup-attempt-payment-method-details-ideal ()
  (bank
   :type (or string null)
   :documentation "The customer's bank. Can be one of `abn_amro`,
`asn_bank`, `bunq`, `handelsbanken`, `ing`, `knab`, `moneyou`, `n26`,
`nn`, `rabobank`, `regiobank`, `revolut`, `sns_bank`, `triodos_bank`,
`van_lanschot`, or `yoursafe`.")
  (bic
   :type (or string null)
   :documentation "The Bank Identifier Code of the customer's bank.
One of `ABNANL2A`, `ASNBNL21`, `BITSNL2A`, `BUNQNL2A`, `FVLBNL22`,
`HANDNL2A`, `INGBNL2A`, `KNABNL2H`, `MOYONL21`, `NNBANL2G`, `NTSBDEB1`,
`RABONL2U`, `RBRBNL21`, `REVOIE23`, `REVOLT21`, `SNSBNL2A`, or
`TRIONL2U`.")
  (generated-sepa-debit
   :type (or string payment-method null)
   :documentation "The ID of the SEPA Direct Debit PaymentMethod which
was generated by this SetupAttempt.")
  (generated-sepa-debit-mandate
   :type (or string mandate null)
   :documentation "The mandate for the SEPA Direct Debit PaymentMethod
which was generated by this SetupAttempt.")
  (iban-last4
   :type (or string null)
   :documentation "Last four characters of the IBAN.")
  (verified-name
   :type (or string null)
   :documentation "Owner's verified full name. Values are verified or
provided by iDEAL directly (if supported) at the time of authorization
or settlement. They cannot be set or mutated."))

(defmethod initialize-instance :after ((instance setup-attempt-payment-method-details-ideal)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:generated-sepa-debit
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%generated-sepa-debit)
                   (make-instance 'payment-method :data value))))
          (:generated-sepa-debit-mandate
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%generated-sepa-debit-mandate)
                   (make-instance 'mandate :data value)))))))))

(define-object setup-attempt-payment-method-details-kakao-pay ())
(define-object setup-attempt-payment-method-details-klarna ())
(define-object setup-attempt-payment-method-details-kr-card ())
(define-object setup-attempt-payment-method-details-link ())
(define-object setup-attempt-payment-method-details-paypal ())
(define-object setup-attempt-payment-method-details-revolut-pay ())
(define-object setup-attempt-payment-method-details-sepa-debit ())

(define-object setup-attempt-payment-method-details-sofort ()
  (bank-code
   :type (or string null)
   :documentation "Bank code of bank associated with the bank account.")
  (bank-name
   :type (or string null)
   :documentation "Name of the bank associated with the bank account.")
  (bic
   :type (or string null)
   :documentation "Bank Identifier Code of the bank associated with the
bank account.")
  (generated-sepa-debit
   :type (or string payment-method null)
   :documentation "The ID of the SEPA Direct Debit PaymentMethod which
was generated by this SetupAttempt.")
  (generated-sepa-debit-mandate
   :type (or string mandate null)
   :documentation "The mandate for the SEPA Direct Debit PaymentMethod
which was generated by this SetupAttempt.")
  (iban-last4
   :type (or string null)
   :documentation "Last four characters of the IBAN.")
  (preferred-language
   :type (or string null)
   :documentation "Preferred language of the Sofort authorization page
that the customer is redirected to. Can be one of `en`, `de`, `fr`, or
`nl`.")
  (verified-name
   :type (or string null)
   :documentation "Owner's verified full name. Values are verified or
provided by Sofort directly (if supported) at the time of authorization
or settlement. They cannot be set or mutated."))

(defmethod initialize-instance :after ((instance setup-attempt-payment-method-details-sofort)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:generated-sepa-debit
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%generated-sepa-debit)
                   (make-instance 'payment-method :data value))))
          (:generated-sepa-debit-mandate
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%generated-sepa-debit-mandate)
                   (make-instance 'mandate :data value)))))))))

(define-object setup-attempt-payment-method-details-us-bank-account ())

(define-object setup-error ()
  (charge
   :type (or string null)
   :documentation "For card errors, the ID of the failed charge.")
  (code
   :type (or string null)
   :documentation "For some errors that could be handled
programmatically, a short string indicating the
[error code](https://stripe.com/docs/error-codes) reported.

One of:

`account_closed`,
`account_country_invalid_address`,
`account_error_country_change_requires_additional_steps`,
`account_information_mismatch`,
`account_invalid`,
`account_number_invalid`,
`acss_debit_session_incomplete`,
`alipay_upgrade_required`,
`amount_too_large`,
`amount_too_small`,
`api_key_expired`,
`application_fees_not_allowed`,
`authentication_required`,
`balance_insufficient`,
`balance_invalid_parameter`,
`bank_account_bad_routing_numbers`,
`bank_account_declined`,
`bank_account_exists`,
`bank_account_restricted`,
`bank_account_unusable`,
`bank_account_unverified`,
`bank_account_verification_failed`,
`billing_invalid_mandate`,
`bitcoin_upgrade_required`,
`capture_charge_authorization_expired`,
`capture_unauthorized_payment`,
`card_decline_rate_limit_exceeded`,
`card_declined`,
`cardholder_phone_number_required`,
`charge_already_captured`,
`charge_already_refunded`,
`charge_disputed`,
`charge_exceeds_source_limit`,
`charge_exceeds_transaction_limit`,
`charge_expired_for_capture`,
`charge_invalid_parameter`,
`charge_not_refundable`,
`clearing_code_unsupported`,
`country_code_invalid`,
`country_unsupported`,
`coupon_expired`,
`customer_max_payment_methods`,
`customer_max_subscriptions`,
`customer_tax_location_invalid`,
`debit_not_authorized`,
`email_invalid`,
`expired_card`,
`financial_connections_account_inactive`,
`financial_connections_no_successful_transaction_refresh`,
`forwarding_api_inactive`,
`forwarding_api_invalid_parameter`,
`forwarding_api_upstream_connection_error`,
`forwarding_api_upstream_connection_timeout`,
`idempotency_key_in_use`,
`incorrect_address`,
`incorrect_cvc`,
`incorrect_number`,
`incorrect_zip`,
`instant_payouts_config_disabled`,
`instant_payouts_currency_disabled`,
`instant_payouts_limit_exceeded`,
`instant_payouts_unsupported`,
`insufficient_funds`,
`intent_invalid_state`,
`intent_verification_method_missing`,
`invalid_card_type`,
`invalid_characters`,
`invalid_charge_amount`,
`invalid_cvc`,
`invalid_expiry_month`,
`invalid_expiry_year`,
`invalid_mandate_reference_prefix_format`,
`invalid_number`,
`invalid_source_usage`,
`invalid_tax_location`,
`invoice_no_customer_line_items`,
`invoice_no_payment_method_types`,
`invoice_no_subscription_line_items`,
`invoice_not_editable`,
`invoice_on_behalf_of_not_editable`,
`invoice_payment_intent_requires_action`,
`invoice_upcoming_none`,
`livemode_mismatch`,
`lock_timeout`,
`missing`,
`no_account`,
`not_allowed_on_standard_account`,
`out_of_inventory`,
`ownership_declaration_not_allowed`,
`parameter_invalid_empty`,
`parameter_invalid_integer`,
`parameter_invalid_string_blank`,
`parameter_invalid_string_empty`,
`parameter_missing`,
`parameter_unknown`,
`parameters_exclusive`,
`payment_intent_action_required`,
`payment_intent_authentication_failure`,
`payment_intent_incompatible_payment_method`,
`payment_intent_invalid_parameter`,
`payment_intent_konbini_rejected_confirmation_number`,
`payment_intent_mandate_invalid`,
`payment_intent_payment_attempt_expired`,
`payment_intent_payment_attempt_failed`,
`payment_intent_unexpected_state`,
`payment_method_bank_account_already_verified`,
`payment_method_bank_account_blocked`,
`payment_method_billing_details_address_missing`,
`payment_method_configuration_failures`,
`payment_method_currency_mismatch`,
`payment_method_customer_decline`,
`payment_method_invalid_parameter`,
`payment_method_invalid_parameter_testmode`,
`payment_method_microdeposit_failed`,
`payment_method_microdeposit_verification_amounts_invalid`,
`payment_method_microdeposit_verification_amounts_mismatch`,
`payment_method_microdeposit_verification_attempts_exceeded`,
`payment_method_microdeposit_verification_descriptor_code_mismatch`,
`payment_method_microdeposit_verification_timeout`,
`payment_method_not_available`,
`payment_method_provider_decline`,
`payment_method_provider_timeout`,
`payment_method_unactivated`,
`payment_method_unexpected_state`,
`payment_method_unsupported_type`,
`payout_reconciliation_not_ready`,
`payouts_limit_exceeded`,
`payouts_not_allowed`,
`platform_account_required`,
`platform_api_key_expired`,
`postal_code_invalid`,
`processing_error`,
`product_inactive`,
`progressive_onboarding_limit_exceeded`,
`rate_limit`,
`refer_to_customer`,
`refund_disputed_payment`,
`resource_already_exists`,
`resource_missing`,
`return_intent_already_processed`,
`routing_number_invalid`,
`secret_key_required`,
`sepa_unsupported_account`,
`setup_attempt_failed`,
`setup_intent_authentication_failure`,
`setup_intent_invalid_parameter`,
`setup_intent_mandate_invalid`,
`setup_intent_setup_attempt_expired`,
`setup_intent_unexpected_state`,
`shipping_address_invalid`,
`shipping_calculation_failed`,
`sku_inactive`,
`state_unsupported`,
`status_transition_invalid`,
`stripe_tax_inactive`,
`tax_id_invalid`,
`taxes_calculation_failed`,
`terminal_location_country_unsupported`,
`terminal_reader_busy`,
`terminal_reader_hardware_fault`,
`terminal_reader_invalid_location_for_activation`,
`terminal_reader_invalid_location_for_payment`,
`terminal_reader_offline`,
`terminal_reader_timeout`,
`testmode_charges_only`,
`tls_version_unsupported`,
`token_already_used`,
`token_card_network_invalid`,
`token_in_use`,
`transfer_source_balance_parameters_mismatch`,
`transfers_not_allowed`,
or `url_invalid`.")
  (decline-code
   :type (or string null)
   :documentation "For card errors resulting from a card issuer
decline, a short string indicating the [card issuer's reason for the
decline](https://stripe.com/docs/declines#issuer-declines) if they
provide one.")
  (doc-url
   :type (or string null)
   :documentation "A URL to more information about the
[error code](https://stripe.com/docs/error-codes) reported.")
  (message
   :type (or string null)
   :documentation "A human-readable message providing more details
about the error. For card errors, these messages can be shown to your
users.")
  (param
   :type (or string null)
   :documentation "If the error is parameter-specific, the parameter
related to the error. For example, you can use this to display a
message near the correct form field.")
  (payment-intent
   :type (or payment-intent null)
   :documentation "A PaymentIntent guides you through the process of
collecting a payment from your customer.

We recommend that you create exactly one PaymentIntent for each order
or customer session in your system. You can reference the PaymentIntent
later to see the history of payment attempts for a particular session.

A PaymentIntent transitions through [multiple statuses](https://stripe.com/docs/payments/intents#intent-statuses)
throughout its lifetime as it interfaces with Stripe.js to perform
authentication flows and ultimately creates at most one successful
charge.

Related guide: [Payment Intents API](https://stripe.com/docs/payments/payment-intents)")
  (payment-method
   :type (or payment-method null)
   :documentation "PaymentMethod objects represent your customer's
payment instruments. You can use them with
[PaymentIntents](https://stripe.com/docs/payments/payment-intents)
to collect payments or save them to Customer objects to store
instrument details for future payments.

Related guides: [Payment Methods](https://stripe.com/docs/payments/payment-methods) and [More Payment Scenarios](https://stripe.com/docs/payments/more-payment-scenarios).")
  (payment-method-type
   :type (or string null)
   :documentation "If the error is specific to the type of payment
method, the payment method type that had a problem. This field is only
populated for invoice-related errors.")
  (request-log-url
   :type (or string null)
   :documentation "A URL to the request log entry in your dashboard.")
  (setup-intent
   :type (or setup-intent null)
   :documentation "A SetupIntent guides you through the process of
setting up and saving a customer's payment credentials for future
payments. For example, you can use a SetupIntent to set up and save
your customer's card without immediately collecting a payment. Later,
you can use [PaymentIntents](https://stripe.com/docs/api#payment_intents)
to drive the payment flow.

Create a SetupIntent when you're ready to collect your customer's
payment credentials. Don't maintain long-lived, unconfirmed
SetupIntents because they might not be valid. The SetupIntent
transitions through multiple
[statuses](https://docs.stripe.com/payments/intents#intent-statuses)
as it guides you through the setup process.

Successful SetupIntents result in payment credentials that are
optimized for future payments. For example, cardholders in
[certain regions](https://stripe.com/guides/strong-customer-authentication)
might need to be run through
[Strong Customer Authentication](https://docs.stripe.com/strong-customer-authentication)
during payment method collection to streamline later
[off-session payments](https://docs.stripe.com/payments/setup-intents).
If you use the SetupIntent with a
[Customer](https://stripe.com/docs/api#setup_intent_object-customer),
it automatically attaches the resulting payment method to that Customer
after successful setup. We recommend using SetupIntents or
[setup_future_usage](https://stripe.com/docs/api#payment_intent_object-setup_future_usage)
on PaymentIntents to save payment methods to prevent saving invalid
or unoptimized payment methods.

By using SetupIntents, you can reduce friction for your customers, even
as regulations change over time.

Related guide: [Setup Intents API](https://docs.stripe.com/payments/setup-intents)")
  (source
   :type (or customer-source null))
  (type
   :reader setup-error-type
   :type string
   :documentation "The type of error returned. One of `api_error`,
`card_error`, `idempotency_error`, or `invalid_request_error`"))

(defmethod initialize-instance :after ((instance setup-error)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:payment-intent
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%payment-intent)
                   (make-instance 'payment-intent :data value))))
          (:payment-method
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%payment-method)
                   (make-instance 'payment-method :data value))))
          (:setup-intent
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%setup-intent)
                   (make-instance 'setup-intent :data value))))
          (:source
           (when (and value (not (stringp value)))
             (let ((object-type (gethash :object value)))
               (setf (slot-value instance '%source)
                     (make-instance
                      (case object-type
                        ("account" 'account)
                        ("bank_account" 'bank-account)
                        ("card" 'card)
                        ("source" 'source)
                        (t (error "Unknown source type: ~A" object-type)))
                      :data value))))))))))
