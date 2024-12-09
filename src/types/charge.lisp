(in-package #:stripe)

(define-object charge ()
  "The Charge object represents a single attempt to move money into
your Stripe account.

PaymentIntent confirmation is the most common way to create Charges,
but transferring money to a different Stripe account through Connect
also creates Charges.

Some legacy payment flows create Charges directly, which is not
recommended for new integrations."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (amount
   :type integer
   :documentation "Amount intended to be collected by this payment. A
positive integer representing how much to charge in the [smallest
currency unit](https://stripe.com/docs/currencies#zero-decimal) (e.g.,
100 cents to charge $1.00 or 100 to charge ¥100, a zero-decimal
currency). The minimum amount is $0.50 US or [equivalent in charge
currency]
(https://stripe.com/docs/currencies#minimum-and-maximum-charge-amounts).
The amount value supports up to eight digits (e.g., a value of 99999999
for a USD charge of $999,999.99).")
  (amount-captured
   :type integer
   :documentation "Amount in cents (or local equivalent) captured (can
be less than the amount attribute on the charge if a partial capture
was made).")
  (amount-refunded
   :type integer
   :documentation "Amount in cents (or local equivalent) refunded (can
be less than the amount attribute on the charge if a partial refund was
issued).")
  (application
   :type (or string application null)
   :documentation "ID of the Connect application that created the
charge.")
  (application-fee
   :type (or string application-fee null)
   :documentation "The application fee (if any) for the charge. [See the
Connect documentation]
(https://stripe.com/docs/connect/direct-charges#collect-fees)
for details.")
  (application-fee-amount
   :type (or integer null)
   :documentation "The amount of the application fee (if any) requested
for the charge. [See the Connect documentation]
(https://stripe.com/docs/connect/direct-charges#collect-fees) for
details.")
  (balance-transaction
   :type (or string balance-transaction null)
   :documentation "ID of the balance transaction that describes the
impact of this charge on your account balance (not including refunds
or disputes).")
  (billing-details
   :type billing-details
   :documentation "Billing information associated with the payment
method at the time of the transaction.")
  (calculated-statement-descriptor
   :type (or string null)
   :documentation "The full statement descriptor that is passed to card
networks, and that is displayed on your customers' credit card and bank
statements. Allows you to see what the statement descriptor looks like
after the static and dynamic portions are combined. This value only
exists for card payments.")
  (captured
   :type boolean
   :documentation "If the charge was created without capturing, this
Boolean represents whether it is still uncaptured or has since been
captured.")
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
   :documentation "ID of the customer this charge is for if one exists.")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the object. Often
useful for displaying to users.")
  (disputed
   :type boolean
   :documentation "Whether the charge has been disputed.")
  (failure-balance-transaction
   :type (or string balance-transaction null)
   :documentation "ID of the balance transaction that describes the
reversal of the balance on your account due to payment failure.")
  (failure-code
   :type (or string null)
   :documentation "Error code explaining reason for charge failure if
available (see [the errors section]
(https://stripe.com/docs/error-codes) for a list of codes).")
  (failure-message
   :type (or string null)
   :documentation "Message to user further explaining reason for charge
failure if available.")
  (fraud-details
   :type charge-fraud-details
   :documentation "Information on fraud assessments for the charge.")
  (invoice
   :type (or string invoice null)
   :documentation "ID of the invoice this charge is for if one exists.")
  (livemode
   :type boolean
   :documentation "True if the object exists in live mode or the
false if the object exists in test mode.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (object
   :type string
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (on-behalf-of
   :type (or string account null)
   :documentation "The account (if any) the charge was made on behalf
of without triggering an automatic transfer. See the [Connect
documentation]
(https://stripe.com/docs/connect/separate-charges-and-transfers) for details.")
  (outcome
   :type charge-outcome
   :documentation "Details about whether the payment was accepted, and
why. See [understanding declines](https://stripe.com/docs/declines) for
details.")
  (paid
   :type boolean
   :documentation "True if the charge succeeded, or was successfully
authorized for later capture.")
  (payment-intent
   :type (or string payment-intent null)
   :documentation "ID of the PaymentIntent associated with this charge,
if one exists.")
  (payment-method
   :type (or string null)
   :documentation "ID of the payment method used in this charge.")
  (payment-method-details
   :type charge-payment-method-details
   :documentation "Details about the payment method at the time of the
transaction.")
  (radar-options
   :type charge-radar-options-nullable
   :documentation "Options to configure Radar. See [Radar Session]
(https://stripe.com/docs/radar/radar-session) for more information.")
  (receipt-email
   :type (or string null)
   :documentation "This is the email address that the receipt for this
charge was sent to.")
  (receipt-number
   :type (or string null)
   :documentation "This is the transaction number that appears on email
receipts sent for this charge. This attribute will be `null` until a
receipt has been sent.")
  (receipt-url
   :type (or string null)
   :documentation "This is the URL to view the receipt for this charge.
The receipt is kept up-to-date to the latest state of the charge,
including any refunds. If the charge is for an Invoice, the receipt
will be stylized as an Invoice receipt.")
  (refunded
   :type boolean
   :documentation "Whether the charge has been fully refunded. If the charge is
only partially refunded, this attribute will still be false.")
  (refunds
   :type (or list-refund null)
   :documentation "A list of refunds that have been applied to the
charge.")
  (review
   :type (or string review null)
   :documentation "ID of the review associated with this charge if one
exists.")
  (shipping
   :type (or shipping null)
   :documentation "Shipping information for the charge.")
  (source-transfer
   :type (or string transfer null)
   :documentation "The transfer ID which created this charge. Only
present if the charge came from another Stripe account. [See the
Connect documentation]
(https://docs.stripe.com/connect/destination-charges) for details.")
  (statement-descriptor
   :type (or string null)
   :documentation "For a non-card charge, text that appears on the
customer's statement as the statement descriptor. This value overrides
the account's default statement descriptor. For information about
requirements, including the 22-character limit, see [the Statement
Descriptor docs]
(https://docs.stripe.com/get-started/account/statement-descriptors).")
  (statement-descriptor-suffix
   :type (or string null)
   :documentation "For a card charge, this value is ignored unless you
don't specify a `statement_descriptor_suffix`, in which case this value
is used as the suffix.")
  (status
   :type string
   :documentation "The status of the payment is either `succeeded`,
`pending`, or `failed`.")
  (transfer
   :type (or string transfer null)
   :documentation "ID of the transfer to the `destination` account
(only applicable if the charge was created using the `destination`
parameter).")
  (transfer-data
   :type (or string null)
   :documentation "An optional dictionary including the account to
automatically transfer to as part of a destination charge. [See the
Connect documentation]
(https://stripe.com/docs/connect/destination-charges) for details.")
  (transfer-group
   :type (or string null)
   :documentation "A string that identifies this transaction as part
of a group. See the [Connect documentation]
(https://stripe.com/docs/connect/separate-charges-and-transfers#transfer-options)
for details."))

(define-object charge-fraud-details ()
  (stripe-report
   :type (or string null)
   :documentation "Assessments from Stripe. If set, the value is
`fraudulent`.")
  (user-report
   :type (or string null)
   :documentation "Assessments reported by you. If set, possible values
of are `safe` and `fraudulent`."))

(define-object charge-outcome ()
  (network-status
   :type (or string null)
   :documentation "Possible values are `approved_by_network`,
`declined_by_network`, `not_sent_to_network`, and
`reversed_after_approval`. The value `reversed_after_approval`
indicates the payment was [blocked by Stripe]
(https://stripe.com/docs/declines#blocked-payments) after bank
authorization, and may temporarily appear as 'pending' on a
cardholder's statement.")
  (reason
   :type (or string null)
   :documentation "An enumerated value providing a more detailed
explanation of the outcome's `type`. Charges blocked by Radar's default
block rule have the value `highest_risk_level`. Charges placed in
review by Radar's default review rule have the value
`elevated_risk_level`. Charges authorized, blocked, or placed in review
by custom rules have the value `rule`. See [understanding declines]
(https://stripe.com/docs/declines) for more details.")
  (risk-level
   :type (or string null)
   :documentation "Stripe Radar's evaluation of the riskiness of the
payment. Possible values for evaluated payments are `normal`,
`elevated`, `highest`. For non-card payments, and card-based payments
predating the public assignment of risk levels, this field will have
the value `not_assessed`. In the event of an error in the evaluation,
this field will have the value `unknown`. This field is only available
with Radar.")
  (risk-score
   :type (or integer null)
   :documentation "Stripe Radar's evaluation of the riskiness of the
payment. Possible values for evaluated payments are between 0 and 100.
For non-card payments, card-based payments predating the public
assignment of risk scores, or in the event of an error during
evaluation, this field will not be present. This field is only
available with Radar for Fraud Teams.")
  (rule
   :type (or string charge-outcome-rule null)
   :documentation "The ID of the Radar rule that matched the payment,
if applicable.")
  (seller-message
   :type (or string null)
   :documentation "A human-readable description of the outcome type and
reason, designed for you (the recipient of the payment), not your
customer.")
  (type
   :reader outcome-type
   :type string
   :documentation "Possible values are `authorized`, `manual_review`,
`issuer_declined`, `blocked`, and `invalid`. See [understanding
declines](https://stripe.com/docs/declines) and [Radar reviews]
(https://stripe.com/docs/radar/reviews) for details."))

(define-object charge-outcome-rule ()
  (action
   :type string
   :documentation "The action taken on the payment.")
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (predicate
   :type string
   :documentation "The predicate to evaluate the payment against."))

(define-object charge-radar-options ()
  (session
   :type (or string null)
   :documentation "A [Radar Session]
(https://stripe.com/docs/radar/radar-session) is a snapshot of the
browser metadata and device details that help Radar make more accurate
predictions on your payments."))

(define-object charge-card-checks ()
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

(define-object charge-card-extended-authorization ()
  (status
   :type string
   :documentation "Indicates whether or not the capture window is
extended beyond the standard authorization. One of `disabled` or
`enabled`."))

(define-object charge-card-incremental-authorization ()
  (status
   :type string
   :documentation "Indicates whether or not the capture window is
extended beyond the standard authorization. One of `available` or
`unavailable`."))

(define-object charge-card-installments ()
  (plan
   :type (or installments-plan null)
   :documentation "Installment plan selected for the payment."))

(define-object charge-card-installments-plan ()
  (count
   :reader installments-plan-count
   :type (or integer null)
   :documentation "For `fixed_count` installment plans, this is the
number of installment payments your customer will make to their credit
card.")
  (interval
   :type (or string null)
   :initform "month"
   :documentation "For `fixed_count` installment plans, this is the
interval between installment payments your customer will make to their
credit card. One of `month`.")
  (type
   :reader installments-plan-type
   :type string
   :initform "fixed_count"
   :documentation "Type of installment plan, one of `fixed_count`."))

(define-object charge-card-multicapture ()
  (status
   :type string
   :documentation "Indicates whether or not multiple captures are
supported. One of `available` or `unavailable`."))

(define-object charge-card-network-token ()
  (used
   :type boolean
   :documentation "Indicates if Stripe used a network token, either
user provided or Stripe managed when processing the transaction."))

(define-object charge-card-overcapture ()
  (maximum-amount-capturable
   :type integer
   :documentation "The maximum amount that can be captured.")
  (status
   :type string
   :documentation "Indicates whether or not the authorized amount can
be over-captured. One of `available` or `unavailable`."))

(define-object charge-card-three-d-secure ()
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
  (exemption-indicator
   :type (or string null)
   :documentation "The exemption requested via 3DS and accepted by the
issuer at authentication time. One of `low_risk` or `none`.")
  (exemption-indicator-applied
   :type (or boolean null)
   :documentation "Whether Stripe requested the value of
`exemption_indicator` in the transaction. This will depend on the
outcome of Stripe's internal risk assessment.")
  (result
   :type (or string null)
   :documentation "Indicates the outcome of 3D Secure authentication.
One of `attempt_acknowledged`, `authenticated`, `exempted`, `failed`,
`not_supported`, or `processing_error`.")
  (result-reason
   :type (or string null)
   :documentation "Additional information about why 3D Secure succeeded
or failed based on the `result`. One of `abandoned`, `bypassed`,
`canceled`, `card_not_enrolled`,`network_not_supported`,
`protocol_error`, or `rejected`.")
  (transaction-id
   :type (or string null)
   :documentation "The 3D Secure 1 XID or 3D Secure 2 Directory Server
Transaction ID (dsTransId) for this payment.")
  (version
   :type (or string null)
   :documentation "The version of 3D Secure that was used. One of
`1.0.2`, `2.1.0`, or `2.2.0`."))

(define-object charge-card-present-offline ()
  (stored-at
   :type (or local-time:timestamp null)
   :documentation "Time at which the payment was collected while
offline.")
  (type
   :reader card-present-offline-type
   :type (or string null)
   :documentation "The method used to process this payment method
offline. Only deferred is allowed. One of `deferred`."))

(define-object charge-card-present-receipt ()
  (account-type
   :type (or string null)
   :documentation "The type of account being debited or credited. One
of `checking`, `credit`, `prepaid`, or `unknown`.")
  (application-cryptogram
   :type (or string null)
   :documentation "EMV tag 9F26, cryptogram generated by the integrated
circuit chip.")
  (application-preferred-name
   :type (or string null)
   :documentation "Mnenomic of the Application Identifier.")
  (authorization-code
   :type (or string null)
   :documentation "Identifier for this transaction.")
  (authorization-response-code
   :type (or string null)
   :documentation "EMV tag 8A. A code returned by the card issuer.")
  (cardholder-verification-method
   :type (or string null)
   :documentation "Describes the method used by the cardholder to
verify ownership of the card. One of the following: `approval`,
`failure`, `none`, `offline_pin`, `offline_pin_and_signature`,
`online_pin`, or `signature`.")
  (dedicated-file-name
   :type (or string null)
   :documentation "EMV tag 84. Similar to the application identifier
stored on the integrated circuit chip.")
  (terminal-verification-results
   :type (or string null)
   :documentation "The outcome of a series of EMV functions performed
by the card reader.")
  (transaction-status-information
   :type (or string null)
   :documentation "An indication of various EMV functions performed
during the transaction."))

(define-object charge-card-wallet ()
  (amex-express-checkout
   :type (or charge-card-wallet-amex-express-checkout null))
  (apple-pay
   :type (or charge-card-wallet-apple-pay null))
  (dynamic-last4
   :type (or string null)
   :documentation "(For tokenized numbers only.) The last four digits
of the device account number.")
  (google-pay
   :type (or charge-card-wallet-google-pay null))
  (link
   :type (or charge-card-wallet-link null))
  (masterpass
   :type (or charge-card-wallet-masterpass null))
  (samsung-pay
   :type (or charge-card-wallet-google-pay null)))

(define-object charge-card-wallet-amex-express-checkout ())

(define-object charge-card-wallet-apple-pay ())

(define-object charge-card-wallet-google-pay ())

(define-object charge-card-wallet-link ())

(define-object charge-card-wallet-masterpass ()
  (billing-address
   :type (or address null)
   :documentation "Owner's verified billing address. Values are
verified or provided by the wallet directly (if supported) at the time
of authorization or settlement. They cannot be set or mutated.")
  (email
   :type (or string null)
   :documentation "Owner's verified email. Values are verified or
provided by the wallet directly (if supported) at the time of
authorization or settlement. They cannot be set or mutated.")
  (name
   :type (or string null)
   :documentation "Owner's verified full name. Values are verified or
provided by the wallet directly (if supported) at the time of
authorization or settlement. They cannot be set or mutated.")
  (shipping-address
   :type (or address null)
   :documentation "Owner's verified shipping address. Values are
verified or provided by the wallet directly (if supported) at the time
of authorization or settlement. They cannot be set or mutated."))

(define-object charge-card-wallet-samsung-pay ())

(define-object charge-card-wallet-visa-checkout ()
  (billing-address
   :type (or address null)
   :documentation "Owner's verified billing address. Values are
verified or provided by the wallet directly (if supported) at the time
of authorization or settlement. They cannot be set or mutated.")
  (email
   :type (or string null)
   :documentation "Owner's verified email. Values are verified or
provided by the wallet directly (if supported) at the time of
authorization or settlement. They cannot be set or mutated.")
  (name
   :type (or string null)
   :documentation "Owner's verified full name. Values are verified or
provided by the wallet directly (if supported) at the time of
authorization or settlement. They cannot be set or mutated.")
  (shipping-address
   :type (or address null)
   :documentation "Owner's verified shipping address. Values are
verified or provided by the wallet directly (if supported) at the time
of authorization or settlement. They cannot be set or mutated."))

(define-object charge-ach-credit-transfer-details ()
  (account-number
   :type (or string null)
   :documentation "Account number to transfer funds to.")
  (bank-name
   :type (or string null)
   :documentation "Name of the bank associated with the routing
number.")
  (routing-number
   :type (or string null)
   :documentation "Routing transit number for the bank account to
transfer funds to.")
  (swift-code
   :type (or string null)
   :documentation "SWIFT code of the bank associated with the routing
number."))

(define-object charge-ach-debit-details ()
  (account-holder-type
   :type (or string null)
   :documentation "Type of entity that holds the account. This can be
either `individual` or `company`.")
  (bank-name
   :type (or string null)
   :documentation "Name of the bank associated with the bank account.")
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the country the bank
account is located in.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular bank account.
You can use this attribute to check whether two bank accounts are the
same.")
  (last4
   :type (or string null)
   :documentation "Last four digits of the bank account number.")
  (routing-number
   :type (or string null)
   :documentation "Routing transit number of the bank account."))

(define-object charge-acss-debit-details ()
  (bank-name
   :type (or string null)
   :documentation "Name of the bank associated with the bank account.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular bank account.
You can use this attribute to check whether two bank accounts are the
same.")
  (institution-number
   :type (or string null)
   :documentation "Institution number of the bank account.")
  (last4
   :type (or string null)
   :documentation "Last four digits of the bank account number.")
  (mandate
   :type (or string null)
   :documentation "ID of the mandate used to make this payment.")
  (transit-number
   :type (or string null)
   :documentation "Transit number of the bank account."))

(define-object charge-affirm-details ()
  (transaction-id
   :type (or string null)
   :documentation "The Affirm transaction ID associated with this
payment."))

(define-object charge-afterpay-clearpay-details ()
  (order-id
   :type (or string null)
   :documentation "The Afterpay order ID associated with this payment
intent.")
  (reference
   :type (or string null)
   :documentation "Order identifier shown to the merchant in Afterpay's
online portal."))

(define-object charge-alipay-details ()
  (buyer-id
   :type (or string null)
   :documentation "Uniquely identifies this particular Alipay account.
You can use this attribute to check whether two Alipay accounts are
the same.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular Alipay account.
You can use this attribute to check whether two Alipay accounts are
the same.")
  (transaction-id
   :type (or string null)
   :documentation "Transaction ID of this particular Alipay
transaction."))

;; NOTE: The Stripe API doesn't mention what the attributes of this object are.
(define-object charge-amazon-pay-details ())

(define-object charge-au-becs-debit-details ()
  (bsb-number
   :type (or string null)
   :documentation "Bank-State-Branch number of the bank account.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular bank account.
You can use this attribute to check whether two bank accounts are the
same.")
  (last4
   :type (or string null)
   :documentation "Last four digits of the bank account number.")
  (mandata
   :type (or string null)
   :documentation "ID of the mandate used to make this payment."))

(define-object charge-bacs-debit-details ()
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular bank account.
You can use this attribute to check whether two bank accounts are the
same.")
  (last4
   :type (or string null)
   :documentation "Last four digits of the bank account number.")
  (mandate
   :type (or string null)
   :documentation "ID of the mandate used to make this payment.")
  (sort-code
   :type (or string null)
   :documentation "Sort code of the bank account. (e.g., `10-20-30`)"))

(define-object charge-bancontact-details ()
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
was generated by this Charge.")
  (generated-sepa-debit-mandate
   :type (or string mandate null)
   :documentation "The mandate for the SEPA Direct Debit PaymentMethod
which was generated by this Charge.")
  (iban-last4
   :type (or string null)
   :documentation "Last four characters of the IBAN.")
  (preferred-language
   :type (or string null)
   :documentation "Preferred language of the Bancontact authorization
page that the customer is redirected to.

Can be one of `en`, `de`, `fr`, or `nl`.")
  (verified-name
   :type (or string null)
   :documentation "Owner's verified full name. Values are verified or
provided by Bancontact directly (if supported) at the time of
authorization or settlement. They cannot be set or mutated."))

(define-object charge-blik-details ()
  (buyer-id
   :type (or string null)
   :documentation "A unique and immutable identifier assigned by BLIK to
every buyer."))

(define-object charge-boleto-details ()
  (tax-id
   :type (or string null)
   :documentation "The tax ID of the customer (CPF for individuals
consumers or CNPJ for businesses consumers)."))

(define-object charge-card-details ()
  (amount-authorized
   :type (or integer null)
   :documentation "The authorized amount.")
  (authorization-code
   :type (or string null)
   :documentation "Authorization code on the charge.")
  (brand
   :type (or string null)
   :documentation "Card brand. Can be `amex`, `diners`, `discover`,
`eftpos_au`,`jcb`, `mastercard`, `unionpay`, `visa`, or `unknown`.")
  (capture-before
   :type (or local-time:timestamp null)
   :documentation "When using manual capture, a future timestamp at
which the charge will be automatically refunded if uncaptured.")
  (checks
   :type (or charge-card-checks null)
   :documentation "Check results by Card networks on Card address and
CVC at time of payment.")
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the country of the
card. You could use this attribute to get a sense of the international
breakdown of cards you've collected.")
  (exp-month
   :type integer
   :documentation "Two-digit number representing the card's expiration
month.")
  (exp-year
   :type integer
   :documentation "Four-digit number representing the card's expiration
year.")
  (extended-authorization
   :type (or charge-card-extended-authorization null)
   :documentation "Whether the capture window of this charge is
extended.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular card number. You
can use this attribute to check whether two customers who've signed up
with you are using the same card number, for example. For payment
methods that tokenize card information (Apple Pay, Google Pay), the
tokenized number might be provided instead of the underlying card
number.

As of May 1, 2021, card fingerprint in India for Connect changed to
allow two fingerprints for the same card---one for India and one for
the rest of the world.")
  (funding
   :type (or string null)
   :documentation "Card funding type. Can be `credit`, `debit`,
`prepaid`, or `unknown`.")
  (incremental-authorization
   :type (or charge-card-incremental-authorization null)
   :documentation "Whether the authorized amount can be incremented or
not.")
  (installments
   :type (or charge-card-installments null)
   :documentation "Installment details for this payment (Mexico only).
For more information, see the [installments integration guide]
(https://stripe.com/docs/payments/installments).")
  (last4
   :type (or string null)
   :documentation "The last four digits of the card.")
  (mandate
   :type (or string null)
   :documentation "ID of the mandate used to make this payment or
created by it.")
  (multicapture
   :type (or charge-card-multicapture null)
   :documentation "Information about the multicapture capability of the
payment method.")
  (network
   :type (or string null)
   :documentation "Identifies which network this charge was processed
on. Can be `amex`, `cartes_bancaires`, `diners`, `discover`,
`eftpos_au`, `interac`, `jcb`,`mastercard`, `unionpay`, `visa`, or
`unknown`.")
  (network-token
   :type (or charge-card-network-token null)
   :documentation "If this card has network token credentials, this
contains the details of the network token credentials.")
  (overcapture
   :type (or charge-card-overcapture null)
   :documentation "Whether the authorized amount can be over-captured
or not.")
  (three-d-secure
   :type (or charge-card-three-d-secure null)
   :documentation "Populated if this transaction used 3D Secure
authentication.")
  (wallet
   :type (or charge-card-wallet null)
   :documentation "If this Card is part of a card wallet, this contains
the details of the card wallet."))

(define-object charge-card-present-details ()
  (amount-authorized
   :type (or integer null)
   :documentation "The authorized amount.")
  (brand
   :type (or string null)
   :documentation "Card brand. Can be `amex`, `diners`, `discover`,
`eftpos_au`,`jcb`, `mastercard`, `unionpay`, `visa`, or `unknown`.")
  (brand-product
   :type (or string null)
   :documentation "The [product code]
(https://stripe.com/docs/card-product-codes) that identifies the
specific program or product associated with a card.")
  (capture-before
   :type (or local-time:timestamp null)
   :documentation "When using manual capture, a future timestamp after
which the charge will be automatically refunded if uncaptured.")
  (cardholder-name
   :type (or string null)
   :documentation "The cardholder name as read from the card, in
[ISO 7813] (https://en.wikipedia.org/wiki/ISO/IEC_7813) format. May
include alphanumeric characters, special characters and first/last
name separator (`/`). In some cases, the cardholder name may not be
available depending on how the issuer has configured the card.
Cardholder name is typically not available on swipe or contactless
payments, such as those made with Apple Pay and Google Pay.")
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
  (emv-auth-data
   :type (or string null)
   :documentation "Authorization response cryptogram.")
  (exp-month
   :type integer
   :documentation "Two-digit number representing the card's expiration
month.")
  (exp-year
   :type integer
   :documentation "Four-digit number representing the card's
expiration year.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular card number.
You can use this attribute to check whether two customers who've
signed up with you are using the same card number, for example. For
payment methods that tokenize card information (Apple Pay,
Google Pay), the tokenized number might be provided instead of the
underlying card number.

As of May 1, 2021, card fingerprint in India for Connect changed to
allow two fingerprints for the same card---one for India and one for
the rest of the world.")
  (funding
   :type (or string null)
   :documentation "Card funding type. Can be `credit`, `debit`,
`prepaid`, or `unknown`.")
  (generated-card
   :type (or string null)
   :documentation "ID of a card PaymentMethod generated from the
card_present PaymentMethod that may be attached to a Customer for
future transactions. Only present if it was possible to generate a
card PaymentMethod.")
  (incremental-authorization
   :type boolean
   :documentation "Whether this [PaymentIntent]
(https://stripe.com/docs/api/payment_intents) is eligible for incremental
authorizations. Request support using [request_incremental_authorization_support]
(https://stripe.com/docs/api/payment_intents/create#create_payment_intent-payment_method_options-card_present-request_incremental_authorization_support).")
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
`eftpos_au`, `interac`, `jcb`,`mastercard`, `unionpay`, `visa`, or
`unknown`.")
  (network-transaction-id
   :type (or string null)
   :documentation "This is used by the financial networks to identify a
transaction. Visa calls this the Transaction ID, Mastercard calls this
the Trace ID, and American Express calls this the Acquirer Reference
Data. The first three digits of the Trace ID is the Financial Network
Code, the next 6 digits is the Banknet Reference Number, and the last
4 digits represent the date (MM/DD). This field will be available for
successful Visa, Mastercard, or American Express transactions and
always null for other card brands.")
  (offline
   :type (or charge-card-present-offline null)
   :documentation "Details about payments collected offline.")
  (overcapture-supported
   :type boolean
   :documentation "Defines whether the authorized amount can be
over-captured or not.")
  (preferred-locales
   :type (or (vector string) null)
   :documentation "EMV tag 5F2D. Preferred languages specified by the
integrated circuit chip.")
  (read-method
   :type (or string null)
   :documentation "How card details were read in this transaction. One
of `contact_emv`, `contactless_emv`, `contactless_magstripe_mode`,
`magnetic_stripe_fallback`, or `magnetic_stripe_track2`.")
  (receipt
   :type (or charge-card-present-receipt null)
   :documentation "A collection of fields required to be displayed on
receipts. Only required for EMV transactions.")
  (wallet
   :type (or charge-card-present-wallet null)
   :documentation "If a mobile wallet was presented in the transaction,
this contains the details of the mobile wallet."))

(define-object charge-cashapp-details ()
  (buyer-id
   :type (or string null)
   :documentation "A unique and immutable identifier assigned by Cash
App to every buyer.")
  (cashtag
   :type (or string null)
   :documentation "A public identifier for buyers using Cash App."))

(define-object charge-customer-balance-details ())

(define-object charge-eps-details ()
  (bank
   :type (or string null)
   :documentation "The customer's bank. Should be one of
`arzte_und_apotheker_bank`, `austrian_anadi_bank_ag`, `bank_austria`,
`bankhaus_carl_spangler`, `bankhaus_schelhammer_und_schattera_ag`,
`bawag_psk_ag`, `bks_bank_ag`, `brull_kallmus_bank_ag`,
`btv_vier_lander_bank`,`capital_bank_grawe_gruppe_ag`,
`deutsche_bank_ag`, `dolomitenbank`,`easybank_ag`,
`erste_bank_und_sparkassen`,`hypo_alpeadriabank_international_ag`,
`hypo_noe_lb_fur_niederosterreich_u_wien`,
`hypo_oberosterreich_salzburg_steiermark`, `hypo_tirol_bank_ag`,
`hypo_vorarlberg_bank_ag`, `hypo_bank_burgenland_aktiengesellschaft`,
`marchfelder_bank`, `oberbank_ag`,
`raiffeisen_bankengruppe_osterreich`,`schoellerbank_ag`,
`sparda_bank_wien`, `volksbank_gruppe`, `volkskreditbank_ag`, or
`vr_bank_braunau`.")
  (verified-name
   :type (or string null)
   :documentation "Owner's verified full name. Values are verified or
provided by EPS directly (if supported) at the time of authorization
or settlement. They cannot be set or mutated. EPS rarely provides this
information so the attribute is usually empty."))

(define-object charge-fpx-details ()
  (bank
   :type (or string null)
   :documentation "The customer's bank.

Can be one of `affin_bank`, `agrobank`,`alliance_bank`, `ambank`,
`bank_islam`,`bank_muamalat`, `bank_rakyat`, `bsn`,`cimb`,
`hong_leong_bank`, `hsbc`, `kfh`,`maybank2u`, `ocbc`,
`public_bank`,`rhb`, `standard_chartered`, `uob`,`deutsche_bank`,
`maybank2e`,`pb_enterprise`, or `bank_of_china`.")
  (transaction-id
   :type (or string null)
   :documentation "Unique transaction id generated by FPX for every
request from the merchant."))

(define-object charge-giropay-details ()
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
  (verified-name
   :type (or string null)
   :documentation "Owner's verified full name. Values are verified or
provided by Giropay directly (if supported) at the time of
authorization or settlement. They cannot be set or mutated. Giropay
rarely provides this information so the attribute is usually empty."))

(define-object charge-grabpay-details ()
  (transaction-id
   :type (or string null)
   :documentation "Unique transaction id generated by GrabPay."))

(define-object charge-ideal-details ()
  (bank
   :type (or string null)
   :documentation "The customer's bank.

Can be one of `abn_amro`, `asn_bank`,`bunq`, `handelsbanken`, `ing`,
`knab`,`moneyou`, `n26`, `nn`, `rabobank`,`regiobank`, `revolut`,
`sns_bank`,`triodos_bank`, `van_lanschot`, or `yoursafe`.")
  (bic
   :type (or string null)
   :documentation "The Bank Identifier Code of the customer's bank.")
  (generated-sepa-debit
   :type (or string payment-method null)
   :documentation "The ID of the SEPA Direct Debit PaymentMethod which
was generated by this Charge.")
  (generated-sepa-debit-mandate
   :type (or string mandate null)
   :documentation "The mandate for the SEPA Direct Debit PaymentMethod
which was generated by this Charge.")
  (iban-last4
   :type (or string null)
   :documentation "Last four characters of the IBAN.")
  (verified-name
   :type (or string null)
   :documentation "Owner's verified full name. Values are verified or
provided by IDEAL directly (if supported) at the time of authorization
or settlement. They cannot be set or mutated."))

(define-object charge-interac-present-details ()
  (brand
   :type (or string null)
   :documentation "Card brand. Can be `interac`, `mastercard` or
`visa`.")
  (cardholder-name
   :type (or string null)
   :documentation "The cardholder name as read from the card, in
[ISO 7813] (https://en.wikipedia.org/wiki/ISO/IEC_7813) format. May
include alphanumeric characters, special characters and first/last
name separator (`/`). In some cases, the cardholder name may not be
available depending on how the issuer has configured the card.
Cardholder name is typically not available on swipe or contactless
payments, such as those made with Apple Pay and Google Pay.")
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the country of the
card. You could use this attribute to get a sense of the international
breakdown of cards you've collected.")
  (description
   :type (or string null)
   :documentation "A high-level description of the type of cards
issued in this range. (For internal use only and not typically
available in standard API requests.)")
  (emv-auth-data
   :type (or string null)
   :documentation "Authorization response cryptogram.")
  (exp-month
   :type integer
   :documentation "Two-digit number representing the card's expiration
month.")
  (exp-year
   :type integer
   :documentation "Four-digit number representing the card's
expiration year.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular card number.
You can use this attribute to check whether two customers who've
signed up with you are using the same card number, for example. For
payment methods that tokenize card information (Apple Pay, Google
Pay), the tokenized number might be provided instead of the
underlying card number.

As of May 1, 2021, card fingerprint in India for Connect changed to
allow two fingerprints for the same card---one for India and one for
the rest of the world.")
  (funding
   :type (or string null)
   :documentation "Card funding type. Can be `credit`, `debit`,
`prepaid`, or `unknown`.")
  (generated-card
   :type (or string null)
   :documentation "ID of a card PaymentMethod generated from the
card_present PaymentMethod that may be attached to a Customer for
future transactions. Only present if it was possible to generate a
card PaymentMethod.")
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
`eftpos_au`, `interac`, `jcb`,`mastercard`, `unionpay`, `visa`, or
`unknown`.")
  (network-transaction-id
   :type (or string null)
   :documentation "This is used by the financial networks to identify a
transaction. Visa calls this the Transaction ID, Mastercard calls this
the Trace ID, and American Express calls this the Acquirer Reference
Data. The first three digits of the Trace ID is the Financial Network
Code, the next 6 digits is the Banknet Reference Number, and the last
4 digits represent the date (MM/DD). This field will be available for
successful Visa, Mastercard, or American Express transactions and
always null for other card brands.")
  (preferred-locales
   :type (or (vector string) null)
   :documentation "EMV tag 5F2D. Preferred languages specified by the
integrated circuit chip.")
  (read-method
   :type (or string null)
   :documentation "How card details were read in this transaction. One
of `contact_emv`, `contactless_emv`, `contactless_magstripe_mode`,
`magnetic_stripe_fallback`, or `magnetic_stripe_track2`.")
  (receipt
   :type (or charge-card-present-receipt null)
   :documentation "A collection of fields required to be displayed on
receipts. Only required for EMV transactions."))

(define-object charge-kakao-pay-details ()
  (buyer-id
   :type (or string null)
   :documentation "A unique identifier for the buyer as determined by
the local payment processor."))

(define-object charge-klarna-details ()
  (payer-details
   :type t
   :documentation "The payer details for this transaction.")
  (payment-method-category
   :type (or string null)
   :documentation "The Klarna payment method used for this transaction.
Can be one of `pay_later`, `pay_now`, `pay_with_financing`, or
`pay_in_installments`")
  (preferred-locale
   :type (or string null)
   :documentation "Preferred language of the Klarna authorization page
that the customer is redirected to.

Can be one of `de-AT`, `en-AT`, `nl-BE`, `fr-BE`, `en-BE`, `de-DE`,
`en-DE`,`da-DK`, `en-DK`, `es-ES`, `en-ES`, `fi-FI`, `sv-FI`, `en-FI`,
`en-GB`, `en-IE`,`it-IT`, `en-IT`, `nl-NL`, `en-NL`, `nb-NO`, `en-NO`,
`sv-SE`, `en-SE`, `en-US`,`es-US`, `fr-FR`, `en-FR`, `cs-CZ`, `en-CZ`,
`ro-RO`, `en-RO`, `el-GR`, `en-GR`,`en-AU`, `en-NZ`, `en-CA`, `fr-CA`,
`pl-PL`, `en-PL`, `pt-PT`, `en-PT`, `de-CH`,`fr-CH`, `it-CH`, or
`en-CH`"))

(define-object charge-konbini-details ()
  (store
   :type (or charge-konbini-store null)
   :documentation "If the payment succeeded, this contains the details
of the convenience store where the payment was completed."))

(define-object charge-konbini-store ()
  (chain
   :type (or string null)
   :documentation "The name of the convenience store chain where the
payment was completed. One of `familymart`, `lawson`, `ministop`, or
`seicomart`."))

(define-object charge-kr-card-details ()
  (brand
   :type (or string null)
   :documentation "The local credit or debit card brand.")
  (buyer-id
   :type (or string null)
   :documentation "A unique identifier for the buyer as determined by
the local payment processor.")
  (last4
   :type (or string null)
   :documentation "The last four digits of the card. This may not be
present for American Express cards."))

(define-object charge-link-details ()
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the funding source
country beneath the Link payment.

You could use this attribute to get a sense of international fees."))

(define-object charge-mobilepay-details ()
  (card
   :type (or charge-mobilepay-card null)
   :documentation "Internal card details."))

(define-object charge-mobilepay-card ()
  (brand
   :type (or string null)
   :documentation "Brand of the card used in the transaction.")
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the country of the
card.")
  (exp-month
   :type integer
   :documentation "Two-digit number representing the card's expiration
month.")
  (exp-year
   :type integer
   :documentation "Four-digit number representing the card's expiration
year.")
  (last4
   :type (or string null)
   :documentation "The last four digits of the card."))

(define-object charge-multibanco-details ()
  (entity
   :type (or string null)
   :documentation "Entity number associated with this Multibanco
payment.")
  (reference
   :type (or string null)
   :documentation "Reference number associated with this Multibanco
payment."))

(define-object charge-naver-pay-details ()
  (buyer-id
   :type (or string null)
   :documentation "A unique identifier for the buyer as determined by
the local payment processor."))

(define-object charge-oxxo-details ()
  (number
   :reader charge-oxxo-details-number
   :type (or string null)
   :documentation "OXXO reference number."))

(define-object charge-p24-details ()
  (bank
   :type (or string null)
   :documentation "The customer's bank.

Can be one of `ing`, `citi_handlowy`,`tmobile_usbugi_bankowe`,
`plus_bank`,`etransfer_pocztowy24`,`banki_spbdzielcze`,
`bank_nowy_bfg_sa`, `getin_bank`,`velobank`, `blik`,`noble_pay`,
`ideabank`, `envelobank`, `santander_przelew24`,`nest_przelew`,
`mbank_mtransfer`, `inteligo`, `pbac_z_ipko`, `bnp_paribas`,
`credit_agricole`,`toyota_bank`, `bank_pekao_sa`, `volkswagen_bank`,
`bank_millennium`,`alior_bank`, or `boz`.")
  (reference
   :type (or string null)
   :documentation "Unique reference for this Przelewy24 payment.")
  (verified-name
   :type (or string null)
   :documentation "Owner's verified full name. Values are verified or
provided by Przelewy24 directly (if supported) at the time of
authorization or settlement. They cannot be set or mutated. Przelewy24
rarely provides this information so the attribute is usually empty."))

(define-object charge-payco-details ()
  (buyer-id
   :type (or string null)
   :documentation "A unique identifier for the buyer as determined by
the local payment processor."))

(define-object charge-paynow-details ()
  (reference
   :type (or string null)
   :documentation "Reference number associated with this PayNow
payment."))

(define-object charge-paypal-details ()
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the buyer’s
country. Values are provided by PayPal directly (if supported) at the
time of authorization or settlement. They cannot be set or mutated.")
  (payer-email
   :type (or string null)
   :documentation "Owner's email. Values are provided by PayPal
directly (if supported) at the time of authorization or settlement.
They cannot be set or mutated.")
  (payer-id
   :type (or string null)
   :documentation "PayPal account PayerID. This identifier uniquely
identifies the PayPal customer.")
  (payer-name
   :type (or string null)
   :documentation "Owner's full name. Values provided by PayPal directly
(if supported) at the time of authorization or settlement. They cannot
be set or mutated.")
  (seller-protection
   :type (or charge-paypal-seller-protection null)
   :documentation "The level of protection offered as defined by PayPal
Seller Protection for Merchants, for this transaction.")
  (transaction-id
   :type (or string null)
   :documentation "A unique ID generated by PayPal for this transaction."))

(define-object charge-paypal-seller-protection ()
  (dispute-categories
   :type (or (vector string) null)
   :documentation "An array of conditions that are covered for the
transaction, if applicable. One of `fraudulent` or
`product_not_received`.")
  (status
   :type (or string null)
   :documentation "Indicates whether the transaction is eligible for PayPal's
seller protection. One of `eligible`, `not_eligible`, or
`partially_eligible`."))

(define-object charge-pix-details ()
  (bank-transaction-id
   :type (or string null)
   :documentation "Unique transaction id generated by BCB."))

(define-object charge-promptpay-details ()
  (reference
   :type (or string null)
   :documentation "Bill reference generated by PromptPay."))

(define-object charge-revolut-pay-details ())

(define-object charge-samsung-pay-details ()
  (buyer-id
   :type (or string null)
   :documentation "A unique identifier for the buyer as determined by
the local payment processor."))

(define-object charge-sepa-debit-details ()
  (bank-code
   :type (or string null)
   :documentation "Bank code of bank associated with the bank
account.")
  (branch-code
   :type (or string null)
   :documentation "Branch code of bank associated with the bank
account.")
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the country the
bank account is located in.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular bank account.
You can use this attribute to check whether two bank accounts are the
same.")
  (last4
   :type (or string null)
   :documentation "Last four characters of the IBAN.")
  (mandate
   :type (or string null)
   :documentation "Find the ID of the mandate used for this payment under the
[payment_method_details.sepa_debit.mandate]
(https://stripe.com/docs/api/charges/object#charge_object-payment_method_details-sepa_debit-mandate)
property on the Charge. Use this mandate ID to [retrieve the Mandate]
(https://stripe.com/docs/api/mandates/retrieve)."))

(define-object charge-sofort-details ()
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
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the country the
bank account is located in.")
  (generated-sepa-debit
   :type (or string payment-method null)
   :documentation "The ID of the SEPA Direct Debit PaymentMethod which
was generated by this Charge.")
  (generated-sepa-debit-mandate
   :type (or string mandate null)
   :documentation "The mandate for the SEPA Direct Debit PaymentMethod
which was generated by this Charge.")
  (iban-last4
   :type (or string null)
   :documentation "Last four characters of the IBAN.")
  (preferred-language
   :type (or string null)
   :documentation "Preferred language of the SOFORT authorization page
that the customer is redirected to.

Can be one of `de`, `en`, `es`, `fr`, `it`, `nl`, or `pl`")
  (verified-name
   :type (or string null)
   :documentation "Owner's verified full name. Values are verified or
provided by SOFORT directly (if supported) at the time of authorization
or settlement. They cannot be set or mutated."))

(define-object charge-stripe-account-details ())

(define-object charge-swish-details ()
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies the payer's Swish account. You
can use this attribute to check whether two Swish transactions were
paid for by the same payer.")
  (payment-reference
   :type (or string null)
   :documentation "Payer bank reference number for the payment.")
  (verified-phone-last4
   :type (or string null)
   :documentation "The last four digits of the Swish account phone
number."))

(define-object charge-twint-details ())

(define-object charge-us-bank-account-details ()
  (account-holder-type
   :type (or string null)
   :documentation "Account holder type: individual or company.")
  (account-type
   :type (or string null)
   :documentation "Account type: checkings or savings. Defaults to
checking if omitted.")
  (bank-name
   :type (or string null)
   :documentation "Name of the bank associated with the bank account.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular bank account.
You can use this attribute to check whether two bank accounts are the
same.")
  (last4
   :type (or string null)
   :documentation "Last four digits of the bank account number.")
  (mandate
   :type (or string null)
   :documentation "ID of the mandate used to make this payment.")
  (payment-reference
   :type (or string null)
   :documentation "Reference number to locate ACH payments with
customer's bank.")
  (routing-number
   :type (or string null)
   :documentation "Routing number of the bank account."))

(define-object charge-wechat-details ())

(define-object charge-wechat-pay-details ()
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular WeChat Pay
account. You can use this attribute to check whether two WeChat
accounts are the same.")
  (transaction-id
   :type (or string null)
   :documentation "Transaction ID of this particular WeChat Pay
transaction."))

(define-object charge-zip-details ())

(defmethod initialize-instance :after ((instance charge-card-details) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:checks
           (when value
             (setf (slot-value instance '%checks)
                   (make-instance 'charge-card-checks :data value)))))))))

(define-object charge-payment-method-details ()
  ;; Define each potential payment method details object
  (ach-credit-transfer
   :type (or charge-ach-credit-transfer-details null))
  (ach-debit
   :type (or charge-ach-debit-details null))
  (acss-debit
   :type (or charge-acss-debit-details null))
  (affirm
   :type (or charge-affirm-details null))
  (afterpay-clearpay
   :type (or charge-afterpay-clearpay-details null))
  (alipay
   :type (or charge-alipay-details null))
  (amazon-pay
   :type (or charge-amazon-pay-details null))
  (au-becs-debit
   :type (or charge-au-becs-debit-details null))
  (bacs-debit
   :type (or charge-bacs-debit-details null))
  (bancontact
   :type (or charge-bancontact-details null))
  (blik
   :type (or charge-blik-details null))
  (boleto
   :type (or charge-boleto-details null))
  (card
   :type (or charge-card-details null))
  (card-present
   :type (or charge-card-present-details null))
  (cashapp
   :type (or charge-cashapp-details null))
  (customer-balance
   :type (or charge-customer-balance-details null))
  (eps
   :type (or charge-eps-details null))
  (fpx
   :type (or charge-fpx-details null))
  (giropay
   :type (or charge-giropay-details null))
  (grabpay
   :type (or charge-grabpay-details null))
  (ideal
   :type (or charge-ideal-details null))
  (interac-present
   :type (or charge-interac-present-details null))
  (kakao-pay
   :type (or charge-kakao-pay-details null))
  (klarna
   :type (or charge-klarna-details null))
  (konbini
   :type (or charge-konbini-details null))
  (kr-card
   :type (or charge-kr-card-details null))
  (link
   :type (or charge-link-details null))
  (mobilepay
   :type (or charge-mobilepay-details null))
  (multibanco
   :type (or charge-multibanco-details null))
  (naver-pay
   :type (or charge-naver-pay-details null))
  (oxxo
   :type (or charge-oxxo-details null))
  (p24
   :type (or charge-p24-details null))
  (payco
   :type (or charge-payco-details null))
  (paynow
   :type (or charge-paynow-details null))
  (paypal
   :type (or charge-paypal-details null))
  (pix
   :type (or charge-pix-details null))
  (promptpay
   :type (or charge-promptpay-details null))
  (revolut-pay
   :type (or charge-revolut-pay-details null))
  (samsung-pay
   :type (or charge-samsung-pay-details null))
  (sepa-debit
   :type (or charge-sepa-debit-details null))
  (sofort
   :type (or charge-sofort-details null))
  (stripe-account
   :type (or charge-stripe-account-details null))
  (swish
   :type (or charge-swish-details null))
  (twint
   :type (or charge-twint-details null))
  (us-bank-account
   :type (or charge-us-bank-account-details null))
  (wechat
   :type (or charge-wechat-details null))
  (wechat-pay
   :type (or charge-wechat-pay-details null))
  (zip
   :type (or charge-zip-details null))
  ;; This field accounts for which payment method detail object is relevant
  (type
   :reader charge-payment-method-details-type
   :type string))

(defmethod initialize-instance :after ((instance charge-payment-method-details)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:ach-credit-transfer
           (when value
             (setf (slot-value instance '%ach-credit-transfer)
                   (make-instance 'charge-ach-credit-transfer-details :data value))))
          (:ach-debit
           (when value
             (setf (slot-value instance '%ach-debit)
                   (make-instance 'charge-ach-debit-details :data value))))
          (:affirm
           (when value
             (setf (slot-value instance '%affirm)
                   (make-instance 'charge-affirm-details :data value))))
          (:afterpay-clearpay
           (when value
             (setf (slot-value instance '%afterpay-clearpay)
                   (make-instance 'charge-afterpay-clearpay-details :data value))))
          (:alipay
           (when value
             (setf (slot-value instance '%alipay)
                   (make-instance 'charge-alipay-details :data value))))
          (:amazon-pay
           (when value
             (setf (slot-value instance '%amazon-pay)
                   (make-instance 'charge-amazon-pay-details :data value))))
          (:au-becs-debit
           (when value
             (setf (slot-value instance '%au-becs-debit)
                   (make-instance 'charge-au-becs-debit-details :data value))))
          (:bacs-debit
           (when value
             (setf (slot-value instance '%bacs-debit)
                   (make-instance 'charge-bacs-debit-details :data value))))
          (:bancontact
           (when value
             (setf (slot-value instance '%bancontact)
                   (make-instance 'charge-bancontact-details :data value))))
          (:blik
           (when value
             (setf (slot-value instance '%blik)
                   (make-instance 'charge-blik-details :data value))))
          (:boleto
           (when value
             (setf (slot-value instance '%boleto)
                   (make-instance 'charge-boleto-details :data value))))
          (:card
           (when value
             (setf (slot-value instance '%card)
                   (make-instance 'charge-card-details :data value))))
          (:card-present
           (when value
             (setf (slot-value instance '%card-present)
                   (make-instance 'charge-card-present-details :data value))))
          (:cashapp
           (when value
             (setf (slot-value instance '%cashapp)
                   (make-instance 'charge-cashapp-details :data value))))
          (:customer-balance
           (when value
             (setf (slot-value instance '%customer-balance)
                   (make-instance 'charge-customer-balance-details :data value))))
          (:eps
           (when value
             (setf (slot-value instance '%eps)
                   (make-instance 'charge-eps-details :data value))))
          (:fpx
           (when value
             (setf (slot-value instance '%fpx)
                   (make-instance 'charge-fpx-details :data value))))
          (:giropay
           (when value
             (setf (slot-value instance '%giropay)
                   (make-instance 'charge-giropay-details :data value))))
          (:grabpay
           (when value
             (setf (slot-value instance '%grabpay)
                   (make-instance 'charge-grabpay-details :data value))))
          (:ideal
           (when value
             (setf (slot-value instance '%ideal)
                   (make-instance 'charge-ideal-details :data value))))
          (:interac-present
           (when value
             (setf (slot-value instance '%interac-present)
                   (make-instance 'charge-interac-present-details :data value))))
          (:kakao-pay
           (when value
             (setf (slot-value instance '%kakao-pay)
                   (make-instance 'charge-kakao-pay-details :data value))))
          (:klarna
           (when value
             (setf (slot-value instance '%klarna)
                   (make-instance 'charge-klarna-details :data value))))
          (:konbini
           (when value
             (setf (slot-value instance '%konbini)
                   (make-instance 'charge-konbini-details :data value))))
          (:kr-card
           (when value
             (setf (slot-value instance '%kr-card)
                   (make-instance 'charge-kr-card-details :data value))))
          (:link
           (when value
             (setf (slot-value instance '%link)
                   (make-instance 'charge-link-details :data value))))
          (:mobilepay
           (when value
             (setf (slot-value instance '%mobilepay)
                   (make-instance 'charge-mobilepay-details :data value))))
          (:multibanco
           (when value
             (setf (slot-value instance '%multibanco)
                   (make-instance 'charge-multibanco-details :data value))))
          (:naver-pay
           (when value
             (setf (slot-value instance '%naver-pay)
                   (make-instance 'charge-naver-pay-details :data value))))
          (:oxxo
           (when value
             (setf (slot-value instance '%oxxo)
                   (make-instance 'charge-oxxo-details :data value))))
          (:p24
           (when value
             (setf (slot-value instance '%p24)
                   (make-instance 'charge-p24-details :data value))))
          (:payco
           (when value
             (setf (slot-value instance '%payco)
                   (make-instance 'charge-payco-details :data value))))
          (:paynow
           (when value
             (setf (slot-value instance '%paynow)
                   (make-instance 'charge-paynow-details :data value))))
          (:paypal
           (when value
             (setf (slot-value instance '%paypal)
                   (make-instance 'charge-paypal-details :data value))))
          (:pix
           (when value
             (setf (slot-value instance '%pix)
                   (make-instance 'charge-pix-details :data value))))
          (:promptpay
           (when value
             (setf (slot-value instance '%promptpay)
                   (make-instance 'charge-promptpay-details :data value))))
          (:revolut-pay
           (when value
             (setf (slot-value instance '%revolut-pay)
                   (make-instance 'charge-revolut-pay-details :data value))))
          (:samsung-pay
           (when value
             (setf (slot-value instance '%samsung-pay)
                   (make-instance 'charge-samsung-pay-details :data value))))
          (:sepa-debit
           (when value
             (setf (slot-value instance '%sepa-debit)
                   (make-instance 'charge-sepa-debit-details :data value))))
          (:sofort
           (when value
             (setf (slot-value instance '%sofort)
                   (make-instance 'charge-sofort-details :data value))))
          (:stripe-account
           (when value
             (setf (slot-value instance '%stripe-account)
                   (make-instance 'charge-stripe-account-details :data value))))
          (:swish
           (when value
             (setf (slot-value instance '%swish)
                   (make-instance 'charge-swish-details :data value))))
          (:twint
           (when value
             (setf (slot-value instance '%twint)
                   (make-instance 'charge-twint-details :data value))))
          (:us-bank-account
           (when value
             (setf (slot-value instance '%us-bank-account)
                   (make-instance 'charge-us-bank-account-details :data value))))
          (:wechat
           (when value
             (setf (slot-value instance '%wechat)
                   (make-instance 'charge-wechat-details :data value))))
          (:wechat-pay
           (when value
             (setf (slot-value instance '%wechat-pay)
                   (make-instance 'charge-wechat-pay-details :data value))))
          (:zip
           (when value
             (setf (slot-value instance '%zip)
                   (make-instance 'charge-zip-details :data value)))))))))

(defmethod initialize-instance :after ((instance charge) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:billing-details
           (unless (eql 'null value)
             (setf (slot-value instance '%billing-details)
                   (make-instance 'billing-details :data value))))
          (:created
           (setf (slot-value instance '%created)
                 (decode-timestamp value)))
          (:fraud-details
           (unless (eql 'null value)
             (setf (slot-value instance '%fraud-details)
                   (make-instance 'charge-fraud-details :data value))))
          (:outcome
           (unless (eql 'null value)
             (setf (slot-value instance '%outcome)
                   (make-instance 'charge-outcome :data value))))
          (:payment-method-details
           (unless (eql 'null value)
             (setf (slot-value instance '%payment-method-details)
                   (make-instance 'charge-payment-method-details :data value))))
          (:refunds
           (when value
             (setf (slot-value instance '%refunds)
                   (decode-hash-table value)))))))))
