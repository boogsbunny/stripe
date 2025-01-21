(in-package #:stripe)

(define-object payment-method ()
  "PaymentMethod objects represent your customer's payment instruments.
You can use them with [PaymentIntents]
(https://stripe.com/docs/payments/payment-intents) to collect payments
or save them to Customer objects to store instrument details for future payments.

Related guides: [Payment Methods]
(https://stripe.com/docs/payments/payment-methods) and [More Payment
Scenarios](https://stripe.com/docs/payments/more-payment-scenarios)."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "payment_method"
   :documentation "String representing the object's type. Objects of
   the same type share the same value.")
  (acss-debit
   :type (or payment-method-acss-debit null))
  (affirm
   :type (or payment-method-affirm null))
  (afterpay-clearpay
   :type (or payment-method-afterpay-clearpay null))
  (alipay
   :type (or payment-method-alipay null))
  (allow-redisplay
   :type (or string null)
   :documentation "This field indicates whether this payment method can
be shown again to its customer in a checkout flow. Stripe products such
as Checkout and Elements use this field to determine whether a payment
method can be shown as a saved payment method in a checkout flow. The
field defaults to “unspecified”. One of `always`, `limited`, or
`unspecified`.")
  (alma
   :type (or payment-method-alma null))
  (amazon-pay
   :type (or payment-method-amazon-pay null))
  (au-becs-debit
   :type (or payment-method-au-becs-debit null))
  (bacs-debit
   :type (or payment-method-bacs-debit null))
  (bancontact
   :type (or payment-method-bancontact null))
  (billing-details
   :type payment-method-billing-details
   :documentation "Billing details associated with the PaymentMethod.")
  (blik
   :type (or payment-method-blik null))
  (boleto
   :type (or payment-method-boleto null))
  (card
   :type (or payment-method-card null))
  (card-present
   :type (or payment-method-card-present null))
  (cashapp
   :type (or payment-method-cashapp null))
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (customer
   :type (or string customer null)
   :documentation "The ID of the Customer to which this PaymentMethod
is saved. This will not be set when the PaymentMethod has not been
saved to a Customer.")
  (customer-balance
   :type (or payment-method-customer-balance null))
  (eps
   :type (or payment-method-eps null))
  (fpx
   :type (or payment-method-fpx null))
  (giropay
   :type (or payment-method-giropay null))
  (grabpay
   :type (or payment-method-grabpay null))
  (ideal
   :type (or payment-method-ideal null))
  (interac-present
   :type (or payment-method-interac-present null))
  (klarna
   :type (or payment-method-klarna null))
  (konbini
   :type (or payment-method-konbini null))
  (kr-card
   :type (or payment-method-kr-card null))
  (link
   :type (or payment-method-link null))
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of key-value pairs that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (mobilepay
   :type (or payment-method-mobilepay null))
  (multibanco
   :type (or payment-method-multibanco null))
  (oxxo
   :type (or payment-method-oxxo null))
  (p24
   :type (or payment-method-p24 null))
  (paynow
   :type (or payment-method-paynow null))
  (paypal
   :type (or payment-method-paypal null))
  (pix
   :type (or payment-method-pix null))
  (promptpay
   :type (or payment-method-promptpay null))
  (radar
   :type (or payment-method-radar-options null))
  (revolut-pay
   :type (or payment-method-revolut-pay null))
  (sepa
   :type (or payment-method-sepa-debit null))
  (sofort
   :type (or payment-method-sofort null))
  (swish
   :type (or payment-method-swish null))
  (twint
   :type (or payment-method-twint null))
  (type
   :reader payment-method-type
   :type string
   :documentation "The type of the PaymentMethod. An additional hash is
included on the PaymentMethod with a name matching this value. It
contains additional information specific to the PaymentMethod type.")
  (us-bank-account
   :type (or payment-method-us-bank-account null))
  (wechat-pay
   :type (or payment-method-wechat-pay null))
  (zip
   :type (or payment-method-zip null))
  (:list-type t))

(define-object payment-method-acss-debit ()
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
   :documentation  "Last four digits of the bank account number.")
  (transit-number
   :type (or string null)
   :documentation "Transit number of the bank account."))

(define-object payment-method-affirm ())

(define-object payment-method-afterpay-clearpay ())

(define-object payment-method-alipay ())

(define-object payment-method-alma ())

(define-object payment-method-amazon-pay ())

(define-object payment-method-au-becs-debit ()
  (bsb-number
   :type (or string null)
   :documentation "Six-digit number identifying bank and branch
associated with this bank account.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular bank account.
You can use this attribute to check whether two bank accounts are the
same.")
  (last4
   :type (or string null)
   :documentation "Last four digits of the bank account number."))

(define-object payment-method-bacs-debit ()
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular bank account.
You can use this attribute to check whether two bank accounts are the
same.")
  (last4
   :type (or string null)
   :documentation "Last four digits of the bank account number.")
  (sort-code
   :type (or string null)
   :documentation "Sort code of the bank account. (e.g., `10-20-30`)"))

(define-object payment-method-bancontact ())

(define-object payment-method-billing-details ()
  (address
   :type (or address null)
   :documentation "Billing address.")
  (email
   :type (or string null)
   :documentation "Email address.")
  (name
   :type (or string null)
   :documentation "Full name.")
  (phone
   :type (or string null)
   :documentation "Billing phone number (including extension)."))

(define-object payment-method-blik ())

(define-object payment-method-boleto ()
  (tax-id
   :type string
   :documentation "Uniquely identifies the customer tax id (CNPJ or
CPF)."))

(define-object payment-method-card ()
  (brand
   :type string
   :documentation "Card brand. Can be `amex`, `diners`, `discover`,
`eftpos_au`, `jcb`, `mastercard`, `unionpay`, `visa`, or `unknown`.")
  (checks
   :type (or payment-method-card-checks null)
   :documentation "Checks on Card address and CVC if provided.")
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
  (display-brand
   :type (or string null)
   :documentation "The brand to use when displaying the card, this
accounts for customer's brand choice on dual-branded cards. Can be
`american_express`, `cartes_bancaires`, `diners_club`, `discover`,
`eftpos_australia`, `interac`, `jcb`, `mastercard`, `union_pay`,
`visa`, or `other` and may contain more values in the future.")
  (exp-month
   :type integer
   :documentation "Two-digit number representing the card's expiration
month.")
  (exp-year
   :type integer
   :documentation "Four-digit number representing the card's expiration
year.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular card number. You
can use this attribute to check whether two customers who've signed up
with you are using the same card number, for example. For payment
methods that tokenize card information (Apple Pay, Google Pay), the
tokenized number might be provided instead of the underlying card
number.

*As of May 1, 2021, card fingerprint in India for Connect changed to
allow two fingerprints for the same card---one for India and one for
the rest of the world.*")
  (funding
   :type string
   :documentation "Card funding type. Can be `credit`, `debit`,
`prepaid`, or `unknown`.")
  (generated-from
   :type (or payment-method-card-generated-from null)
   :documentation "Details of the original PaymentMethod that created
this object.")
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
  (networks
   :type (or payment-method-card-networks null)
   :documentation "Contains information about card networks that can
be used to process the payment.")
  (three-d-secure-usage
   :type (or payment-method-card-three-d-secure-usage null)
   :documentation "Contains details on how this Card may be used for
3D Secure authentication.")
  (wallet
   :type (or payment-method-card-wallet null)
   :documentation "If this Card is part of a card wallet, this contains
the details of the card wallet."))

(define-object payment-method-card-checks ()
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

(define-object payment-method-card-generated-from ()
  (charge
   :type (or string null)
   :documentation "The charge that created this object.")
  (payment-method-details
   :type (or generated-from-payment-method-details null)
   :documentation "Transaction-specific details of the payment method
used in the payment.")
  (setup-attempt
   ;; TODO: make this expandable
   :type (or string null)
   :documentation "The ID of the SetupAttempt that generated this
PaymentMethod, if any."))

(define-object generated-from-payment-method-details ()
  (card-present
   :type (or generated-from-payment-method-details-card-present null))
  (type
   :reader payment-method-details-type
   :type string
   :documentation "The type of payment method transaction-specific
details from the transaction that generated this `card` payment method.
Always `card_present`."))

(define-object generated-from-payment-method-details-card-present ()
  (amount-authorized
   :type (or integer null)
   :documentation "The authorized amount.")
  (brand
   :type (or string null)
   :documentation "Card brand. Can be `amex`, `diners`, `discover`,
`eftpos_au`, `jcb`, `mastercard`, `unionpay`, `visa`, or `unknown`.")
  (brand-product
   :type (or string null)
   :documentation "The [product code]
(https://stripe.com/docs/card-product-codes) that identifies the
specific program or product associated with a card.")
  (capture-before
   :type (or integer null)
   :documentation "When using manual capture, a future timestamp after
which the charge will be automatically refunded if uncaptured.")
  (cardholder-name
   :type (or string null)
   :documentation "The cardholder name as read from the card, in
[ISO 7813](https://en.wikipedia.org/wiki/ISO/IEC_7813) format. May
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
   :documentation "Four-digit number representing the card's expiration
year.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular card number. You
can use this attribute to check whether two customers who've signed up
with you are using the same card number, for example. For payment
methods that tokenize card information (Apple Pay, Google Pay), the
tokenized number might be provided instead of the underlying card
number.

*As of May 1, 2021, card fingerprint in India for Connect changed to
allow two fingerprints for the same card---one for India and one for
the rest of the world.*")
  (funding
   :type (or string null)
   :documentation "Card funding type. Can be `credit`, `debit`,
`prepaid`, or `unknown`.")
  (generated-card
   :type (or string null)
   :documentation "ID of a card PaymentMethod generated from the
card_present PaymentMethod that may be attached to a Customer for
future transactions. Only present if it was possible to generate a card
PaymentMethod.")
  (iin
   :type (or string null)
   :documentation "Issuer identification number of the card. (For
internal use only and not typically available in standard API
requests.)")
  (incremental-authorization-supported
   :type boolean
   :documentation "Whether this [PaymentIntent]
(https://stripe.com/docs/api/payment_intents) is eligible for
incremental authorizations. Request support using
[request_incremental_authorization_support]
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
`eftpos_au`, `interac`, `jcb`, `mastercard`, `unionpay`, `visa`, or
`unknown`.")
  (network-transaction-id
   :type (or string null)
   :documentation "This is used by the financial networks to identify
a transaction. Visa calls this the Transaction ID, Mastercard calls
this the Trace ID, and American Express calls this the Acquirer
Reference Data. The first three digits of the Trace ID is the Financial
Network Code, the next 6 digits is the Banknet Reference Number, and
the last 4 digits represent the date (MM/DD). This field will be
available for successful Visa, Mastercard, or American Express
transactions and always null for other card brands.")
  (offline
   :type (or generated-from-payment-method-details-card-present-offline null)
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
   :type (or generated-from-payment-method-details-card-present-receipt null)
   :documentation "A collection of fields required to be displayed on
receipts. Only required for EMV transactions.")
  (wallet
   :type (or generated-from-payment-method-details-card-present-wallet null)))

(define-object generated-from-payment-method-details-card-present-offline ()
  (stored-at
   :type (or time:timestamp null)
   :documentation "Time at which the payment was collected while
offline.")
  (type
   :reader card-present-offline-type
   :type (or string null)
   :initform "deferred"
   :documentation "The method used to process this payment method
offline. Only deferred is allowed."))

(define-object generated-from-payment-method-details-card-present-receipt ()
  (account-type
   :type (or string null)
   :documentation "The type of account being debited or credited. One
of checking`, `credit`, `prepaid`, or `unknown`.")
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

(define-object generated-from-payment-method-details-card-present-wallet ()
  (type
   :reader wallet-type
   :type string
   :documentation "The type of mobile wallet, one of `apple_pay`,
`google_pay`, `samsung_pay`, or `unknown`."))

(define-object payment-method-card-networks ()
  (available
   :type (vector string)
   :documentation "All available networks for the card.")
  (preferred
   :type (or string null)
   :documentation "The preferred network for co-branded cards. Can be
`cartes_bancaires`, `mastercard`, `visa` or `invalid_preference` if
requested network is not valid for the card."))

(define-object payment-method-card-three-d-secure-usage ()
  (supported
   :type boolean
   :documentation "Whether 3D Secure is supported on this card."))

(define-object payment-method-card-wallet ()
  (amex-express-checkout
   :type (or payment-method-card-wallet-amex-express-checkout null))
  (apple-pay
   :type (or payment-method-card-wallet-apple-pay null))
  (dynamic-last4
   :type (or string null)
   :documentation "(For tokenized numbers only.) The last four digits
of the device account number.")
  (google-pay
   :type (or payment-method-card-wallet-google-pay null))
  (link
   :type (or payment-method-card-wallet-link null))
  (masterpass
   :type (or payment-method-card-wallet-masterpass null))
  (samsung-pay
   :type (or payment-method-card-wallet-samsung-pay null))
  (type
   :reader wallet-type
   :type (or string null)
   :documentation "The type of the card wallet, one of
`amex_express_checkout`, `apple_pay`, `google_pay`, `masterpass`,
`samsung_pay`, `visa_checkout`, or `link`. An additional hash is
included on the Wallet subhash with a name matching this value. It
contains additional information specific to the card wallet type.")
  (visa-checkout
   :type (or payment-method-card-wallet-visa-checkout null)))

(define-object payment-method-card-wallet-amex-express-checkout ())

(define-object payment-method-card-wallet-apple-pay ())

(define-object payment-method-card-wallet-google-pay ())

(define-object payment-method-card-wallet-link ())

(define-object payment-method-card-wallet-masterpass ()
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

(define-object payment-method-card-wallet-samsung-pay ())

(define-object payment-method-card-wallet-visa-checkout ()
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

(define-object payment-method-card-present ()
  (brand
   :type string
   :documentation "Card brand. Can be `amex`, `diners`, `discover`,
`eftpos_au`, `jcb`, `mastercard`, `unionpay`, `visa`, or `unknown`.")
  (brand-product
   :type (or string null)
   :documentation "The [product code]
(https://stripe.com/docs/card-product-codes) that identifies the
specific program or product associated with a card.")
  (cardholder-name
   :type (or string null)
   :documentation "The cardholder name as read from the card, in
[ISO 7813](https://en.wikipedia.org/wiki/ISO/IEC_7813) format. May
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
  (display-brand
   :type (or string null)
   :documentation "The brand to use when displaying the card, this
accounts for customer's brand choice on dual-branded cards. Can be
`american_express`, `cartes_bancaires`, `diners_club`, `discover`,
`eftpos_australia`, `interac`, `jcb`, `mastercard`, `union_pay`,
`visa`, or `other` and may contain more values in the future.")
  (exp-month
   :type integer
   :documentation "Two-digit number representing the card's expiration
month.")
  (exp-year
   :type integer
   :documentation "Four-digit number representing the card's expiration
year.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular card number. You
can use this attribute to check whether two customers who've signed up
with you are using the same card number, for example. For payment
methods that tokenize card information (Apple Pay, Google Pay), the
tokenized number might be provided instead of the underlying card
number.

*As of May 1, 2021, card fingerprint in India for Connect changed to
allow two fingerprints for the same card---one for India and one for
the rest of the world.*")
  (funding
   :type string
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
  (networks
   :type (or payment-method-card-present-networks null)
   :documentation "Contains information about card networks that can
be used to process the payment.")
  (offline
   :type (or payment-method-card-present-offline null)
   :documentation "Details about payment methods collected offline.")
  (preferred-locales
   :type (or (vector string) null)
   :documentation "EMV tag 5F2D. Preferred languages specified by the
integrated circuit chip.")
  (read-method
   :type (or string null)
   :documentation "How card details were read in this transaction. One
of `contact_emv`, `contactless_emv` , `contactless_magstripe_mode`,
`magnetic_stripe_fallback`, or `magnetic_stripe_track2`.")
  (wallet
   :type (or payment-method-card-wallet null)
   :documentation "If this Card is part of a card wallet, this contains
the details of the card wallet."))

(define-object payment-method-card-present-networks ()
  (available
   :type (vector string)
   :documentation "All available networks for the card.")
  (preferred
   :type (or string null)
   :documentation "The preferred network for the card."))

(define-object payment-method-card-present-offline ()
  (stored-at
   :type (or time:timestamp null)
   :documentation "Time at which the payment was collected while
offline.")
  (type
   :reader offline-type
   :type (or string null)
   :initform "deferred"
   :documentation "The method used to process this payment method
offline. Only deferred is allowed."))

(define-object payment-method-card-present-wallet ()
  (type
   :reader wallet-type
   :type string
   :documentation "The type of mobile wallet, one of `apple_pay`,
`google_pay`, `samsung_pay`, or `unknown`."))

(define-object payment-method-cashapp ()
  (buyer-id
   :type (or string null)
   :documentation "A unique and immutable identifier assigned by Cash
App to every buyer.")
  (cashtag
   :type (or string null)
   :documentation "A public identifier for buyers using Cash App."))

(define-object payment-method-customer-balance ())

(define-object payment-method-eps ()
  (bank
   :type (or string null)
   :documentation "The customer's bank. Should be one of
`arzte_und_apotheker_bank`, `austrian_anadi_bank_ag`, `bank_austria`,
`bankhaus_carl_spangler`, `bankhaus_schelhammer_und_schattera_ag`,
`bawag_psk_ag`, `bks_bank_ag`, `brull_kallmus_bank_ag`,
`btv_vier_lander_bank`, `capital_bank_grawe_gruppe_ag`,
`deutsche_bank_ag`, `dolomitenbank`, `easybank_ag`,
`erste_bank_und_sparkassen`, `hypo_alpeadriabank_international_ag`,
`hypo_noe_lb_fur_niederosterreich_u_wien`,
`hypo_oberosterreich_salzburg_steiermark`, `hypo_tirol_bank_ag`,
`hypo_vorarlberg_bank_ag`, `hypo_bank_burgenland_aktiengesellschaft`,
`marchfelder_bank`, `oberbank_ag`,
`raiffeisen_bankengruppe_osterreich`, `schoellerbank_ag`,
`sparda_bank_wien`, `volksbank_gruppe`, `volkskreditbank_ag`, or
`vr_bank_braunau`."))

(define-object payment-method-fpx ()
  (account-holder-type
   :type (or string null)
   :documentation "Account holder type, if provided. Can be one of
`individual` or `company`.")
  (bank
   :type string
   :documentation "The customer's bank, if provided. Can be one of
`affin_bank`, `agrobank`, `alliance_bank`, `ambank`, `bank_islam`,
`bank_muamalat`, `bank_rakyat`, `bsn`, `cimb`, `hong_leong_bank`,
`hsbc`, `kfh`, `maybank2u`, `ocbc`, `public_bank`, `rhb`,
`standard_chartered`, `uob`, `deutsche_bank`, `maybank2e`,
`pb_enterprise`, or `bank_of_china`."))

(define-object payment-method-giropay ())

(define-object payment-method-grabpay ())

(define-object payment-method-ideal ()
  (bank
   :type (or string null)
   :documentation "The customer's bank, if provided. Can be one of
`abn_amro`, `asn_bank`, `bunq`, `handelsbanken`, `ing`, `knab`,
`moneyou`, `n26`, `nn`, `rabobank`, `regiobank`, `revolut`, `sns_bank`,
`triodos_bank`, `van_lanschot`, or `yoursafe`.")
  (bic
   :type (or string null)
   :documentation "The Bank Identifier Code of the customer's bank, if
the bank was provided. One of `ABNANL2A`, `ASNBNL21`, `BITSNL2A`,
`BUNQNL2A`, `FVLBNL22`, `HANDNL2A`, `INGBNL2A`, `KNABNL2H`, `MOYONL21`,
`NNBANL2G`, `NTSBDEB1`, `RABONL2U`, `RBRBNL21`, `REVOIE23`, `REVOLT21`,
`SNSBNL2A`, or `TRIONL2U`."))

(define-object payment-method-interac-present ()
  (brand
   :type string
   :documentation "Card brand. Can be `interac`, `mastercard` or
`visa`.")
  (cardholder-name
   :type (or string null)
   :documentation "The cardholder name as read from the card, in
[ISO 7813](https://en.wikipedia.org/wiki/ISO/IEC_7813) format. May
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
  (exp-month
   :type integer
   :documentation "Two-digit number representing the card's expiration
month.")
  (exp-year
   :type integer
   :documentation "Four-digit number representing the card's expiration
year.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular card number. You
can use this attribute to check whether two customers who've signed up
with you are using the same card number, for example. For payment
methods that tokenize card information (Apple Pay, Google Pay), the
tokenized number might be provided instead of the underlying card
number.

*As of May 1, 2021, card fingerprint in India for Connect changed to
allow two fingerprints for the same card---one for India and one for
the rest of the world.*")
  (funding
   :type string
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
  (networks
   :type (or payment-method-card-interac-present-networks null)
   :documentation "Contains information about card networks that can
be used to process the payment.")
  (preferred-locales
   :type (or (vector string) null)
   :documentation "EMV tag 5F2D. Preferred languages specified by the
integrated circuit chip.")
  (read-method
   :type (or string null)
   :documentation "How card details were read in this transaction. One
of `contact_emv`, `contactless_emv` , `contactless_magstripe_mode`,
`magnetic_stripe_fallback`, or `magnetic_stripe_track2`."))

(define-object payment-method-interac-present-networks ()
  (available
   :type (vector string)
   :documentation "All available networks for the card.")
  (preferred
   :type (or string null)
   :documentation "The preferred network for the card."))

(define-object payment-method-kakao-pay ())

(define-object payment-method-klarna ()
  (dob
   :type (or payment-method-klarna-dob null)
   :documentation "The customer's date of birth, if provided."))

(define-object payment-method-klarna-dob ()
  (day
   :type (or integer null)
   :documentation "The day of birth, between 1 and 31.")
  (month
   :type (or integer null)
   :documentation "The month of birth, between 1 and 12.")
  (year
   :type (or integer null)
   :documentation "The four-digit year of birth."))

(define-object payment-method-konbini ())

(define-object payment-method-kr-card ()
  (brand
   :type (or string null)
   :documentation "The local credit or debit card brand. One of `bc`,
`citi`, `hana`, `hyundai`, `jeju`, `jeonbuk`, `kakaobank`, `kbank`,
`kdbbank`, `kookmin`, `kwangju`, `lotte`, `mg`, `nh`, `post`,
`samsung`, `savingsbank`, `shinhan`, `shinhyup`, `suhyup`, `tossbank`,
or `woori`.")
  (last4
   :type (or string null)
   :documentation "The last four digits of the card. This may not be
present for American Express cards."))

(define-object payment-method-link ()
  (email
   :type (or string null)
   :documentation "Account owner's email address."))

(define-object payment-method-mobilepay ())

(define-object payment-method-multibanco ())

(define-object payment-method-naver-pay ()
  (funding
   :type string
   :documentation "Whether to fund this transaction with Naver Pay
points or a card. One of `card` or `points`."))

(define-object payment-method-oxxo ())

(define-object payment-method-p24 ()
  (bank
   :type (or string null)
   :documentation "The customer's bank, if provided. One of
`alior_bank`, `bank_millennium`, `bank_nowy_bfg_sa`, `bank_pekao_sa`,
`banki_spbdzielcze`, `blik`, `bnp_paribas`, `boz`, `citi_handlowy`,
`credit_agricole`, `envelobank`, `etransfer_pocztowy24`, `getin_bank`,
`ideabank`, `ing`, `inteligo`, `mbank_mtransfer`, `nest_przelew`,
`noble_pay`, `pbac_z_ipko`, `plus_bank`, `santander_przelew24`,
`tmobile_usbugi_bankowe`, `toyota_bank`, `velobank`, or
`volkswagen_bank`."))

(define-object payment-method-payco ())

(define-object payment-method-paynow ())

(define-object payment-method-paypal ()
  (payer-email
   :type (or string null)
   :documentation "Owner's email. Values are provided by PayPal
directly (if supported) at the time of authorization or settlement.
They cannot be set or mutated.")
  (payer-id
   :type (or string null)
   :documentation "PayPal account PayerID. This identifier uniquely
identifies the PayPal customer."))

(define-object payment-method-pix ())

(define-object payment-method-promptpay ())

(define-object payment-method-radar-options ()
  (session
   :type (or string null)
   :documentation "A [Radar Session]
(https://stripe.com/docs/radar/radar-session) is a snapshot of the
browser metadata and device details that help Radar make more accurate
predictions on your payments."))

(define-object payment-method-revolut-pay ())

(define-object payment-method-samsung-pay ())

(define-object payment-method-sepa-debit ()
  (bank-code
   :type (or string null)
   :documentation "Bank code of bank associated with the bank account.")
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
  (generated-from
   :type (or payment-method-sepa-debit-generated-from null)
   :documentation "Information about the object that generated this
PaymentMethod.")
  (last4
   :type (or string null)
   :documentation "Last four characters of the IBAN."))

(define-object payment-method-sepa-debit-generated-from ()
  (charge
   :type (or string charge null)
   :documentation "The ID of the Charge that generated this
PaymentMethod, if any.")
  (setup-attempt
   ;; TODO: make this expandable
   :type (or string null)
   :documentation "The ID of the SetupAttempt that generated this
PaymentMethod, if any."))

(define-object payment-method-sofort ()
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the country the
bank account is located in."))

(define-object payment-method-swish ())

(define-object payment-method-twint ())

(define-object payment-method-us-bank-account ()
  (account-holder-type
   :type (or string null)
   :documentation "Account holder type: `individual` or `company`.")
  (account-type
   :type (or string null)
   :documentation "Account type: `checking` or `savings`. Defaults to
checking if omitted.")
  (bank-name
   :type (or string null)
   :documentation "The name of the bank.")
  (financial-connections-account
   :type (or string null)
   :documentation "The ID of the Financial Connections Account used to
create the payment method.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular bank account.
You can use this attribute to check whether two bank accounts are the
same.")
  (last4
   :type (or string null)
   :documentation "Last four digits of the bank account number.")
  (networks
   :type (or payment-method-us-bank-account-networks null)
   :documentation "Contains information about US bank account networks
that can be used.")
  (routing-number
   :type (or string null)
   :documentation "Routing number of the bank account.")
  (status-details
   :type (or payment-method-us-bank-account-status-details null)
   :documentation "Contains information about the future reusability of
this PaymentMethod."))

(define-object payment-method-us-bank-account-networks ()
  (preferred
   :type (or string null)
   :documentation "The preferred network.")
  (supported
   :type (vector string)
   :documentation "All supported networks. One of `ach` or
`us_domestic_wire`."))

(define-object payment-method-us-bank-account-status-details ()
  (blocked
   :type (or payment-method-us-bank-account-status-details-blocked null)))

(define-object payment-method-us-bank-account-status-details-blocked ()
  (network-code
   :type (or string null)
   :documentation "The ACH network code that resulted in this block.
One of `R02`, `R03`, `R04`, `R05`, `R07`, `R08`, `R10`, `R11`, `R16`,
`R20`, `R29`, or `R31`.")
  (reason
   :type (or string null)
   :documentation "The reason why this PaymentMethod's fingerprint has
been blocked. One of `bank_account_closed`, `bank_account_frozen`,
`bank_account_invalid_details`, `bank_account_restricted`,
`bank_account_unusable`, or `debit_not_authorized`."))

(define-object payment-method-wechat-pay ())

(define-object payment-method-zip ())
