(in-package #:stripe)

(define-object dispute ()
  "A dispute occurs when a customer questions your charge with their
card issuer. When this happens, you have the opportunity to respond to
the dispute with evidence that shows that the charge is legitimate.

Related guide: [Disputes and fraud](https://stripe.com/docs/disputes)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "dispute"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (amount
   :type integer
   :documentation "Disputed amount. Usually the amount of the charge,
but it can differ (usually because of currency fluctuation or because
only part of the order is disputed).")
  (balance-transactions
   :type (vector balance-transactions)
   :documentation "List of zero, one, or two balance transactions that
show funds withdrawn and reinstated to your Stripe account as a result
of this dispute.")
  (charge
   :type (or string charge)
   :documentation "ID of the charge that's disputed.")
  (created
   :type local-time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (currency
   :type string
   :documentation "Three-letter [ISO currency code]
(https://www.iso.org/iso-4217-currency-codes.html), in lowercase. Must
be a [supported currency](https://stripe.com/docs/currencies).")
  (enhanced-eligibility-types
   :type (vector string)
   :documentation "List of eligibility types that are included in
`enhanced_evidence`. One of `visa_compelling_evidence_3`.")
  (evidence
   :type dispute-evidence)
  (evidence-details
   :type dispute-evidence-details)
  (is-charge-refundable
   :type boolean
   :documentation "If true, it's still possible to refund the disputed
payment. After the payment has been fully refunded, no further funds
are withdrawn from your Stripe account as a result of this dispute.")
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
  (network-reason-code
   :type (or string null)
   :documentation "Network-dependent reason code for the dispute.")
  (payment-intent
   :type (or string payment-intent null)
   :documentation "ID of the PaymentIntent that's disputed.")
  (payment-method-details
   :type (or dispute-payment-method-details null))
  (reason
   :type string
   :documentation "Reason given by cardholder for dispute. Possible
values are `bank_cannot_process`, `check_returned`,
`credit_not_processed`, `customer_initiated`, `debit_not_authorized`,
`duplicate`, `fraudulent`, `general`, `incorrect_account_details`,
`insufficient_funds`, `product_not_received`, `product_unacceptable`,
`subscription_canceled`, or `unrecognized`. Learn more about
[dispute reasons](https://stripe.com/docs/disputes/categories).")
  (status
   :type string
   :documentation "Current status of dispute. Possible values are
`warning_needs_response`, `warning_under_review`, `warning_closed`,
`needs_response`, `under_review`, `won`, or `lost`."))

(defmethod initialize-instance :after ((instance dispute) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:balance-transactions
           (when value
             (setf (slot-value instance '%balance-transactions)
                   (map 'vector
                        (lambda (item)
                          (make-instance 'balance-transaction :data item))
                        value))))
          (:charge
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%charge)
                   (make-instance 'charge :data value))))
          (:evidence
           (when value
             (setf (slot-value instance '%evidence)
                   (make-instance 'dispute-evidence :data value))))
          (:evidence-details
           (when value
             (setf (slot-value instance '%evidence-details)
                   (make-instance 'dispute-evidence-details :data value))))
          (:payment-intent
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%payment-intent)
                   (make-instance 'payment-intent :data value))))
          (:payment-method-details
           (when value
             (setf (slot-value instance '%payment-method-details)
                   (make-instance 'dispute-payment-method-details :data value)))))))))

(define-object dispute-evidence ()
  (access-activity-log
   :type (or string null)
   :documentation "Any server or activity logs showing proof that the
customer accessed or downloaded the purchased digital product. This
information should include IP addresses, corresponding timestamps, and
any detailed recorded activity.")
  (billing-address
   :type (or string null)
   :documentation "The billing address provided by the customer.")
  (cancellation-policy
   :type (or string file null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Your subscription
cancellation policy, as shown to the customer.")
  (cancellation-policy-disclosure
   :type (or string null)
   :documentation "An explanation of how and when the customer was
shown your refund policy prior to purchase.")
  (cancellation-rebuttal
   :type (or string null)
   :documentation "A justification for why the customer's subscription
was not canceled.")
  (customer-communication
   :type (or string file null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Any communication with
the customer that you feel is relevant to your case. Examples include
emails proving that the customer received the product or service, or
demonstrating their use of or satisfaction with the product or service.")
  (customer-email-address
   :type (or string null)
   :documentation "The email address of the customer.")
  (customer-name
   :type (or string null)
   :documentation "The name of the customer.")
  (customer-purchase-ip
   :type (or string null)
   :documentation "The IP address that the customer used when making
the purchase.")
  (customer-signature
   :type (or string file null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) A relevant document or
contract showing the customer's signature.")
  (duplicate-charge-documentation
   :type (or string file null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Documentation for the
prior charge that can uniquely identify the charge, such as a receipt,
shipping label, work order, etc. This document should be paired with a
similar document from the disputed payment that proves the two payments
are separate.")
  (duplicate-charge-explanation
   :type (or string null)
   :documentation "An explanation of the difference between the
disputed charge versus the prior charge that appears to be a duplicate.")
  (duplicate-charge-id
   :type (or string null)
   :documentation "The Stripe ID for the prior charge which appears to
be a duplicate of the disputed charge.")
  (enhanced-evidence
   :type dispute-enhanced-evidence
   :documentation "Enhanced evidence details.")
  (product-description
   :type (or string null)
   :documentation "A description of the product or service that was
sold.")
  (receipt
   :type (or string file null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Any receipt or message
sent to the customer notifying them of the charge.")
  (refund-policy
   :type (or string file null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Your refund policy, as
shown to the customer.")
  (refund-policy-disclosure
   :type (or string null)
   :documentation "Documentation demonstrating that the customer was
shown your refund policy prior to purchase.")
  (refund-refusal-explanation
   :type (or string null)
   :documentation "A justification for why the customer is not entitled
to a refund.")
  (service-date
   :type (or string null)
   :documentation "The date on which the customer received or began
receiving the purchased service, in a clear human-readable format.")
  (service-documentation
   :type (or string file null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Documentation showing
proof that a service was provided to the customer. This could include
a copy of a signed contract, work order, or other form of written
agreement.")
  (shipping-address
   :type (or string null)
   :documentation "The address to which a physical product was shipped.
You should try to include as complete address information as possible.")
  (shipping-carrier
   :type (or string null)
   :documentation "The delivery service that shipped a physical
product, such as Fedex, UPS, USPS, etc. If multiple carriers were used
for this purchase, please separate them with commas.")
  (shipping-date
   :type (or string null)
   :documentation "The date on which a physical product began its route
to the shipping address, in a clear human-readable format.")
  (shipping-documentation
   :type (or string file null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Documentation showing
proof that a product was shipped to the customer at the same address
the customer provided to you. This could include a copy of the shipment
receipt, shipping label, etc. It should show the customer's full
shipping address, if possible.")
  (shipping-tracking-number
   :type (or string null)
   :documentation "The tracking number for a physical product, obtained
from the delivery service. If multiple tracking numbers were generated
for this purchase, please separate them with commas.")
  (uncategorized-file
   :type (or string file null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Any additional evidence
or statements.")
  (uncategorized-text
   :type (or string null)
   :documentation "Any additional evidence or statements."))

(defmethod initialize-instance :after ((instance dispute-evidence) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:cancellation-policy
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%cancellation-policy)
                   (make-instance 'file :data value))))
          (:customer-communication
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%customer-communication)
                   (make-instance 'file :data value))))
          (:customer-signature
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%customer-signature)
                   (make-instance 'file :data value))))
          (:duplicate-charge-documentation
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%duplicate-charge-documentation)
                   (make-instance 'file :data value))))
          (:enhanced-evidence
           (when value
             (setf (slot-value instance '%enhanced-evidence)
                   (make-instance 'dispute-enhanced-evidence :data value))))
          (:receipt
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%receipt)
                   (make-instance 'file :data value))))
          (:refund-policy
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%refund-policy)
                   (make-instance 'file :data value))))
          (:service-documentation
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%service-documentation)
                   (make-instance 'file :data value))))
          (:shipping-documentation
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%shipping-documentation)
                   (make-instance 'file :data value))))
          (:uncategorized-file
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%uncategorized-file)
                   (make-instance 'file :data value)))))))))

(define-object dispute-enhanced-evidence ()
  (visa-compelling-evidence-3
   :type (or dispute-enhanced-evidence-visa-compelling-evidence-3 null)))

(defmethod initialize-instance :after ((instance dispute-enhanced-evidence)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:visa-compelling-evidence-3
           (when value
             (setf (slot-value instance '%visa-compelling-evidence-3)
                   (make-instance 'dispute-enhanced-evidence-visa-compelling-evidence-3
                                  :data value)))))))))

(define-object dispute-enhanced-evidence-visa-compelling-evidence-3 ()
  (disputed-transaction
   :type (or dispute-enhanced-evidence-visa-compelling-evidence-3-disputed-transaction null)
   :documentation "Disputed transaction details for Visa Compelling
Evidence 3.0 evidence submission.")
  (prior-undisputed-transactions
   :type (vector dispute-enhanced-evidence-visa-compelling-evidence-3-prior-undisputed-transaction)
   :documentation "List of exactly two prior undisputed transaction
objects for Visa Compelling Evidence 3.0 evidence submission."))

(defmethod initialize-instance :after ((instance
                                        dispute-enhanced-evidence-visa-compelling-evidence-3)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:disputed-transaction
           (when value
             (setf (slot-value instance '%disputed-transaction)
                   (make-instance
                    'dispute-enhanced-evidence-visa-compelling-evidence-3-disputed-transaction
                    :data value))))
          (:prior-undisputed-transactions
           (when value
             (setf (slot-value instance '%prior-undisputed-transactions)
                   (map 'vector
                        (lambda (item)
                          (make-instance
                           'dispute-enhanced-evidence-visa-compelling-evidence-3-prior-undisputed-transaction
                           :data item))
                        value)))))))))

(define-object dispute-enhanced-evidence-visa-compelling-evidence-3-disputed-transaction ()
  (customer-account-id
   :type (or string null)
   :documentation "User Account ID used to log into business platform.
Must be recognizable by the user.")
  (customer-device-fingerprint
   :type (or string null)
   :documentation "Unique identifier of the cardholder's device derived
from a combination of at least two hardware and software attributes.
Must be at least 20 characters.")
  (customer-device-id
   :type (or string null)
   :documentation "Unique identifier of the cardholder's device such as
a device serial number (e.g., International Mobile Equipment Identity
[IMEI]). Must be at least 15 characters.")
  (customer-email-address
   :type (or string null)
   :documentation "The email address of the customer.")
  (customer-purchase-ip
   :type (or string null)
   :documentation "The IP address that the customer used when making
the purchase.")
  (merchandise-or-services
   :type (or string null)
   :documentation "Categorization of disputed payment. One of
`merchandise` or `services`.")
  (product-description
   :type (or string null)
   :documentation "A description of the product or service that was
sold.")
  (shipping-address
   :type (or address null)
   :documentation "The address to which a physical product was shipped.
All fields are required for Visa Compelling Evidence 3.0 evidence
submission."))

(defmethod initialize-instance :after
    ((instance dispute-enhanced-evidence-visa-compelling-evidence-3-disputed-transaction)
     &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:shipping-address
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping-address)
                   (make-instance 'address :data value)))))))))

(define-object dispute-enhanced-evidence-visa-compelling-evidence-3-prior-undisputed-transaction ()
  (charge
   :type string
   :documentation "Stripe charge ID for the Visa Compelling Evidence
3.0 eligible prior charge.")
  (customer-account-id
   :type (or string null)
   :documentation "User Account ID used to log into business platform.
Must be recognizable by the user.")
  (customer-device-fingerprint
   :type (or string null)
   :documentation "Unique identifier of the cardholder's device derived
from a combination of at least two hardware and software attributes.
Must be at least 20 characters.")
  (customer-device-id
   :type (or string null)
   :documentation "Unique identifier of the cardholder's device such as
a device serial number (e.g., International Mobile Equipment Identity
[IMEI]). Must be at least 15 characters.")
  (customer-email-address
   :type (or string null)
   :documentation "The email address of the customer.")
  (customer-purchase-ip
   :type (or string null)
   :documentation "The IP address that the customer used when making
the purchase.")
  (product-description
   :type (or string null)
   :documentation "A description of the product or service that was
sold.")
  (shipping-address
   :type (or address null)
   :documentation "The address to which a physical product was shipped.
All fields are required for Visa Compelling Evidence 3.0 evidence
submission."))

(defmethod initialize-instance :after
    ((instance dispute-enhanced-evidence-visa-compelling-evidence-3-prior-undisputed-transaction)
     &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:shipping-address
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping-address)
                   (make-instance 'address :data value)))))))))

(define-object dispute-evidence-details ()
  (due-by
   :type (or local-time:timestamp null)
   :documentation "Date by which evidence must be submitted in order to
successfully challenge dispute. Will be 0 if the customer's bank or
credit card company doesn't allow a response for this particular
dispute.")
  (enhanced-eligibility
   :type dispute-enhanced-eligibility)
  (has-evidence
   :type boolean
   :documentation "Whether evidence has been staged for this dispute.")
  (past-due
   :type boolean
   :documentation "Whether the last evidence submission was submitted
past the due date. Defaults to `false` if no evidence submissions have
occurred. If `true`, then delivery of the latest evidence is *not*
guaranteed.")
  (submission-count
   :type integer
   :documentation "The number of times evidence has been submitted.
Typically, you may only submit evidence once."))

(defmethod initialize-instance :after ((instance dispute-evidence-details)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:due-by
           (unless (eql 'null value)
             (setf (slot-value instance '%due-by) (decode-timestamp value))))
          (:enhanced-eligibility
           (when value
             (setf (slot-value instance '%enhanced-eligibility)
                   (make-instance 'dispute-enhanced-eligibility :data value)))))))))

(define-object dispute-enhanced-eligibility ()
  (visa-compelling-evidence-3
   :type (or dispute-enhanced-eligibility-visa-compelling-evidence-3 null)))

(defmethod initialize-instance :after ((instance dispute-enhanced-eligibility)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:visa-compelling-evidence-3
           (when value
             (setf (slot-value instance '%visa-compelling-evidence-3)
                   (make-instance 'dispute-enhanced-eligibility-visa-compelling-evidence-3
                                  :data value)))))))))

(define-object dispute-enhanced-eligibility-visa-compelling-evidence-3 ()
  (required-actions
   :type (vector string)
   :documentation "List of actions required to qualify dispute for Visa
Compelling Evidence 3.0 evidence submission. One of
`missing_customer_identifiers`,
`missing_disputed_transaction_description`,
`missing_merchandise_or_services`,
`missing_prior_undisputed_transaction_description`,
or `missing_prior_undisputed_transactions`.")
  (status
   :type string
   :documentation "Visa Compelling Evidence 3.0 eligibility status. One
of `not_qualified`, `qualified`, or `requires_action`."))

(define-object dispute-payment-method-details ()
  (amazon-pay
   :type (or dispute-payment-method-details-amazon-pay null))
  (card
   :type (or dispute-payment-method-details-card null))
  (klarna
   :type (or dispute-payment-method-details-klarna null))
  (paypal
   :type (or dispute-payment-method-details-paypal null))
  (type
   :reader dispute-payment-method-type
   :type string
   :documentation "Payment method type. One of `amazon_pay`, `card`,
`klarna`, or `paypal`."))

(defmethod initialize-instance :after ((instance dispute-payment-method-details)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:amazon-pay
           (when value
             (setf (slot-value instance '%amazon-pay)
                   (make-instance 'dispute-payment-method-details-amazon-pay :data value))))
          (:card
           (when value
             (setf (slot-value instance '%card)
                   (make-instance 'dispute-payment-method-details-card :data value))))
          (:klarna
           (when value
             (setf (slot-value instance '%klarna)
                   (make-instance 'dispute-payment-method-details-klarna :data value))))
          (:paypal
           (when value
             (setf (slot-value instance '%paypal)
                   (make-instance 'dispute-payment-method-details-paypal :data value)))))))))

(define-object dispute-payment-method-details-amazon-pay ()
  (dispute-type
   :type (or string null)
   :documentation "The AmazonPay dispute type, `chargeback` or `claim`."))

(define-object dispute-payment-method-details-card ()
  (brand
   :type string
   :documentation "Card brand. Can be `amex`, `diners`, `discover`,
`eftpos_au`, `jcb`, `mastercard`, `unionpay`, `visa`, or `unknown`.")
  (case-type
   :type string
   :documentation "The type of dispute opened. Different case types may
have varying fees and financial impact. One of `chargeback` or `inquiry`.")
  (network-reason-code
   :type (or string null)
   :documentation "The card network's specific dispute reason code,
which maps to one of Stripe's primary dispute categories to simplify
response guidance. The [Network code map]
(https://stripe.com/docs/disputes/categories#network-code-map) lists
all available dispute reason codes by network."))

(define-object dispute-payment-method-details-klarna ()
  (reason-code
   :type (or string null)
   :documentation "The reason for the dispute as defined by Klarna."))

(define-object dispute-payment-method-details-paypal ()
  (case-id
   :type (or string null)
   :documentation "The ID of the dispute in PayPal.")
  (reason-code
   :type (or string null)
   :documentation "The reason for the dispute as defined by PayPal."))

(define-object dispute-update-evidence ()
  (access-activity-log
   :type (or string null)
   :documentation "Any server or activity logs showing proof that the
customer accessed or downloaded the purchased digital product. This
information should include IP addresses, corresponding timestamps, and
any detailed recorded activity. Has a maximum character count of
20,000.")
  (billing-address
   :type (or string null)
   :documentation "The billing address provided by the customer.")
  (cancellation-policy
   :type (or string null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Your subscription
cancellation policy, as shown to the customer.")
  (cancellation-policy-disclosure
   :type (or string null)
   :documentation "An explanation of how and when the customer was
shown your refund policy prior to purchase. Has a maximum character
count of 20,000.")
  (cancellation-rebuttal
   :type (or string null)
   :documentation "A justification for why the customer’s subscription
was not canceled. Has a maximum character count of 20,000.")
  (customer-communication
   :type (or string null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Any communication with
the customer that you feel is relevant to your case.")
  (customer-email-address
   :type (or string null)
   :documentation "The email address of the customer.")
  (customer-name
   :type (or string null)
   :documentation "The name of the customer.")
  (customer-purchase-ip
   :type (or string null)
   :documentation "The IP address that the customer used when making
the purchase.")
  (customer-signature
   :type (or string null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) A relevant document or
contract showing the customer's signature.")
  (duplicate-charge-documentation
   :type (or string null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Documentation for the
prior charge that can uniquely identify the charge.")
  (duplicate-charge-explanation
   :type (or string null)
   :documentation "An explanation of the difference between the
disputed charge versus the prior charge that appears to be a duplicate.
Has a maximum character count of 20,000.")
  (duplicate-charge-id
   :type (or string null)
   :documentation "The Stripe ID for the prior charge which appears to
be a duplicate of the disputed charge.")
  (enhanced-evidence
   :type dispute-enhanced-evidence)
  (product-description
   :type (or string null)
   :documentation "A description of the product or service that was
sold. Has a maximum character count of 20,000.")
  (receipt
   :type (or string null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Any receipt or message
sent to the customer notifying them of the charge.")
  (refund-policy
   :type (or string null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Your refund policy, as
shown to the customer.")
  (refund-policy-disclosure
   :type (or string null)
   :documentation "Documentation demonstrating that the customer was
shown your refund policy prior to purchase. Has a maximum character
count of 20,000.")
  (refund-refusal-explanation
   :type (or string null)
   :documentation "A justification for why the customer is not entitled
to a refund. Has a maximum character count of 20,000.")
  (service-date
   :type (or string null)
   :documentation "The date on which the customer received or began
receiving the purchased service, in a clear human-readable format.")
  (service-documentation
   :type (or string null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Documentation showing
proof that a service was provided to the customer. This could include
a copy of a signed contract, work order, or other form of written
agreement.")
  (shipping-address
   :type (or string null)
   :documentation "The address to which a physical product was shipped.
You should try to include as complete address information as possible.")
  (shipping-carrier
   :type (or string null)
   :documentation "The delivery service that shipped a physical
product, such as Fedex, UPS, USPS, etc. If multiple carriers were used
for this purchase, please separate them with commas.")
  (shipping-date
   :type (or string null)
   :documentation "The date on which a physical product began its route
to the shipping address, in a clear human-readable format.")
  (shipping-documentation
   :type (or string null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Documentation showing
proof that a product was shipped to the customer at the same address
the customer provided to you. This could include a copy of the shipment
receipt, shipping label, etc. It should show the customer’s full
shipping address, if possible.")
  (shipping-tracking-number
   :type (or string null)
   :documentation "The tracking number for a physical product, obtained
from the delivery service. If multiple tracking numbers were generated
for this purchase, please separate them with commas.")
  (uncategorized-file
   :type (or string null)
   :documentation "(ID of a [file upload]
(https://stripe.com/docs/guides/file-upload)) Any additional evidence
or statements.")
  (uncategorized-text
   :type (or string null)
   :documentation "Any additional evidence or statements. Has a maximum
character count of 20,000."))
