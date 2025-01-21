(in-package #:stripe)

(define-object mandate ()
  "Mandate objects represent a customer's acceptance of a recurring
payment agreement."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "mandate"
   :documentation "String representing the object's type.")
  (customer-acceptance
   :type mandate-customer-acceptance)
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (multi-use
   :type (or mandate-multi-use null))
  (on-behalf-of
   :type (or string null)
   :documentation "The account (if any) that the mandate is intended
for.")
  (payment-method
   :type (or string payment-method)
   :documentation "ID of the payment method associated with this
mandate.")
  (payment-method-details
   :type mandate-payment-method-details)
  (single-use
   :type (or mandate-single-use null))
  (status
   :type string
   :documentation "The mandate status (e.g., 'active', 'inactive').")
  (type
   :reader mandate-type
   :type string
   :documentation "The type of the mandate (e.g., 'multi_use' or
'single_use').")
  (:list-type t))

(define-object mandate-customer-acceptance ()
  (accepted-at
   :type (or time:timestamp null)
   :documentation "The time that the customer accepts the mandate.")
  (offline
   :type (or mandate-customer-acceptance-offline null))
  (online
   :type (or mandate-customer-acceptance-online null))
  (type
   :reader customer-acceptance-type
   :type string
   :documentation "The mandate includes the type of customer acceptance
information, such as: `online` or `offline`."))

(define-object mandate-customer-acceptance-offline ())

(define-object mandate-customer-acceptance-online ()
  (ip-address
   :type (or string null)
   :documentation "The customer accepts the mandate from this IP
address.")
  (user-agent
   :type (or string null)
   :documentation "The customer accepts the mandate using the user
agent of the browser."))

(define-object mandate-payment-method-details ()
  (acss-debit
   :type (or mandate-payment-method-details-acss-debit null))
  (amazon-pay
   :type (or mandate-payment-method-details-amazon-pay null))
  (au-becs-debit
   :type (or mandate-payment-method-details-au-becs-debit null))
  (bacs-debit
   :type (or mandate-payment-method-details-bacs-debit null))
  (card
   :type (or mandate-payment-method-details-card null))
  (cashapp
   :type (or mandate-payment-method-details-cashapp null))
  (kakao-pay
   :type (or mandate-payment-method-details-kakao-pay null))
  (kr-card
   :type (or mandate-payment-method-details-kr-card null))
  (link
   :type (or mandate-payment-method-details-link null))
  (paypal
   :type (or mandate-payment-method-details-paypal null))
  (revolut-pay
   :type (or mandate-payment-method-details-revolut-pay null))
  (sepa-debit
   :type (or mandate-payment-method-details-sepa-debit null))
  (type
   :reader payment-method-details-type
   :type (or mandate-payment-method-details-kr-card null)
   :documentation "This mandate corresponds with a specific payment
method type. The `payment_method_details` includes an additional hash
with the same name and contains mandate information that's specific to
that payment method.")
  (us-bank-account
   :type (or mandate-payment-method-details-us-bank-account null)))

(define-object mandate-payment-method-details-acss-debit ()
  (default-for
   :type (or (vector string) null)
   :documentation "List of Stripe products where this mandate can be
selected automatically. One of `invoice` or `subscription`.")
  (interval-description
   :type (or string null)
   :documentation "Description of the interval. Only required if the
'payment_schedule' parameter is 'interval' or 'combined'.")
  (payment-schedule
   :type string
   :documentation "Payment schedule for the mandate. One of `combined`,
`interval`, `sporadic`.")
  (transaction-type
   :type string
   :documentation "Transaction type of the mandate. One of `business`
or `personal`."))

(define-object mandate-payment-method-details-amazon-pay ())

(define-object mandate-payment-method-details-au-becs-debit ()
  (url
   :type string
   :documentation "The URL of the mandate. This URL generally contains
sensitive information about the customer and should be shared with them
exclusively."))

(define-object mandate-payment-method-details-bacs-debit ()
  (network-status
   :type string
   :documentation "The status of the mandate on the Bacs network. Can
be one of `pending`, `revoked`, `refused`, or `accepted`.")
  (reference
   :type string
   :documentation "The unique reference identifying the mandate on the
Bacs network.")
  (revocation-reason
   :type (or string null)
   :documentation "When the mandate is revoked on the Bacs network this
field displays the reason for the revocation. One of `account_closed`,
`bank_account_restricted`, `bank_ownership_changed`,
`could_not_process`, `debit_not_authorized`.")
  (url
   :type string
   :documentation "The URL that will contain the mandate that the
customer has signed."))

(define-object mandate-payment-method-details-card ())

(define-object mandate-payment-method-details-cashapp ())

(define-object mandate-payment-method-details-kakao-pay ())

(define-object mandate-payment-method-details-kr-card ())

(define-object mandate-payment-method-details-link ())

(define-object mandate-payment-method-details-paypal ()
  (billing-agreement-id
   :type (or string null)
   :documentation "The PayPal Billing Agreement ID (BAID). This is an
ID generated by PayPal which represents the mandate between the
merchant and the customer.")
  (payer-id
   :type (or string null)
   :documentation "PayPal account PayerID. This identifier uniquely
identifies the PayPal customer."))

(define-object mandate-payment-method-details-revolut-pay ())

(define-object mandate-payment-method-details-sepa-debit ()
  (reference
   :type string
   :documentation "The unique reference of the mandate.")
  (url
   :type string
   :documentation "The URL of the mandate. This URL generally contains
sensitive information about the customer and should be shared with them
exclusively."))

(define-object mandate-payment-method-details-us-bank-account ()
  (collection-method
   :type (or string null)
   :initform "paper"
   :documentation "Mandate collection method"))

(define-object mandate-multi-use ())

(define-object mandate-single-use ()
  (amount
   :type integer
   :documentation "The amount of the payment on a single use mandate.")
  (currency
   :type string
   :documentation "The currency of the payment on a single use mandate."))

(defmethod initialize-instance :after ((instance mandate) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:customer-acceptance
           (unless (eql 'null value)
             (setf (slot-value instance '%customer-acceptance)
                   (make-instance 'mandate-customer-acceptance :data value))))
          (:payment-method
           (unless (eql 'null value)
             (setf (slot-value instance '%payment-method)
                   (make-instance 'payment-method :data value))))
          (:payment-method-details
           (unless (eql 'null value)
             (setf (slot-value instance '%payment-method-details)
                   (make-instance 'mandate-payment-method-details :data value))))
          (:multi-use
           (unless (eql 'null value)
             (setf (slot-value instance '%multi-use)
                   (make-instance 'mandate-multi-use :data value))))
          (:single-use
           (unless (eql 'null value)
             (setf (slot-value instance '%single-use)
                   (make-instance 'mandate-single-use :data value)))))))))

(defmethod initialize-instance :after ((instance mandate-customer-acceptance) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:accepted-at
           (setf (slot-value instance '%accepted-at)
                 (decode-timestamp value)))
          (:offline
           (unless (eql 'null value)
             (setf (slot-value instance '%offline)
                   (make-instance 'mandate-customer-acceptance-offline :data value))))
          (:online
           (unless (eql 'null value)
             (setf (slot-value instance '%online)
                   (make-instance 'mandate-customer-acceptance-online :data value)))))))))

(defmethod initialize-instance :after ((instance mandate-payment-method-details)
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
                   (make-instance 'mandate-payment-method-details-acss-debit
                                  :data value))))
          (:amazon-pay
           (unless (eql 'null value)
             (setf (slot-value instance '%amazon-pay)
                   (make-instance 'mandate-payment-method-details-amazon-pay
                                  :data value))))
          (:au-becs-debit
           (unless (eql 'null value)
             (setf (slot-value instance '%au-becs-debit)
                   (make-instance 'mandate-payment-method-details-au-becs-debit
                                  :data value))))
          (:bacs-debit
           (unless (eql 'null value)
             (setf (slot-value instance '%bacs-debit)
                   (make-instance 'mandate-payment-method-details-bacs-debit
                                  :data value))))
          (:card
           (unless (eql 'null value)
             (setf (slot-value instance '%card)
                   (make-instance 'mandate-payment-method-details-card
                                  :data value))))
          (:cashapp
           (unless (eql 'null value)
             (setf (slot-value instance '%cashapp)
                   (make-instance 'mandate-payment-method-details-cashapp
                                  :data value))))
          (:kakao-pay
           (unless (eql 'null value)
             (setf (slot-value instance '%kakao-pay)
                   (make-instance 'mandate-payment-method-details-kakao-pay
                                  :data value))))
          (:kr-card
           (unless (eql 'null value)
             (setf (slot-value instance '%kr-card)
                   (make-instance 'mandate-payment-method-details-kr-card
                                  :data value))))
          (:link
           (unless (eql 'null value)
             (setf (slot-value instance '%link)
                   (make-instance 'mandate-payment-method-details-link
                                  :data value))))
          (:paypal
           (unless (eql 'null value)
             (setf (slot-value instance '%paypal)
                   (make-instance 'mandate-payment-method-details-paypal
                                  :data value))))
          (:revolut-pay
           (unless (eql 'null value)
             (setf (slot-value instance '%revolut-pay)
                   (make-instance 'mandate-payment-method-details-revolut-pay
                                  :data value))))
          (:sepa-debit
           (unless (eql 'null value)
             (setf (slot-value instance '%sepa-debit)
                   (make-instance 'mandate-payment-method-details-sepa-debit
                                  :data value))))
          (:us-bank-account
           (unless (eql 'null value)
             (setf (slot-value instance '%us-bank-account)
                   (make-instance 'mandate-payment-method-details-us-bank-account
                                  :data value)))))))))
