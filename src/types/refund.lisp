(in-package #:stripe)

(define-object refund ()
  "Refund objects allow you to refund a previously created charge that
isn't refunded yet. Funds are refunded to the credit or debit card
that's initially charged.

Related guide: [Refunds](https://stripe.com/docs/refunds)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "refund"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (amount
   :type integer
   :documentation "Amount, in cents (or local equivalent).")
  (balance-transaction
   :type (or string balance-transaction null)
   :documentation "Balance transaction that describes the impact on
your account balance.")
  (charge
   :type (or string charge null)
   :documentation "ID of the charge that's refunded.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (currency
   :type string
   :documentation "Three-letter [ISO currency code]
(https://www.iso.org/iso-4217-currency-codes.html), in lowercase. Must
be a [supported currency](https://stripe.com/docs/currencies).")
  (description
   :type (or string null)
   :documentation "An arbitrary string attached to the object. You can
use this for displaying to users (available on non-card refunds only).")
  (destination-details
   :type (or refund-destination-details null)
   :documentation "Destination details for the refund.")
  (failure-balance-transaction
   :type (or string balance-transaction null)
   :documentation "After the refund fails, this balance transaction
describes the adjustment made on your account balance that reverses the
initial balance transaction.")
  (failure-reason
   :type (or string null)
   :documentation "Provides the reason for the refund failure. Possible
values are: `lost_or_stolen_card`, `expired_or_canceled_card`,
`charge_for_pending_refund_disputed`, `insufficient_funds`, `declined`,
`merchant_request`, or `unknown`.")
  (instructions-email
   :type (or string null)
   :documentation "For payment methods without native refund support
(for example, Konbini, PromptPay), provide an email address for the
customer to receive refund instructions.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (next-action
   :type (or refund-next-action null)
   :documentation "Next action to be taken, if any.")
  (payment-intent
   :type (or string payment-intent null)
   :documentation "ID of the PaymentIntent that's refunded.")
  (reason
   :type (or string null)
   :documentation "Reason for the refund, which is either user-provided
(`duplicate`, `fraudulent`, or `requested_by_customer`) or generated by
Stripe internally (`expired_uncaptured_charge`).")
  (receipt-number
   :type (or string null)
   :documentation "This is the transaction number that appears on email
receipts sent for this refund.")
  (source-transfer-reversal
   :type (or string transfer-reversal null)
   :documentation "The transfer reversal that's associated with the
refund. Only present if the charge came from another Stripe account.")
  (status
   :type (or string null)
   :documentation "Status of the refund. This can be `pending`,
`requires_action`, `succeeded`, `failed`, or `canceled`. Learn more
about [failed refunds](https://stripe.com/docs/refunds#failed-refunds).")
  (transfer-reversal
   :type (or string transfer-reversal null)
   :documentation "This refers to the transfer reversal object if the
accompanying transfer reverses. This is only applicable if the charge
was created using the destination parameter."))

(defmethod initialize-instance :after ((instance refund) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:balance-transaction
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%balance-transaction)
                   (make-instance 'balance-transaction :data value))))
          (:charge
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%charge)
                   (make-instance 'charge :data value))))
          (:destination-details
           (unless (eql 'null value)
             (setf (slot-value instance '%destination-details)
                   (make-instance 'refund-destination-details :data value))))
          (:failure-balance-transaction
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%failure-balance-transaction)
                   (make-instance 'balance-transaction :data value))))
          (:next-action
           (unless (eql 'null value)
             (setf (slot-value instance '%next-action)
                   (make-instance 'refund-next-action :data value))))
          (:payment-intent
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%payment-intent)
                   (make-instance 'payment-intent :data value))))
          (:source-transfer-reversal
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%source-transfer-reversal)
                   (make-instance 'transfer-reversal :data value))))
          (:transfer-reversal
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%transfer-reversal)
                   (make-instance 'transfer-reversal :data value)))))))))

(define-object refund-destination-details ()
  (affirm
   :type (or refund-destination-details-affirm null))
  (afterpay-clearpay
   :type (or refund-destination-details-afterpay-clearpay null))
  (alipay
   :type (or refund-destination-details-alipay null))
  (alma
   :type (or refund-destination-details-alma null))
  (amazon-pay
   :type (or refund-destination-details-amazon-pay null))
  (au-bank-transfer
   :type (or refund-destination-details-au-bank-transfer null))
  (blik
   :type (or refund-destination-details-blik null))
  (br-bank-transfer
   :type (or refund-destination-details-br-bank-transfer null))
  (card
   :type (or refund-destination-details-card null))
  (cashapp
   :type (or refund-destination-details-cashapp null))
  (customer-cash-balance
   :type (or refund-destination-details-customer-cash-balance null))
  (eps
   :type (or refund-destination-details-eps null))
  (eu-bank-transfer
   :type (or refund-destination-details-eu-bank-transfer null))
  (gb-bank-transfer
   :type (or refund-destination-details-gb-bank-transfer null))
  (giropay
   :type (or refund-destination-details-giropay null))
  (grabpay
   :type (or refund-destination-details-grabpay null))
  (jp-bank-transfer
   :type (or refund-destination-details-jp-bank-transfer null))
  (klarna
   :type (or refund-destination-details-klarna null))
  (multibanco
   :type (or refund-destination-details-multibanco null))
  (mx-bank-transfer
   :type (or refund-destination-details-mx-bank-transfer null))
  (p24
   :type (or refund-destination-details-p24 null))
  (paynow
   :type (or refund-destination-details-paynow null))
  (paypal
   :type (or refund-destination-details-paypal null))
  (pix
   :type (or refund-destination-details-pix null))
  (revolut
   :type (or refund-destination-details-revolut null))
  (sofort
   :type (or refund-destination-details-sofort null))
  (swish
   :type (or refund-destination-details-swish null))
  (th-bank-transfer
   :type (or refund-destination-details-th-bank-transfer null))
  (type
   :reader refund-destination-details-type
   :type string
   :documentation "The type of transaction-specific details of the
payment method used in the refund (e.g., `card`). An additional hash
is included on `destination_details` with a name matching this value.
It contains information specific to the refund transaction.")
  (us-bank-transfer
   :type (or refund-destination-details-us-bank-transfer null))
  (wechat-pay
   :type (or refund-destination-details-wechat-pay null))
  (zip
   :type (or refund-destination-details-zip null)))

(defmethod initialize-instance :after ((instance refund-destination-details)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:affirm
           (unless (eql 'null value)
             (setf (slot-value instance '%affirm)
                   (make-instance 'refund-destination-details-affirm :data value))))
          (:afterpay-clearpay
           (unless (eql 'null value)
             (setf (slot-value instance '%afterpay-clearpay)
                   (make-instance 'refund-destination-details-afterpay-clearpay :data value))))
          (:alipay
           (unless (eql 'null value)
             (setf (slot-value instance '%alipay)
                   (make-instance 'refund-destination-details-alipay :data value))))
          (:alma
           (unless (eql 'null value)
             (setf (slot-value instance '%alma)
                   (make-instance 'refund-destination-details-alma :data value))))
          (:amazon-pay
           (unless (eql 'null value)
             (setf (slot-value instance '%amazon-pay)
                   (make-instance 'refund-destination-details-amazon-pay :data value))))
          (:au-bank-transfer
           (unless (eql 'null value)
             (setf (slot-value instance '%au-bank-transfer)
                   (make-instance 'refund-destination-details-au-bank-transfer :data value))))
          (:blik
           (unless (eql 'null value)
             (setf (slot-value instance '%blik)
                   (make-instance 'refund-destination-details-blik :data value))))
          (:br-bank-transfer
           (unless (eql 'null value)
             (setf (slot-value instance '%br-bank-transfer)
                   (make-instance 'refund-destination-details-br-bank-transfer :data value))))
          (:card
           (unless (eql 'null value)
             (setf (slot-value instance '%card)
                   (make-instance 'refund-destination-details-card :data value))))
          (:cashapp
           (unless (eql 'null value)
             (setf (slot-value instance '%cashapp)
                   (make-instance 'refund-destination-details-cashapp :data value))))
          (:customer-cash-balance
           (unless (eql 'null value)
             (setf (slot-value instance '%customer-cash-balance)
                   (make-instance 'refund-destination-details-customer-cash-balance :data value))))
          (:eps
           (unless (eql 'null value)
             (setf (slot-value instance '%eps)
                   (make-instance 'refund-destination-details-eps :data value))))
          (:eu-bank-transfer
           (unless (eql 'null value)
             (setf (slot-value instance '%eu-bank-transfer)
                   (make-instance 'refund-destination-details-eu-bank-transfer :data value))))
          (:gb-bank-transfer
           (unless (eql 'null value)
             (setf (slot-value instance '%gb-bank-transfer)
                   (make-instance 'refund-destination-details-gb-bank-transfer :data value))))
          (:giropay
           (unless (eql 'null value)
             (setf (slot-value instance '%giropay)
                   (make-instance 'refund-destination-details-giropay :data value))))
          (:grabpay
           (unless (eql 'null value)
             (setf (slot-value instance '%grabpay)
                   (make-instance 'refund-destination-details-grabpay :data value))))
          (:jp-bank-transfer
           (unless (eql 'null value)
             (setf (slot-value instance '%jp-bank-transfer)
                   (make-instance 'refund-destination-details-jp-bank-transfer :data value))))
          (:klarna
           (unless (eql 'null value)
             (setf (slot-value instance '%klarna)
                   (make-instance 'refund-destination-details-klarna :data value))))
          (:multibanco
           (unless (eql 'null value)
             (setf (slot-value instance '%multibanco)
                   (make-instance 'refund-destination-details-multibanco :data value))))
          (:mx-bank-transfer
           (unless (eql 'null value)
             (setf (slot-value instance '%mx-bank-transfer)
                   (make-instance 'refund-destination-details-mx-bank-transfer :data value))))
          (:p24
           (unless (eql 'null value)
             (setf (slot-value instance '%p24)
                   (make-instance 'refund-destination-details-p24 :data value))))
          (:paynow
           (unless (eql 'null value)
             (setf (slot-value instance '%paynow)
                   (make-instance 'refund-destination-details-paynow :data value))))
          (:paypal
           (unless (eql 'null value)
             (setf (slot-value instance '%paypal)
                   (make-instance 'refund-destination-details-paypal :data value))))
          (:pix
           (unless (eql 'null value)
             (setf (slot-value instance '%pix)
                   (make-instance 'refund-destination-details-pix :data value))))
          (:revolut
           (unless (eql 'null value)
             (setf (slot-value instance '%revolut)
                   (make-instance 'refund-destination-details-revolut :data value))))
          (:sofort
           (unless (eql 'null value)
             (setf (slot-value instance '%sofort)
                   (make-instance 'refund-destination-details-sofort :data value))))
          (:swish
           (unless (eql 'null value)
             (setf (slot-value instance '%swish)
                   (make-instance 'refund-destination-details-swish :data value))))
          (:th-bank-transfer
           (unless (eql 'null value)
             (setf (slot-value instance '%th-bank-transfer)
                   (make-instance 'refund-destination-details-th-bank-transfer :data value))))
          (:us-bank-transfer
           (unless (eql 'null value)
             (setf (slot-value instance '%us-bank-transfer)
                   (make-instance 'refund-destination-details-us-bank-transfer :data value))))
          (:wechat-pay
           (unless (eql 'null value)
             (setf (slot-value instance '%wechat-pay)
                   (make-instance 'refund-destination-details-wechat-pay :data value))))
          (:zip
           (unless (eql 'null value)
             (setf (slot-value instance '%zip)
                   (make-instance 'refund-destination-details-zip :data value)))))))))

(define-object refund-destination-details-affirm ())
(define-object refund-destination-details-afterpay-clearpay ())
(define-object refund-destination-details-alipay ())
(define-object refund-destination-details-alma ())
(define-object refund-destination-details-amazon-pay ())
(define-object refund-destination-details-au-bank-transfer ())

(define-object refund-destination-details-blik ()
  (reference
   :type (or string null)
   :documentation "The reference assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference on the refund. This can be
`pending`, `available` or `unavailable`."))

(define-object refund-destination-details-br-bank-transfer ()
  (reference
   :type (or string null)
   :documentation "The reference assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference on the refund. This can be
`pending`, `available` or `unavailable`."))

(define-object refund-destination-details-card ()
  (reference
   :type (or string null)
   :documentation "Value of the reference number assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference number on the refund. This
can be `pending`, `available` or `unavailable`.")
  (reference-type
   :type (or string null)
   :documentation "Type of the reference number assigned to the refund.")
  (type
   :reader refund-destination-card-type
   :type string
   :documentation "The type of refund. This can be `refund`,
`reversal`, or `pending`."))

(define-object refund-destination-details-cashapp ())
(define-object refund-destination-details-customer-cash-balance ())
(define-object refund-destination-details-eps ())

(define-object refund-destination-details-eu-bank-transfer ()
  (reference
   :type (or string null)
   :documentation "The reference assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference on the refund. This can be
`pending`, `available` or `unavailable`."))

(define-object refund-destination-details-gb-bank-transfer ()
  (reference
   :type (or string null)
   :documentation "The reference assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference on the refund. This can be
`pending`, `available` or `unavailable`."))

(define-object refund-destination-details-giropay ())
(define-object refund-destination-details-grabpay ())

(define-object refund-destination-details-jp-bank-transfer ()
  (reference
   :type (or string null)
   :documentation "The reference assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference on the refund. This can be
`pending`, `available` or `unavailable`."))

(define-object refund-destination-details-klarna ())

(define-object refund-destination-details-multibanco ()
  (reference
   :type (or string null)
   :documentation "The reference assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference on the refund. This can be
`pending`, `available` or `unavailable`."))

(define-object refund-destination-details-mx-bank-transfer ()
  (reference
   :type (or string null)
   :documentation "The reference assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference on the refund. This can be
`pending`, `available` or `unavailable`."))

(define-object refund-destination-details-p24 ()
  (reference
   :type (or string null)
   :documentation "The reference assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference on the refund. This can be
`pending`, `available` or `unavailable`."))

(define-object refund-destination-details-paynow ())
(define-object refund-destination-details-paypal ())
(define-object refund-destination-details-pix ())
(define-object refund-destination-details-revolut ())
(define-object refund-destination-details-sofort ())

(define-object refund-destination-details-swish ()
  (reference
   :type (or string null)
   :documentation "The reference assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference on the refund. This can be
`pending`, `available` or `unavailable`."))

(define-object refund-destination-details-th-bank-transfer ()
  (reference
   :type (or string null)
   :documentation "The reference assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference on the refund. This can be
`pending`, `available` or `unavailable`."))

(define-object refund-destination-details-us-bank-transfer ()
  (reference
   :type (or string null)
   :documentation "The reference assigned to the refund.")
  (reference-status
   :type (or string null)
   :documentation "Status of the reference on the refund. This can be
`pending`, `available` or `unavailable`."))

(define-object refund-destination-details-wechat-pay ())
(define-object refund-destination-details-zip ())

(define-object refund-next-action ()
  (display-details
   :type (or refund-next-action-display-details null))
  (type
   :reader refund-next-action-type
   :type string
   :documentation "Type of the next action to perform."))

(defmethod initialize-instance :after ((instance refund-next-action) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:display-details
           (unless (eql 'null value)
             (setf (slot-value instance '%display-details)
                   (make-instance 'refund-next-action-display-details :data value)))))))))

(define-object refund-next-action-display-details ()
  (email-sent
   :type refund-next-action-display-details-email-sent)
  (expires-at
   :type time:timestamp
   :documentation "The expiry timestamp."))

(defmethod initialize-instance :after ((instance refund-next-action-display-details)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:email-sent
           (unless (eql 'null value)
             (setf (slot-value instance '%email-sent)
                   (make-instance 'refund-next-action-display-details-email-sent :data value))))
          (:expires-at
           (setf (slot-value instance '%expires-at) (decode-timestamp value))))))))

(define-object refund-next-action-display-details-email-sent ()
  (email-sent-at
   :type time:timestamp
   :documentation "The timestamp when the email was sent.")
  (email-sent-to
   :type string
   :documentation "The recipient's email address."))

(defmethod initialize-instance :after ((instance refund-next-action-display-details-email-sent)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:email-sent-at
           (setf (slot-value instance '%email-sent-at) (decode-timestamp value))))))))
