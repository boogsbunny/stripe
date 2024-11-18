(in-package #:stripe)

(define-object invoice ()
  "Invoices are statements of amounts owed by a customer, and are
either generated one-off, or generated periodically from a
subscription.

They contain [invoice items](https://stripe.com/docs/api#invoiceitems),
and proration adjustments that may be caused by subscription
upgrades/downgrades (if necessary).

If your invoice is configured to be billed through automatic charges,
Stripe automatically finalizes your invoice and attempts payment. Note
that finalizing the invoice, [when automatic]
(https://stripe.com/docs/invoicing/integration/automatic-advancement-collection),
does not happen immediately as the invoice is created. Stripe waits
until one hour after the last webhook was successfully sent (or the
last webhook timed out after failing). If you (and the platforms you
may have connected to) have no webhooks configured, Stripe waits one
hour after creation to finalize the invoice.

If your invoice is configured to be billed by sending an email, then
based on your [email settings]
(https://dashboard.stripe.com/account/billing/automatic), Stripe will
email the invoice to your customer and await payment. These emails can
contain a link to a hosted page to pay the invoice.

Stripe applies any customer credit on the account before determining
the amount due for the invoice (i.e., the amount that will be actually
charged). If the amount due for the invoice is less than Stripe's
[minimum allowed charge per currency]
(https://stripe.com/docs/currencies#minimum-and-maximum-charge-amounts),
the invoice is automatically marked paid, and we add the amount due to
the customer's credit balance which is applied to the next invoice.

More details on the customer's credit balance are
[here](https://stripe.com/docs/billing/customer/balance).

Related guide: [Send invoices to customers]
(https://stripe.com/docs/billing/invoices/sending)"
  id
  account-country
  account-name
  amount-due
  amount-remaining
  attempt-count
  attempted
  auto-advance
  billing-reason
  charge
  collection-method
  created
  currency
  customer
  customer-address
  customer-email
  customer-name
  customer-phone
  customer-shipping
  default-payment-method
  default-source
  description
  discount
  due-date
  ending-balance
  footer
  hosted-invoice-url
  invoice-pdf
  lines
  next-payment-attempt
  (number :reader invoice-number)
  paid
  payment-intent
  period-end
  period-start
  post-payment-credit-notes-amount
  pre-payment-credit-notes-amount
  receipt-number
  starting-balance
  status
  status-transitions
  subscription
  subscription-proration-date
  subtotal
  tax
  total
  webhooks-delivered-at)

(defmethod initialize-instance :after ((instance invoice) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:customer-address
           (unless (eql 'null value)
             (setf (slot-value instance '%customer-address) (make-instance 'address :data value))))
          (:customer-shipping
           (unless (eql 'null value)
             (setf (slot-value instance '%customer-shipping) (make-instance 'shipping :data value))))
          (:discount
           (unless (eql 'null value)
             (setf (slot-value instance '%discount) (make-instance 'discount :data value))))
          (:due-date
           (setf (slot-value instance '%due-date) (decode-timestamp value)))
          (:next-payment-attempt
           (setf (slot-value instance '%next-payment-attempt) (decode-timestamp value)))
          (:period-end
           (setf (slot-value instance '%period-end) (decode-timestamp value)))
          (:period-start
           (setf (slot-value instance '%period-start) (decode-timestamp value)))
          (:status-transitions
           (unless (eql 'null value)
             (setf (slot-value instance '%status-transitions)
                   (make-instance 'invoice-status-transition :data value))))
          (:webhooks-delivered-at
           (setf (slot-value instance '%webhooks-delivered-at) (decode-timestamp value))))))))

(define-object invoice-status-transition ()
  finalized-at
  marked-uncollectible-at
  paid-at
  voided-at)

(defmethod initialize-instance :after ((instance invoice-status-transition)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:finalized-at
           (setf (slot-value instance '%finalized-at) (decode-timestamp value)))
          (:marked-uncollectible-at
           (setf (slot-value instance '%marked-uncollectible-at) (decode-timestamp value)))
          (:paid-at
           (setf (slot-value instance '%paid-at) (decode-timestamp value)))
          (:voided-at
           (setf (slot-value instance '%voided-at) (decode-timestamp value))))))))

(define-object invoice-line ()
  id
  amount
  currency
  description
  discountable
  invoice-item
  period
  proration
  quantity
  subscription
  subscription-item
  (type :reader source-type)
  unified-proration)
