(in-package #:stripe)

(define-object invoice-item ()
  "Invoice Items represent the component lines of an [invoice]
(https://stripe.com/docs/api/invoices). An invoice item is added to an
invoice by creating or updating it with an `invoice` field, at which
point it will be included as [an invoice line item]
(https://stripe.com/docs/api/invoices/line_item) within [invoice.lines]
(https://stripe.com/docs/api/invoices/object#invoice_object-lines).

Invoice Items can be created before you are ready to actually send the
invoice. This can be particularly useful when combined with a
[subscription](https://stripe.com/docs/api/subscriptions). Sometimes
you want to add a charge or credit to a customer, but actually charge
or credit the customer's card only at the end of a regular billing
cycle. This is useful for combining several charges
(to minimize per-transaction fees), or for having Stripe tabulate your
usage-based billing totals.

Related guides: [Integrate with the Invoicing API]
(https://stripe.com/docs/invoicing/integration),
[Subscription Invoices]
(https://stripe.com/docs/billing/invoices/subscription#adding-upcoming-invoice-items)."
  id
  amount
  currency
  customer
  date
  description
  discountable
  invoice
  period-end
  period-start
  plan
  proration
  quantity
  subscription
  subscription-item
  unified-proration
  unit-amount)

(defmethod initialize-instance :after ((instance invoice-item) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:date
           (setf (slot-value instance '%date) (decode-timestamp value)))
          (:period
           (setf (slot-value instance '%period-end) (decode-timestamp (gethash :end value))
                 (slot-value instance '%period-start) (decode-timestamp (gethash :start value))))
          (:plan
           (unless (eql 'null value)
             (setf (slot-value instance '%plan) (make-instance 'plan :data value)))))))))
