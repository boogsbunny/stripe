(in-package #:stripe)

(define-object credit-note ()
  "Issue a credit note to adjust an invoice's amount after the invoice
is finalized.

Related guide: [Credit notes]
(https://stripe.com/docs/billing/invoices/credit-notes)"
  id
  amount
  created
  currency
  customer
  customer-balance-transaction
  invoice
  memo
  (number :reader credit-note-number)
  pdf
  reason
  refund
  status
  (type :reader credit-note-type)
  (:list-type t))

(defmethod initialize-instance :after ((instance credit-note) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created)
                 (decode-timestamp value))))))))
