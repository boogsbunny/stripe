(in-package #:stripe)

(define-object invoice ()
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

(define-query create-invoice (:type invoice)
  (:post "invoices")
  auto-advance
  collection-method
  customer
  days-until-due
  default-payment-method
  default-source
  description
  due-date
  footer
  statement-descriptor
  subscription)

(define-query retrieve-invoice (:type invoice)
  (:get "invoices/~a" invoice))

(define-query update-invoice (:type invoice)
  (:post "invoices/~a" invoice)
  auto-advance
  collection-method
  days-until-due
  default-payment-method
  default-source
  description
  due-date
  footer
  statement-descriptor)

(define-query delete-invoice (:type invoice)
  (:delete "invoices/~a" invoice))

(define-query finalize-invoice (:type invoice)
  (:post "invoices/~a/finalize" invoice)
  auto-advance)

(define-query pay-invoice (:type invoice)
  (:post "invoices/~a/pay" invoice)
  forgive
  paid-out-of-band
  payment-method
  source)

(define-query send-invoice (:type invoice)
  (:post "invoices/~a/send" invoice))

(define-query void-invoice (:type invoice)
  (:post "invoices/~a/void" invoice))

(define-query mark-invoice-uncollectible (:type invoie)
  (:post "invoices/~a/mark-uncollectible" invoice))

(define-query retrieve-invoice-lines (:type vector)
  (:get "invoices/~a/lines" invoice)
  ending-before
  limit
  starting-after)

(define-query retrieve-upcoming-invoice (:type invoice)
  (:get "invoices/upcoming")
  coupon
  customer
  invoice-items
  schedule
  subscription
  subscription-billing-cycle-anchor
  subscription-cancel-at
  subscription-cancel-at-period-end
  subscription-cancel-now
  subscription-items
  subscription-prorate
  subscription-proration-date
  subscription-start-date
  subscription-trial-end
  subscription-trial-from-plan)

(define-query retrieve-upcoming-invoice-lines (:type vector)
  (:get "invoices/upcoming/lines")
  coupon
  customer
  ending-before
  invoice-items
  limit
  schedule
  starting-after
  subscription
  subscription-billing-anchor
  subscription-cancel-at
  subscription-cancel-at-period-end
  subscription-cancel-now
  subscription-items
  subscription-prorate
  subscription-proration-date
  subscription-start-date
  subscription-trial-end
  subscription-trial-from-plan)

(define-query list-invoices (:type vector)
  (:get "invoices")
  collection-method
  created
  customer
  due-date
  ending-before
  limit
  starting-after
  status
  subscription)
