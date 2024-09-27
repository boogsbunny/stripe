(in-package #:stripe)

(define-object charge ()
  id
  amount
  amount-refunded
  balance-transaction
  billing-details
  captured
  created
  currency
  customer
  description
  dispute
  failure-code
  failure-message
  fraud-details
  invoice
  order
  outcome
  paid
  payment-intent
  payment-method
  payment-method-details
  receipt-email
  receipt-number
  receipt-url
  refunded
  refunds
  review
  shipping
  statement-descriptor
  status)

(define-object charge-outcome ()
  network-status
  reason
  risk-level
  risk-score
  seller-message
  (type :reader outcome-type))

(define-object charge-card-details ()
  brand
  checks
  country
  exp-month
  exp-year
  fingerprint
  funding
  last4
  three-d-secure
  wallet)

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

(define-object charge-card-checks ()
  address-line1-check
  address-postal-code-check
  cvc-check)

(define-object charge-fraud-details ()
  stripe-report
  user-report)

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
           (let ((card (gethash :card value)))
             (unless (eql 'null card)
               (setf (slot-value instance '%payment-method-details)
                     (make-instance 'charge-card-details :data card)))))
          (:refunds
           (when value
             (setf (slot-value instance '%refunds)
                   (decode-hash-table value)))))))))

(define-query create-charge (:type charge)
  (:post "charges")
  amount
  currency
  capture
  customer
  description
  shipping
  source
  statement-descriptor)

(define-query retrieve-charge (:type charge)
  (:get "charges/~a" charge))

(define-query update-charge (:type charge)
  (:post "charges/~a" charge)
  customer
  description
  fraud-details)

(define-query list-charges (:type vector)
  (:get "charges")
  created
  customer
  ending-before
  limit
  payment-intent
  starting-after)
