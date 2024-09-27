(in-package #:stripe)

(define-object refund ()
  id
  amount
  balance-transaction
  charge
  created
  currency
  failure-balance-transaction
  failure-reason
  reason
  receipt-number
  status)

(defmethod initialize-instance :after ((instance refund) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value))))))))

(define-query create-refund (:type refund)
  (:post "refunds")
  charge
  amount
  reason)

(define-query retrieve-refund (:type refund)
  (:get "refunds/~a" refund))

(define-query list-refunds (:type vector)
  (:get "refunds")
  charge
  created
  ending-before
  limit
  starting-after)
