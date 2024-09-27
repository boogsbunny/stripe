(in-package #:stripe)

(define-object payout ()
  id
  amount
  arrival-date
  automatic
  balance-transaction
  created
  currency
  description
  destination
  failure-balance-transaction
  failure-code
  failure-message
  (method :reader payout-method)
  source-type
  statement-descriptor
  status
  (type :reader payout-type))

(defmethod initialize-instance :after ((instance payout) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:arrival-date
           (setf (slot-value instance '%arrival-date) (decode-timestamp value)))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value))))))))

(define-query create-payout (:type payout)
  (:post "payouts")
  amount
  currency
  description
  destination
  method
  source-type
  statement-descriptor)

(define-query retrieve-payout (:type payout)
  (:get "payouts" payout))

(define-query list-payouts (:type vector)
  (:get "payouts")
  arrival-date
  created
  destination
  ending-before
  limit
  starting-after
  status)

(define-query cancel-payout (:type payout)
  (:post "payouts/~a/cancel" payout))
