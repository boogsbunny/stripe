(in-package #:stripe)

(define-object balance-transaction ()
  id
  amount
  available-on
  created
  currency
  description
  exchange-rate
  fee
  fee-details
  net
  source
  status
  (type :reader transaction-type))

(define-object fee ()
  amount
  application
  currency
  description
  (type :reader fee-type))

(defmethod initialize-instance :after ((instance balance-transaction) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more key value)
          (next-entry)
        (unless more (return))
        (case key
          (:available-on
           (setf (slot-value instance '%available-on) (decode-timestamp value)))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:fee-details
           (setf (slot-value instance '%fee-details)
                 (when value
                   (map 'list
                        (lambda (fee-data)
                          (make-instance 'fee :data fee-data))
                        value)))))))))

(define-query retrieve-balance-transaction (:type balance-transaction)
  (:get "balance/history/~a" balance-transaction))

(define-query list-balance-history (:type vector)
  (:get "balance/history")
  available-on
  created
  currency
  ending-before
  limit
  payout
  source
  starting-after
  type)
