(in-package #:stripe)

(define-object invoice-item ()
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

(define-query create-invoice-item (:type invoice-item)
  (:post "invoiceitems")
  currency
  customer
  amount
  description
  discountable
  invoice
  period
  quantity
  subscription
  unit-amount)

(define-query retrieve-invoice-item (:type invoice-item)
  (:get "invoiceitems/~a" invoice-item))

(define-query update-invoice-item (:type invoice-item)
  (:post "invoiceitems/~a" invoice-item)
  amount
  description
  discountable
  period
  quantity
  unit-amount)

(define-query delete-invoice-item ()
  (:delete "invoiceitems/~a" invoice-item))

(define-query list-invoice-items (:type vector)
  (:get "invoiceitems")
  created
  customer
  ending-before
  invoice
  limit
  pending
  starting-after)
