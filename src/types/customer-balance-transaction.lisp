(in-package #:stripe)

(define-object customer-balance-transaction ()
  id
  amount
  created
  credit-note
  currency
  customer
  description
  ending-balance
  invoice
  (type :reader transaction-type))

(defmethod initialize-instance :after ((instance customer-balance-transaction)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created)
                 (decode-timestamp value))))))))
