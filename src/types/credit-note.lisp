(in-package #:stripe)

(define-object credit-note ()
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
  (type :reader credit-note-type))

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
