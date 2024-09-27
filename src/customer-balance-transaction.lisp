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

(define-query create-customer-balance-transaction (:type customer-balance-transaction)
  (:post "customers/~a/balance-transactions" customer)
  amount
  currency
  description)

(define-query retrieve-customer-balance-transaction (:type customer-balance-transaction)
  (:get "customers/~a/balance-transactions/~a" customer transaction))

(define-query update-customer-balance-transaction (:type customer-balance-transaction)
  (:post "customers/~a/balance-transactions/~a" customer transaction)
  description)

(define-query list-customer-balance-transactions (:type vector)
  (:get "customers/~a/balance-transactions" customer)
  ending-before
  limit
  starting-after)
