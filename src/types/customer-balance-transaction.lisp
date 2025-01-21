(in-package #:stripe)

(define-object customer-balance-transaction ()
  "Each customer has a [Balance]
(https://stripe.com/docs/api/customers/object#customer_object-balance)
value, which denotes a debit or credit that's automatically applied to
their next invoice upon finalization. You may modify the value directly
by using the [update customer API]
(https://stripe.com/docs/api/customers/update), or by creating a
Customer Balance Transaction, which increments or decrements the
customer's `balance` by the specified `amount`.

Related guide: [Customer balance]
(https://stripe.com/docs/billing/customer/balance)"
  id
  amount
  created
  credit-note
  currency
  customer
  description
  ending-balance
  invoice
  (type :reader transaction-type)
  (:list-type t))

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
