(in-package #:stripe)

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
