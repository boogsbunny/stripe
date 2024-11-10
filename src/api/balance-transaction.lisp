(in-package #:stripe)

(define-query retrieve-balance-transaction (:type balance-transaction)
  (:get "balance_transactions/~a" balance-transaction))

(define-query list-balance-transactions (:type vector)
  (:get "balance_transactions")
  created
  currency
  ending-before
  limit
  payout
  source
  starting-after
  type)
