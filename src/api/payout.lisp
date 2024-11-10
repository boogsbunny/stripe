(in-package #:stripe)

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
