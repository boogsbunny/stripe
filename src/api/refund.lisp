(in-package #:stripe)

(define-query create-refund (:type refund)
  (:post "refunds")
  charge
  amount
  reason)

(define-query retrieve-refund (:type refund)
  (:get "refunds/~a" refund))

(define-query list-refunds (:type vector)
  (:get "refunds")
  charge
  created
  ending-before
  limit
  starting-after)
