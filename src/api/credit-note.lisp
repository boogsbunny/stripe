(in-package #:stripe)

(define-query create-credit-note (:type credit-note)
  (:post "credit-notes")
  amount
  invoice
  credit-amount
  memo
  reason
  refund
  refund-amount)

(define-query retrieve-credit-note (:type credit-note)
  (:get "credit-notes/~a" credit-note))

(define-query update-credit-note (:type credit-note)
  (:post "credit-notes/~a" credit-note)
  memo)

(define-query void-credit-note (:type credit-note)
  (:post "credit-notes/~a/void" credit-note))

(define-query list-credit-notes (:type vector)
  (:get "credit-notes")
  ending-before
  invoice
  limit
  starting-after)
