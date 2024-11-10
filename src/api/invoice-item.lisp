(in-package #:stripe)

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
