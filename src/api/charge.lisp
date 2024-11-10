(in-package #:stripe)

(define-query retrieve-charge (:type charge)
  (:get "charges/~a" charge))

(define-query update-charge (:type charge)
  (:post "charges/~a" charge)
  customer
  description
  fraud-details
  metadata
  receipt-email
  shipping
  transfer-group)

(define-query list-charges (:type vector)
  (:get "charges")
  created
  customer
  ending-before
  limit
  payment-intent
  starting-after
  transfer-group)

(define-query capture-charge (:type charge)
  (:post "charges/~a/capture" charge)
  amount
  application-fee-amount
  receipt-email
  statement-descriptor
  statement-descriptor-suffix
  transfer-data
  transfer-group)

(define-query search-charge (:type vector)
  (:get "charges/search" charge)
  query
  limit
  page)
