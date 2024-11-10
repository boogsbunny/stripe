(in-package #:stripe)

(define-query create-customer-tax-id (:type customer-tax-id)
  (:post "customers/~a/tax-ids" customer)
  type
  value)

(define-query retrieve-customer-tax-id (:type customer-tax-id)
  (:get "customers/~a/tax-ids/~a" customer tax-id))

(define-query delete-customer-tax-id ()
  (:delete "customers/~a/tax-ids/~a" customer tax-id))

(define-query list-customer-tax-ids (:type customer-tax-id)
  (:get "customers/~a/tax-ids" customer)
  ending-before
  limit
  starting-after)
