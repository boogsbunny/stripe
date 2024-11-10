(in-package #:stripe)

(define-object session ()
  id
  cancel-url
  client-reference-id
  currency
  customer
  customer-email
  line-items
  metadata
  mode
  payment-intent
  payment-method-types
  payment-status
  success-url
  (type :reader session-url))
