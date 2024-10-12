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

(define-query create-session (:type session)
  (:post "checkout/sessions")
  cancel-url
  line-items
  mode
  payment-method-types
  success-url)

(define-query retrieve-session (:type session)
  (:get "checkout/sessions/~a" id))
