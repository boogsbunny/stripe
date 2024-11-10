(in-package #:stripe)

(define-query create-session (:type session)
  (:post "checkout/sessions")
  cancel-url
  line-items
  mode
  payment-method-types
  success-url)

(define-query retrieve-session (:type session)
  (:get "checkout/sessions/~a" id))
