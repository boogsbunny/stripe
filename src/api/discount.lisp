(in-package #:stripe)

(define-query delete-customer-discount ()
  (:delete "customers/~a/discount" customer))

(define-query delete-subscription-discount ()
  (:delete "subscriptions/~a/discount" subscription))
