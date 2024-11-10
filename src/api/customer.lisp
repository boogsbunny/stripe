(in-package #:stripe)

(define-query create-customer (:type customer)
  (:post "customers")
  balance
  description
  name
  email
  phone
  address
  shipping
  source)

(define-query retrieve-customer (:type customer)
  (:get "customers/~a" customer))

(define-query update-customer (:type customer)
  (:post "customers/~a" customer)
  address
  balance
  coupon
  default-source
  description
  email
  name
  phone
  shipping
  source)

(define-query delete-customer ()
  (:delete "customers/~a" customer))

(define-query list-customers (:type vector)
  (:get "customers")
  created
  email
  ending-before
  limit
  starting-after)
