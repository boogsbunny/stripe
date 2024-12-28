(in-package #:stripe)

(define-object card ()
  id
  address-city
  address-country
  address-line1
  address-line2
  address-state
  address-zip
  address-zip-check
  available-payment-methods
  brand
  country
  customer
  exp-month
  exp-year
  fingerprint
  funding
  last4
  name
  tokenization-method)

(define-object deleted-card ()
  id
  object
  currency
  deleted)
