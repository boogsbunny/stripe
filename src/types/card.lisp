(in-package #:stripe)

(define-object card ()
  id
  (object
   :type string
   :initform "card"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
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
  tokenization-method
  (:list-type t))

(define-object deleted-card ()
  id
  object
  currency
  deleted
  (:list-type t))
