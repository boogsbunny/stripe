(in-package #:stripe)

(define-object session ()
  id
  cancel-url
  client-reference-id
  currency
  customer
  customer-email
  line-items
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  mode
  payment-intent
  payment-method-types
  payment-status
  success-url
  (type :reader session-url)
  (:list-type t))
