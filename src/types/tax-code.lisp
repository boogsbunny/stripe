(in-package #:stripe)

(define-object tax-code ()
  "[Tax codes](https://stripe.com/docs/tax/tax-categories) classify
goods and services for tax purposes."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "tax_code"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (description
   :type string
   :documentation "A detailed description of which types of products
the tax code represents.")
  (name
   :type string
   :documentation "A short name for the tax code.")
  (:list-type t))
