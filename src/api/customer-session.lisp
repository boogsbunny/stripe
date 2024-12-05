(in-package #:stripe)

(define-query create-customer-session (:type customer-session)
  (:post "customer_sessions")
  (components
   :type customer-session-components
   :required t
   :documentation "Configuration for each component. Exactly 1
component must be enabled.")
  (customer
   :type string
   :required t
   :documentation "The ID of an existing customer for which to create
the Customer Session."))
