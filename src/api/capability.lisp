(in-package #:stripe)

(define-query update-capability (:type capability)
  "Updates an existing Account Capability. Request or remove a
capability by updating its `requested` parameter."
  (:post "accounts/~a/capabilities/~a"
         (account
          :type string
          :required t
          :documentation "The ID of the account to which the capability
belongs.")
         (capability
          :type string
          :required t
          :documentation "The ID of the capability to update."))
  (requested
   :type boolean
   :documentation "To request a new capability for an account, pass
true. There can be a delay before the requested capability becomes
active. If the capability has any activation requirements, the response
includes them in the `requirements` arrays.

If a capability isn’t permanent, you can remove it from the account by
passing false. Some capabilities are permanent after they’ve been
requested. Attempting to remove a permanent capability returns an
error."))

(define-query retrieve-capability (:type capability)
  "Retrieves information about the specified account capability."
  (:get "accounts/~a/capabilities/~a"
        (account
         :type string
         :required t
         :documentation "The ID of the account to which the capability
belongs.")
        (capability
         :type string
         :required t
         :documentation "The ID of the capability to retrieve.")))

(define-query list-capabilities (:type vector)
  "Returns a list of capabilities associated with the account. The
capabilities are returned sorted by creation date, with the most recent
capability appearing first."
  (:get "accounts/~a/capabilities"
        (account
         :type string
         :required t
         :documentation "The ID of the account whose capabilities will
be retrieved.")))
