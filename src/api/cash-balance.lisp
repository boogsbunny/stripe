(in-package #:stripe)

(define-query update-cash-balance (:type cash-balance)
  "Updates a customer's cash balance configuration."
  (:post
   "customers/~a/cash_balance"
   (id
    :type string
    :documentation "The ID of the customer whose cash balance
configuration to update."))
  (settings
   :type cash-balance-settings-request
   :documentation "A hash of settings for this cash balance."))

(define-query retrieve-cash-balance (:type cash-balance)
  "Retrieve a customer's cash balance.

Returns the Cash Balance object for a given customer."
  (:get
   "customers/~a/cash_balance"
   (id
    :type string
    :documentation "The ID of the customer whose cash balance to
retrieve.")))
