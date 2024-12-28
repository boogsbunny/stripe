(in-package #:stripe)

(define-object account-application-authorized-event-data (event-data)
  (object
   :type application
   :documentation "The application object.")
  (previous-attributes
   :type (or application null)
   :documentation "Previous attributes of the application, if any.")
  (:list-type nil))

(define-event account-application-authorized-event (:type "account.application.authorized")
  "Occurs whenever a user authorizes an application. Sent to the
related application only."
  (data
   :type account-application-authorized-event-data
   :documentation "The event data containing the application."))

(define-object account-application-deauthorized-event-data (event-data)
  (object
   :type application
   :documentation "The application object.")
  (previous-attributes
   :type (or application null)
   :documentation "Previous attributes of the application, if any.")
  (:list-type nil))

(define-event account-application-deauthorized-event (:type "account.application.deauthorized")
  "Occurs whenever a user deauthorizes an application. Sent to the
related application only."
  (data
   :type account-application-deauthorized-event-data
   :documentation "The event data containing the application."))

(define-object account-external-account-created-event-data (event-data)
  (object
   :type external-account
   :documentation "The external account object.")
  (previous-attributes
   :type (or external-account null)
   :documentation "Previous attributes of the external account, if any.")
  (:list-type nil))

(define-event account-external-account-created-event (:type "account.external_account.created")
  "Occurs whenever an external account is created."
  (data
   :type account-external-account-created-event-data
   :documentation "The event data containing the external account."))

(define-object account-external-account-deleted-event-data (event-data)
  (object
   :type external-account
   :documentation "The external account object.")
  (previous-attributes
   :type (or external-account null)
   :documentation "Previous attributes of the external account, if any.")
  (:list-type nil))

(define-event account-external-account-deleted-event (:type "account.external_account.deleted")
  "Occurs whenever an external account is deleted."
  (data
   :type account-external-account-deleted-event-data
   :documentation "The event data containing the external account."))

(define-object account-external-account-updated-event-data (event-data)
  (object
   :type external-account
   :documentation "The external account object.")
  (previous-attributes
   :type (or external-account null)
   :documentation "Previous attributes of the external account, if any.")
  (:list-type nil))

(define-event account-external-account-updated-event (:type "account.external_account.updated")
  "Occurs whenever an external account is updated."
  (data
   :type account-external-account-updated-event-data
   :documentation "The event data containing the external account."))

(define-object account-updated-event-data (event-data)
  (object
   :type account
   :documentation "The account object.")
  (previous-attributes
   :type (or account null)
   :documentation "Previous attributes of the account, if any.")
  (:list-type nil))

(define-event account-updated-event (:type "account.updated")
  "Occurs whenever an account status or property has changed."
  (data
   :type account-updated-event-data
   :documentation "The event data containing the account."))

(define-object application-fee-created-event-data (event-data)
  (object
   :type application-fee
   :documentation "The application fee object.")
  (previous-attributes
   :type (or application-fee null)
   :documentation "Previous attributes of the application fee, if any.")
  (:list-type nil))

(define-event application-fee-created-event (:type "application_fee.created")
  "Occurs whenever an application fee is created on a charge."
  (data
   :type application-fee-created-event-data
   :documentation "The event data containing the application fee."))

;; TODO:
;; (define-object application-fee-refund-updated-event-data (event-data)
;;   (object
;;    :type fee-refund
;;    :documentation "The fee refund object.")
;;   (previous-attributes
;;    :type (or fee-refund null)
;;    :documentation "Previous attributes of the fee refund, if any.")
;;   (:list-type nil))
;;
;; (define-event application-fee-refund-updated-event (:type "application_fee.refund.updated")
;;   "Occurs whenever an application fee refund is updated."
;;   (data
;;    :type application-fee-refund-updated-event-data
;;    :documentation "The event data containing the fee refund."))

(define-object application-fee-refunded-event-data (event-data)
  (object
   :type application-fee
   :documentation "The application fee object.")
  (previous-attributes
   :type (or application-fee null)
   :documentation "Previous attributes of the application fee, if any.")
  (:list-type nil))

(define-event application-fee-refunded-event (:type "application_fee.refunded")
  "Occurs whenever an application fee is refunded, whether from
refunding a charge or from refunding the application fee directly. This
includes partial refunds."
  (data
   :type application-fee-refunded-event-data
   :documentation "The event data containing the application fee."))

(define-object balance-available-event-data (event-data)
  (object
   :type balance
   :documentation "The balance object.")
  (previous-attributes
   :type (or balance null)
   :documentation "Previous attributes of the balance, if any.")
  (:list-type nil))

(define-event balance-available-event (:type "balance.available")
  "Occurs whenever your Stripe balance has been updated (e.g., when a
charge is available to be paid out). By default, Stripe automatically
transfers funds in your balance to your bank account on a daily basis.
This event is not fired for negative transactions."
  (data
   :type balance-available-event-data
   :documentation "The event data containing the balance."))

(define-object billing-alert-triggered-event-data (event-data)
  (object
   :type billing-alert-triggered
   :documentation "The billing alert triggered object.")
  (previous-attributes
   :type (or billing-alert-triggered null)
   :documentation "Previous attributes of the billing alert triggered
object, if any.")
  (:list-type nil))

(define-event billing-alert-triggered-event (:type "billing.alert.triggered")
  "Occurs whenever your custom alert threshold is met."
  (data
   :type billing-alert-triggered-event-data
   :documentation "The event data containing the billing alert that's
been triggered."))

(define-object billing-portal-configuration-created-event-data (event-data)
  (object
   :type billing-portal-configuration
   :documentation "The billing portal configuration.")
  (previous-attributes
   :type (or billing-portal-configuration null)
   :documentation "Previous attributes of the billing portal
configuration, if any.")
  (:list-type nil))

(define-event billing-portal-configuration-created-event (:type
                                                          "billing_portal.configuration.created")
  "Occurs whenever a portal configuration is created."
  (data
   :type billing-portal-configuration-created-event-data
   :documentation "The event data containing the billing portal
configuration."))

(define-object billing-portal-configuration-updated-event-data (event-data)
  (object
   :type billing-portal-configuration
   :documentation "The billing portal configuration.")
  (previous-attributes
   :type (or billing-portal-configuration null)
   :documentation "Previous attributes of the billing portal
configuration, if any.")
  (:list-type nil))

(define-event billing-portal-configuration-updated-event (:type
                                                          "billing_portal.configuration.updated")
  "Occurs whenever a portal configuration is updated."
  (data
   :type billing-portal-configuration-updated-event-data
   :documentation "The event data containing the billing portal
configuration."))
