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

(define-object billing-portal-session-created-event-data (event-data)
  (object
   :type billing-portal-session
   :documentation "The billing portal session.")
  (previous-attributes
   :type (or billing-portal-session null)
   :documentation "Previous attributes of the billing portal session,
if any.")
  (:list-type nil))

(define-event billing-portal-session-created-event (:type "billing_portal.session.created")
  "Occurs whenever a portal session is created."
  (data
   :type billing-portal-session-created-event-data
   :documentation "The event data containing the billing portal
session."))

(define-object capability-updated-event-data (event-data)
  (object
   :type capability
   :documentation "The account capability.")
  (previous-attributes
   :type (or capability null)
   :documentation "Previous attributes of the account capability,
if any.")
  (:list-type nil))

(define-event capability-updated-event (:type "capability.updated")
  "Occurs whenever a capability has new requirements or a new status."
  (data
   :type capability-updated-event-data
   :documentation "The event data containing the account capability."))

(define-object cash-balance-funds-available-event-data (event-data)
  (object
   :type cash-balance
   :documentation "The cash balance.")
  (previous-attributes
   :type (or cash-balance null)
   :documentation "Previous attributes of the cash balance,
if any.")
  (:list-type nil))

(define-event cash-balance-funds-available-event (:type "cash_balance.funds_available")
  "Occurs whenever there is a positive remaining cash balance after
Stripe automatically reconciles new funds into the cash balance. If you
enabled manual reconciliation, this webhook will fire whenever there
are new funds into the cash balance."
  (data
   :type cash-balance-funds-available-event-data
   :documentation "The event data containing the cash balance."))

(define-object charge-captured-event-data (event-data)
  (object
   :type charge
   :documentation "The charge.")
  (previous-attributes
   :type (or charge null)
   :documentation "Previous attributes of the charge,
if any.")
  (:list-type nil))

(define-event charge-captured-event (:type "charge.captured")
  "Occurs whenever a previously uncaptured charge is captured."
  (data
   :type charge-captured-event-data
   :documentation "The event data containing the charge."))

(define-object charge-dispute-closed-event-data (event-data)
  (object
   :type dispute
   :documentation "The dispute.")
  (previous-attributes
   :type (or dispute null)
   :documentation "Previous attributes of the dispute,
if any.")
  (:list-type nil))

(define-event charge-dispute-closed-event (:type "charge.dispute.closed")
  "Occurs when a dispute is closed and the dispute status changes to
`lost`, `warning_closed`, or `won`."
  (data
   :type charge-dispute-closed-event-data
   :documentation "The event data containing the dispute."))

(define-object charge-dispute-created-event-data (event-data)
  (object
   :type dispute
   :documentation "The dispute.")
  (previous-attributes
   :type (or dispute null)
   :documentation "Previous attributes of the dispute,
if any.")
  (:list-type nil))

(define-event charge-dispute-closed-event (:type "charge.dispute.created")
  "Occurs whenever a customer disputes a charge with their bank."
  (data
   :type charge-dispute-created-event-data
   :documentation "The event data containing the dispute."))

(define-object charge-dispute-funds-reinstated-event-data (event-data)
  (object
   :type dispute
   :documentation "The dispute.")
  (previous-attributes
   :type (or dispute null)
   :documentation "Previous attributes of the dispute,
if any.")
  (:list-type nil))

(define-event charge-dispute-funds-reinstated-event (:type "charge.dispute.funds_reinstated")
  "Occurs when funds are reinstated to your account after a dispute is
closed. This includes [partially refunded payments](https://docs.stripe.com/disputes#disputes-on-partially-refunded-payments)."
  (data
   :type charge-dispute-funds-reinstated-event-data
   :documentation "The event data containing the dispute."))

(define-object charge-dispute-funds-withdrawn-event-data (event-data)
  (object
   :type dispute
   :documentation "The dispute.")
  (previous-attributes
   :type (or dispute null)
   :documentation "Previous attributes of the dispute,
if any.")
  (:list-type nil))

(define-event charge-dispute-funds-withdrawn-event (:type "charge.dispute.funds_withdrawn")
  "Occurs when funds are removed from your account due to a dispute."
  (data
   :type charge-dispute-funds-withdrawn-event-data
   :documentation "The event data containing the dispute."))

(define-object charge-dispute-updated-event-data (event-data)
  (object
   :type dispute
   :documentation "The dispute.")
  (previous-attributes
   :type (or dispute null)
   :documentation "Previous attributes of the dispute,
if any.")
  (:list-type nil))

(define-event charge-dispute-updated-event (:type "charge.dispute.updated")
  "Occurs when the dispute is updated (usually with evidence)."
  (data
   :type charge-dispute-updated-event-data
   :documentation "The event data containing the dispute."))

(define-object charge-expired-event-data (event-data)
  (object
   :type charge
   :documentation "The charge.")
  (previous-attributes
   :type (or charge null)
   :documentation "Previous attributes of the charge,
if any.")
  (:list-type nil))

(define-event charge-expired-event (:type "charge.expired")
  "Occurs whenever an uncaptured charge expires."
  (data
   :type charge-expired-event-data
   :documentation "The event data containing the charge."))

(define-object charge-failed-event-data (event-data)
  (object
   :type charge
   :documentation "The charge.")
  (previous-attributes
   :type (or charge null)
   :documentation "Previous attributes of the charge,
if any.")
  (:list-type nil))

(define-event charge-failed-event (:type "charge.failed")
  "Occurs whenever a failed charge attempt occurs."
  (data
   :type charge-failed-event-data
   :documentation "The event data containing the charge."))

(define-object charge-pending-event-data (event-data)
  (object
   :type charge
   :documentation "The charge.")
  (previous-attributes
   :type (or charge null)
   :documentation "Previous attributes of the charge,
if any.")
  (:list-type nil))

(define-event charge-pending-event (:type "charge.pending")
  "Occurs whenever a pending charge is created."
  (data
   :type charge-pending-event-data
   :documentation "The event data containing the charge."))

(define-object charge-refund-updated-event-data (event-data)
  (object
   :type refund
   :documentation "The refund.")
  (previous-attributes
   :type (or refund null)
   :documentation "Previous attributes of the refund,
if any.")
  (:list-type nil))

(define-event charge-refund-updated-event (:type "charge.refund.updated")
  "Occurs whenever a refund is updated, on selected payment methods."
  (data
   :type charge-refund-updated-event-data
   :documentation "The event data containing the refund."))

(define-object charge-refunded-event-data (event-data)
  (object
   :type charge
   :documentation "The charge.")
  (previous-attributes
   :type (or refund null)
   :documentation "Previous attributes of the charge,
if any.")
  (:list-type nil))

(define-event charge-refunded-event (:type "charge.refunded")
  "Occurs whenever a charge is refunded, including partial refunds."
  (data
   :type charge-refunded-event-data
   :documentation "The event data containing the charge refunded."))

(define-object charge-succeeded-event-data (event-data)
  (object
   :type charge
   :documentation "The charge.")
  (previous-attributes
   :type (or refund null)
   :documentation "Previous attributes of the charge,
if any.")
  (:list-type nil))

(define-event charge-succeeded-event (:type "charge.suceeded")
  "Occurs whenever a charge is successful."
  (data
   :type charge-succeeded-event-data
   :documentation "The event data containing the charge."))

(define-object charge-updated-event-data (event-data)
  (object
   :type charge
   :documentation "The charge.")
  (previous-attributes
   :type (or refund null)
   :documentation "Previous attributes of the charge,
if any.")
  (:list-type nil))

(define-event charge-updated-event (:type "charge.updated")
  "Occurs whenever a charge description or metadata is updated, or upon
an asynchronous capture."
  (data
   :type charge-updated-event-data
   :documentation "The event data containing the charge."))
