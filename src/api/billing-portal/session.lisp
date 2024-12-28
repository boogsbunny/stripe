(in-package #:stripe)

(define-query create-billing-portal-session (:type billing-portal-session)
  "Creates a session of the customer portal.

For security reasons, sessions are short-lived and will expire if the
customer does not visit the URL. Create sessions on-demand when
customers intend to manage their subscriptions and billing details."
  (:post "billing_portal/sessions")
  (customer
   :type string
   :required t
   :documentation "The ID of the customer to create a portal session
for.")
  (configuration
   :type string
   :documentation "The ID of an existing [configuration](https://docs.stripe.com/api/customer_portal/configuration)
to use for this session, describing its functionality and features. If
not specified, the session uses the default configuration.")
  (flow-data
   :type billing-portal-session-flow
   :documentation "Information about a specific flow for the customer to
go through. See the [docs](https://docs.stripe.com/customer-management/portal-deep-links)
to learn more about using customer portal deep links and flows.")
  (locale
   :type string
   :documentation "The IETF language tag of the locale customer portal
is displayed in. If blank or auto, the customer's `preferred_locales`
or browser's locale is used.")
  (on-behalf-of
   :type string
   :documentation "The `on_behalf_of` account to use for this session.
When specified, only subscriptions and invoices with this `on_behalf_of`
account appear in the portal. For more information, see the
[docs](https://docs.stripe.com/connect/separate-charges-and-transfers#settlement-merchant).
Use the [Accounts API](https://docs.stripe.com/api/accounts/object#account_object-settings-branding)
to modify the on_behalf_of account’s branding settings, which the
portal displays.")
  (return-url
   :type string
   :documentation "The default URL to redirect customers to when they
click on the portal's link to return to your website."))
