(in-package #:stripe)

(define-object account ()
  "This is an object representing a Stripe account. You can retrieve it
to see properties on the account like its current requirements or if
the account is enabled to make live charges or receive payouts.

For accounts where [controller.requirement_collection]
(https://stripe.com/api/accounts/object#account_object-controller-requirement_collection)
is `application`, which includes Custom accounts, the properties
below are always returned.

For accounts where [controller.requirement_collection]
(https://stripe.com/api/accounts/object#account_object-controller-requirement_collection)
is `stripe`, which includes Standard and Express accounts, some
properties are only returned until you create an [Account Link]
(https://stripe.com/api/account_links) or
[Account Session](https://stripe.com/api/account_sessions) to start
Connect Onboarding. Learn about the
[differences between accounts](https://stripe.com/connect/accounts)."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "account"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (business-profile
   :documentation "Business information about the account.")
  (business-type
   :documentation "The business type. After you create an
[Account Link](https://stripe.com/api/account_links) or
[Account Session](https://stripe.com/api/account_sessions), this
property is only returned for accounts where
[controller.requirement_collection]
(https://stripe.com/api/accounts/object#account_object-controller-requirement_collection)
is `application`, which includes Custom accounts.")
  (capabilities)
  (charges-enabled
   :documentation "Whether the account can create live charges.")
  (company)
  (controller)
  (country
   :documentation "The account's country.")
  (created
   :type time:timestamp
   :documentation "Time at which the account was connected. Measured
in seconds since the Unix epoch.")
  (default-currency
   :type (or string null)
   :documentation "Three-letter ISO currency code representing the
default currency for the account. This must be a currency that [Stripe
supports in the account's country](https://stripe.com/docs/payouts).")
  (deleted
   :documentation "Indicates whether the object is deleted. Presence
indicates deletion.")
  (details-submitted
   :documentation "Whether account details have been submitted.
Accounts with Stripe Dashboard access, which includes Standard
accounts, cannot receive payouts before this is true. Accounts where
this is false should be directed to [an onboarding flow]
(https://stripe.com/connect/onboarding) to finish submitting account
details.")
  (email
   :type (or string null)
   :documentation "An email address associated with the account. It's
not used for authentication and Stripe doesn't market to this field
without explicit approval from the platform.")
  (external-accounts
   :documentation "External accounts (bank accounts and debit cards)
currently attached to this account. External accounts are only returned
for requests where `controller[is_controller]` is true.")
  (future-requirements)
  (individual
   :documentation "This is an object representing a person associated
with a Stripe account.

A platform cannot access a person for an account where
[account.controller.requirement_collection]
(https://stripe.com/api/accounts/object#account_object-controller-requirement_collection)
is `stripe`, which includes Standard and Express accounts, after
creating an Account Link or Account Session to start Connect
onboarding.

See the [Standard onboarding]
(https://stripe.com/connect/standard-accounts) or [Express onboarding]
(https://stripe.com/connect/express-accounts) documentation for
information about prefilling information and account onboarding steps.
Learn more about [handling identity verification with the API]
(https://stripe.com/connect/handling-api-verification#person-information).")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (payouts-enabled
   :type boolean
   :documentation "Whether Stripe can send payouts to this account.")
  (requirements)
  (settings
   :documentation "Options for customizing how the account functions
within Stripe.")
  (tos-acceptance)
  (type
   :reader account-type
   :documentation "The Stripe account type. Can be `standard`,
`express`, `custom`, or `none`."))
