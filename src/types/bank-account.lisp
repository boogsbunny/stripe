(in-package #:stripe)

(define-object bank-account ()
  "These bank accounts are payment methods on `Customer` objects.

On the other hand [External Accounts]
(https://stripe.com/api#external_accounts) are transfer destinations on
`Account` objects for connected accounts. They can be bank accounts or
debit cards as well, and are documented in the links above.

Related guide: [Bank debits and transfers]
(https://stripe.com/payments/bank-debits-transfers)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "bank_account"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (account
   :type (or string account null)
   :documentation "The ID of the account that the bank account is
associated with.")
  (account-holder-name
   :type (or string null)
   :documentation "The name of the person or business that owns the
bank account.")
  (account-holder-type
   :type (or string null)
   :documentation "The type of entity that holds the account. This can
be either `individual` or `company`.")
  (account-type
   :type (or string null)
   :documentation "The bank account type. This can only be `checking`
or `savings` in most countries. In Japan, this can only be `futsu` or
`toza`.")
  (available-payout-methods
   :type (or (vector string) null)
   :documentation "A set of available payout methods for this bank
account. One of `instant` or `standard`. Only values from this set
should be passed as the `method` when creating a payout.")
  (bank-name
   :type (or string null)
   :documentation "Name of the bank associated with the routing number
(e.g., `WELLS FARGO`).")
  (country
   :type string
   :documentation "Two-letter ISO code representing the country the
bank account is located in.")
  (currency
   :type string
   :documentation "Three-letter [ISO code for the currency]
(https://stripe.com/docs/payouts) paid out to the bank account.")
  (customer
   :type (or string customer deleted-customer null)
   :documentation "The ID of the customer that the bank account is
associated with.")
  (default-for-currency
   :type (or boolean null)
   :documentation "Whether this bank account is the default external
account for its currency.")
  (deleted
  :documentation "Indicates whether the object is deleted. Presence
indicates deletion.")
  (fingerprint
   :type (or string null)
   :documentation "Uniquely identifies this particular bank account.
You can use this attribute to check whether two bank accounts are the
same.")
  (future-requirements
   :type (or bank-account-future-requirements null)
   :documentation "Information about the [upcoming new requirements for
the bank account]
(https://stripe.com/docs/connect/custom-accounts/future-requirements),
including what information needs to be collected, and by when.")
  (:list-type t))

(define-object bank-account-future-requirements ()
  (currently-due
   :type (or (vector string) null)
   :documentation "Fields that need to be collected to keep the
external account enabled. If not collected by `current_deadline`, these
fields appear in `past_due` as well, and the account is disabled.")
  (errors
   :type (or (vector bank-account-future-requirements-error) null)
   :documentation "Fields that need to be collected to keep the
external account enabled. If not collected by `current_deadline`, these
fields appear in `past_due` as well, and the account is disabled.")
  (past-due
   :type (or (vector string) null)
   :documentation "Fields that weren't collected by `current_deadline`.
These fields need to be collected to enable the external account.")
  (pending-verification
   :type (or (vector string) null)
   :documentation "Fields that might become required depending on the
results of verification or review. It's an empty array unless an
asynchronous verification is pending. If verification fails, these
fields move to `eventually_due`, `currently_due`, or `past_due`. Fields
might appear in `eventually_due`, `currently_due`, or `past_due` and in
`pending_verification` if verification fails but another verification
is still pending."))

(define-object bank-account-future-requirements-error ()
  (code
   :type string
   :documentation "The code for the type of error. One of
`invalid_address_city_state_postal_code`,
`invalid_address_highway_contract_box`,
`invalid_address_private_mailbox`,
`invalid_business_profile_name`,
`invalid_business_profile_name_denylisted`,
`invalid_company_name_denylisted`,
`invalid_dob_age_over_maximum`,
`invalid_dob_age_under_18`,
`invalid_dob_age_under_minimum`,
`invalid_product_description_length`,
`invalid_product_description_url_match`,
`invalid_representative_country`,
`invalid_statement_descriptor_business_mismatch`,
`invalid_statement_descriptor_denylisted`,
`invalid_statement_descriptor_length`,
`invalid_statement_descriptor_prefix_denylisted`,
`invalid_statement_descriptor_prefix_mismatch`,
`invalid_street_address`,
`invalid_tax_id`,
`invalid_tax_id_format`,
`invalid_tos_acceptance`,
`invalid_url_denylisted`,
`invalid_url_format`,
`invalid_url_length`,
`invalid_url_web_presence_detected`,
`invalid_url_website_business_information_mismatch`,
`invalid_url_website_empty`,
`invalid_url_website_inaccessible`,
`invalid_url_website_inaccessible_geoblocked`,
`invalid_url_website_inaccessible_password_protected`,
`invalid_url_website_incomplete`,
`invalid_url_website_incomplete_cancellation_policy`,
`invalid_url_website_incomplete_customer_service_details`,
`invalid_url_website_incomplete_legal_restrictions`,
`invalid_url_website_incomplete_refund_policy`,
`invalid_url_website_incomplete_return_policy`,
`invalid_url_website_incomplete_terms_and_conditions`,
`invalid_url_website_incomplete_under_construction`,
`invalid_url_website_other`,
`invalid_value_other`,
`verification_directors_mismatch`,
`verification_document_address_mismatch`,
`verification_document_address_missing`,
`verification_document_corrupt`,
`verification_document_country_not_supported`,
`verification_document_directors_mismatch`,
`verification_document_dob_mismatch`,
`verification_document_duplicate_type`,
`verification_document_expired`,
`verification_document_failed_copy`,
`verification_document_failed_greyscale`,
`verification_document_failed_other`,
`verification_document_failed_test_mode`,
`verification_document_fraudulent`,
`verification_document_id_number_mismatch`,
`verification_document_id_number_missing`,
`verification_document_incomplete`,
`verification_document_invalid`,
`verification_document_issue_or_expiry_date_missing`,
`verification_document_manipulated`,
`verification_document_missing_back`,
`verification_document_missing_front`,
`verification_document_name_mismatch`,
`verification_document_name_missing`,
`verification_document_nationality_mismatch`,
`verification_document_not_readable`,
`verification_document_not_signed`,
`verification_document_not_uploaded`,
`verification_document_photo_mismatch`,
`verification_document_too_large`,
`verification_document_type_not_supported`,
`verification_extraneous_directors`,
`verification_failed_address_match`,
`verification_failed_business_iec_number`,
`verification_failed_document_match`,
`verification_failed_id_number_match`,
`verification_failed_keyed_identity`,
`verification_failed_keyed_match`,
`verification_failed_name_match`,
`verification_failed_other`,
`verification_failed_representative_authority`,
`verification_failed_residential_address`,
`verification_failed_tax_id_match`,
`verification_failed_tax_id_not_issued`,
`verification_missing_directors`,
`verification_missing_executives`,
`verification_missing_owners`,
`verification_requires_additional_memorandum_of_associations`,
`verification_requires_additional_proof_of_registration`,
or `verification_supportability`.")
  (reason
   :type string
   :documentation "An informative message that indicates the error type
and provides additional details about the error.")
  (requirement
   :type string
   :documentation "The specific user onboarding requirement field (in
the requirements hash) that needs to be resolved."))

(defmethod initialize-instance :after ((instance bank-account) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:account
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%account)
                   (make-instance 'account :data value))))
          (:available-payout-methods
           (when value
             (setf (slot-value instance '%available-payout-methods)
                   (coerce value 'vector))))
          (:customer
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%customer)
                   (make-instance 'customer :data value))))
          (:future-requirements
           (when value
             (setf (slot-value instance '%future-requirements)
                   (make-instance 'bank-account-future-requirements :data value)))))))))

(define-object deleted-bank-account ()
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "bank_account"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (currency
   :type (or string null)
   :documentation "Three-letter [ISO code for the currency]
(https://stripe.com/docs/payouts) paid out to the bank account.")
  (deleted
   :type boolean
   :initform t
   :documentation "Always true for a deleted object.")
  (:list-type t))
