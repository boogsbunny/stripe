(in-package #:stripe)

(define-query create-billing-portal-configuration (:type billing-portal-configuration)
  "Creates a configuration that describes the functionality and
behavior of a portal session."
  (:post "billing_portal/configurations")
  (features
   :type configuration-features
   :documentation "Information about the features available in the
portal.")
  (business-profile
   :type configuration-business-profile
   :documentation "The business information shown to customers in the
portal.")
  (default-return-url
   :type string
   :documentation "The default URL to redirect customers to when they
click on the portal's link to return to your website. This can be
[overriden](https://docs.stripe.com/api/customer_portal/sessions/create#create_portal_session-return_url)
when creating the session.")
  (login-page
   :type api-login-page
   :documentation "The hosted login page for this configuration.")
  (metadata
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format. Individual keys can be unset by posting
an empty value to them. All keys can be unset by posting an empty value
o `metadata`."))

(define-query update-billing-portal-configuration (:type billing-portal-configuration)
  "Updates a configuration that describes the functionality of the
customer portal."
  (:post "billing_portal/configurations/~a"
         (id
          :type string
          :required t
          :documentation "The ID of the portal configuration to update."))
  (active
   :type boolean
   :documentation "Whether the configuration is active and can be used
to create portal sessions.")
  (features
   :type configuration-features
   :documentation "Information about the features available in the
portal.")
  (business-profile
   :type configuration-business-profile
   :documentation "The business information shown to customers in the
portal.")
  (default-return-url
   :type string
   :documentation "The default URL to redirect customers to when they
click on the portal's link to return to your website. This can be
[overriden](https://docs.stripe.com/api/customer_portal/sessions/create#create_portal_session-return_url)
when creating the session.")
  (login-page
   :type api-login-page
   :documentation "The hosted login page for this configuration.")
  (metadata
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format. Individual keys can be unset by posting
an empty value to them. All keys can be unset by posting an empty value
o `metadata`."))

(define-query retrieve-billing-portal-configuration (:type billing-portal-configuration)
  "Retrieves a configuration that describes the functionality of the
customer portal."
  (:get "billing_portal/configurations/~a"
        (id
         :type string
         :required t
         :documentation "The ID of the portal configuration to
retrieve.")))

(define-query list-billing-portal-configurations (:type vector)
  "Returns a list of configurations that describe the functionality of
the customer portal."
  (:get "billing_portal/configurations")
  (active
   :type boolean
   :documentation "Only return configurations that are active or
inactive (e.g., pass `true` to only list active configurations).")
  (is-default
   :type boolean
   :documentation "Only return the default or non-default
configurations (e.g., pass `true` to only list the default
configuration).")
  (ending-before
   :type string
   :documentation "A cursor for use in pagination. `ending_before` is
an object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, starting with `obj_bar`,
your subsequent call can include `ending_before=obj_bar` in order to
fetch the previous page of the list.")
  (limit
   :type integer
   :documentation "A limit on the number of objects to be returned.
Limit can range between 1 and 100, and the default is 10.")
  (starting-after
   :type string
   :documentation "A cursor for use in pagination. `starting_after` is
an object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, ending with `obj_foo`,
your subsequent call can include `starting_after=obj_foo` in order to
fetch the next page of the list."))
