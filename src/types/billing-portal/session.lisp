(in-package #:stripe)

(define-object billing-portal-session ()
  "The Billing customer portal is a Stripe-hosted UI for subscription
and billing management.

A portal configuration describes the functionality and features that
you want to provide to your customers through the portal.

A portal session describes the instantiation of the customer portal
for a particular customer. By visiting the session's URL, the customer
can manage their subscriptions and billing details. For security
reasons, sessions are short-lived and will expire if the customer does
not visit the URL. Create sessions on-demand when customers intend to
manage their subscriptions and billing details.

Related guide: [Customer management](https://stripe.com/customer-management)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "billing_portal.session"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (configuration
   :type (or string billing-portal-configuration)
   :documentation "The configuration used by this session, describing
the features available.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (customer
   :type string
   :documentation "The ID of the customer for this session.")
  (flow
   :type (or billing-portal-session-flow null)
   :documentation "Information about a specific flow for the customer
to go through. See the [docs]
(https://stripe.com/docs/customer-management/portal-deep-links) to
learn more about using customer portal deep links and flows.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (locale
   :type (or string null)
   :documentation "The IETF language tag of the locale Customer Portal
is displayed in. If blank or auto, the customer's `preferred_locales`
or browser's locale is used. One of `auto`, `bg`, `cs`, `da`, `de`,
`el`, `en`, `en-AU`, `en-CA`, `en-GB`, `en-IE`, `en-IN`, `en-NZ`,
`en-SG`, `es`, `es-419`, `et`, `fi`, `fil`, `fr`, `fr-CA`, `hr`, `hu`,
`id`, `it`, `ja`, `ko`, `lt`, `lv`, `ms`, `mt`, `nb`, `nl`, `pl`, `pt`,
 `pt-BR`, `ro`, `ru`, `sk`, `sl`, `sv`, `th`, `tr`, `vi`, `zh`,
`zh-HK`, or `zh-TW`.")
  (on-behalf-of
   :type (or string null)
   :documentation "The account for which the session was created on
behalf of. When specified, only subscriptions and invoices with this
`on_behalf_of` account appear in the portal. For more information, see
the [docs](https://stripe.com/docs/connect/separate-charges-and-transfers#settlement-merchant).
Use the [Accounts API](https://stripe.com/docs/api/accounts/object#account_object-settings-branding)
to modify the `on_behalf_of` account's branding settings, which the
portal displays.")
  (return-url
   :type (or string null)
   :documentation "The URL to redirect customers to when they click on
the portal's link to return to your website.")
  (url
   :type string
   :documentation "The short-lived URL of the session that gives
customers access to the customer portal.")
  (:list-type t))

(defmethod initialize-instance :after ((instance billing-portal-session) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:configuration
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%configuration)
                   (make-instance 'billing-portal-configuration :data value))))
          (:flow
           (unless (eql 'null value)
             (setf (slot-value instance '%flow)
                   (make-instance 'billing-portal-session-flow :data value)))))))))

(define-object billing-portal-session-flow ()
  (after-completion
   :type flow-after-completion)
  (subscription-cancel
   :type (or flow-subscription-cancel null)
   :documentation "Configuration when `flow.type=subscription_cancel`.")
  (subscription-update
   :type (or flow-subscription-update null)
   :documentation "Configuration when `flow.type=subscription_update`.")
  (subscription-update-confirm
   :type (or flow-subscription-update-confirm null)
   :documentation "Configuration when
`flow.type=subscription_update_confirm`.")
  (type
   :reader flow-type
   :documentation "Type of flow that the customer will go through. One
of `payment_method_update`, `subscription_cancel`,
`subscription_update`, or `subscription_update_confirm`."))

(defmethod initialize-instance :after ((instance billing-portal-session-flow)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:after-completion
           (unless (eql 'null value)
             (setf (slot-value instance '%after-completion)
                   (make-instance 'flow-after-completion :data value))))
          (:subscription-cancel
           (unless (eql 'null value)
             (setf (slot-value instance '%subscription-cancel)
                   (make-instance 'flow-subscription-cancel :data value))))
          (:subscription-update
           (unless (eql 'null value)
             (setf (slot-value instance '%subscription-update)
                   (make-instance 'flow-subscription-update :data value))))
          (:subscription-update-confirm
           (unless (eql 'null value)
             (setf (slot-value instance '%subscription-update-confirm)
                   (make-instance 'flow-subscription-update-confirm :data value)))))))))

(define-object flow-after-completion ()
  (hosted-confirmation
   :type (or flow-after-completion-hosted-confirmation null)
   :documentation "Configuration when
`after_completion.type=hosted_confirmation`.")
  (redirect
   :type (or flow-after-completion-redirect null)
   :documentation "Configuration when `after_completion.type=redirect`.")
  (type
   :reader after-completion-type
   :type string
   :documentation "The specified type of behavior after the flow is
completed. One of `hosted_confirmation`, `portal_homepage`, or
`redirect`."))

(defmethod initialize-instance :after ((instance flow-after-completion)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:hosted-confirmation
           (unless (eql 'null value)
             (setf (slot-value instance '%hosted-confirmation)
                   (make-instance 'flow-after-completion-hosted-confirmation :data value))))
          (:redirect
           (unless (eql 'null value)
             (setf (slot-value instance '%redirect)
                   (make-instance 'flow-after-completion-redirect :data value)))))))))

(define-object flow-after-completion-hosted-confirmation ()
  (custom-message
   :type (or string null)
   :documentation "A custom message to display to the customer after
the flow is completed."))

(define-object flow-after-completion-redirect ()
  (return-url
   :type string
   :documentation "The URL the customer will be redirected to after the
flow is completed."))

(define-object flow-subscription-cancel ()
  (retention
   :type (or flow-subscription-cancel-retention null)
   :documentation "Specify a retention strategy to be used in the
cancellation flow.")
  (subscription
   :type string
   :documentation "The ID of the subscription to be canceled."))

(defmethod initialize-instance :after ((instance flow-subscription-cancel)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:retention
           (unless (eql 'null value)
             (setf (slot-value instance '%retention)
                   (make-instance 'flow-subscription-cancel-retention :data value)))))))))

(define-object flow-subscription-cancel-retention ()
  (coupon-offer
   :type (or flow-subscription-cancel-retention-coupon-offer null)
   :documentation "Configuration when `retention.type=coupon_offer`.")
  (type
   :reader retention-type
   :type string
   :initform "coupon_offer"
   :documentation "Type of retention strategy that will be used."))

(defmethod initialize-instance :after ((instance flow-subscription-cancel-retention)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:coupon-offer
           (unless (eql 'null value)
             (setf (slot-value instance '%coupon-offer)
                   (make-instance 'flow-subscription-cancel-retention-coupon-offer
                                  :data value)))))))))

(define-object flow-subscription-cancel-retention-coupon-offer ()
  (coupon
   :type string
   :documentation "The ID of the coupon to be offered."))

(define-object flow-subscription-update ()
  (subscription
   :type string
   :documentation "The ID of the subscription to be updated."))

(define-object flow-subscription-update-confirm ()
  (discounts
   :type (or (vector flow-subscription-update-confirm-discount) null)
   :documentation "The coupon or promotion code to apply to this
subscription update. Currently, only up to one may be specified.")
  (items
   :type (vector flow-subscription-update-confirm-item)
   :documentation "The [subscription item](https://stripe.com/docs/api/subscription_items)
to be updated through this flow. Currently, only up to one may be
specified and subscriptions with multiple items are not updatable.")
  (subscription
   :type string
   :documentation "The ID of the subscription to be updated."))

(defmethod initialize-instance :after ((instance flow-subscription-update-confirm)
                                       &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:discounts
           (unless (eql 'null value)
             (setf (slot-value instance '%discounts)
                   (map 'vector
                        (lambda (discount)
                          (make-instance 'flow-subscription-update-confirm-discount
                                         :data discount))
                        value))))
          (:items
           (unless (eql 'null value)
             (setf (slot-value instance '%items)
                   (map 'vector
                        (lambda (item)
                          (make-instance 'flow-subscription-update-confirm-item
                                         :data item))
                        value)))))))))

(define-object flow-subscription-update-confirm-discount ()
  (coupon
   :type (or string null)
   :documentation "The ID of the coupon to apply to this subscription
update.")
  (promotion-code
   :type (or string null)
   :documentation "The ID of a promotion code to apply to this
subscription update."))

(define-object flow-subscription-update-confirm-item ()
  (id
   :type (or string null)
   :documentation "The ID of the [subscription item](https://stripe.com/docs/api/subscriptions/object#subscription_object-items-data-id)
to be updated.")
  (price
   :type (or string null)
   :documentation "The price the customer should subscribe to through
this flow. The price must also be included in the configuration's
[`features.subscription_update.products`](https://stripe.com/docs/api/customer_portal/configuration#portal_configuration_object-features-subscription_update-products).")
  (quantity
   :type (or integer null)
   :documentation "[Quantity](https://stripe.com/docs/subscriptions/quantities)
for this item that the customer should subscribe to through this flow."))
