(in-package #:cl-user)

(defpackage #:stripe
  (:use #:cl)
  (:local-nicknames
   (#:alex #:alexandria)
   (#:jzon #:com.inuoe.jzon)
   (#:u #:golden-utils))
  ;; common
  (:export
   #:*api-version*
   #:*api-key*
   #:*webhook-secret*)
  ;; conditions
  (:export
   #:stripe-error
   #:account-already-exists
   #:account-country-invalid-address
   #:account-invalid
   #:account-number-invalid
   #:alipay-upgrade-required
   #:amount-too-large
   #:amount-too-small
   #:api-key-expired
   #:balance-insufficient
   #:bank-account-exists
   #:bank-account-unusable
   #:bank-account-unverified
   #:bitcoin-upgrade-required
   #:card-declined
   #:charge-already-captured
   #:charge-already-refunded
   #:charge-disputed
   #:charge-exceeds-source-limit
   #:charge-expired-for-capture
   #:country-unsupported
   #:coupon-expired
   #:customer-max-subscriptions
   #:email-invalid
   #:expired-card
   #:idempotency-key-in-use
   #:incorrect-cvc
   #:incorrect-number
   #:incorrect-zip
   #:instant-payouts-unsupported
   #:invalid-card-type
   #:invalid-charge-amount
   #:invalid-cvc
   #:invalid-expiry-month
   #:invalid-expiry-year
   #:invalid-number
   #:invalid-source-usage
   #:invoice-no-customer-line-items
   #:invoice-no-subscription-line-items
   #:invoice-not-editable
   #:invoice-upcoming-none
   #:livemode-mismatch
   #:missing
   #:not-allowed-on-standard-account
   #:order-creation-failed
   #:order-required-settings
   #:order-status-invalid
   #:order-upstream-timeout
   #:parameter-invalid-empty
   #:parameter-invalid-integer
   #:parameter-invalid-string-blank
   #:parameter-invalid-string-empty
   #:parameter-missing
   #:parameter-unknown
   #:parameters-exclusive
   #:payment-intent-authentication-failure
   #:payment-intent-incompatible-payment-method
   #:payment-intent-invalid-parameter
   #:payment-intent-payment-attempt-failed
   #:payment-intent-unexpected-state
   #:payment-intent-unactivated
   #:payment-method-unexpected-state
   #:payouts-not-allowed
   #:platform-api-key-expired
   #:postal-code-invalid
   #:processing-error
   #:product-inactive
   #:rate-limit
   #:resource-already-exists
   #:resource-missing
   #:routing-number-invalid
   #:secret-key-required
   #:sepa-unsupported-account
   #:setup-attempt-failed
   #:setup-attempt-unexpected-state
   #:shipping-calculation-failed
   #:state-unsupported
   #:tax-id-invalid
   #:taxes-calculation-failed
   #:testmode-charges-only
   #:tls-version-unsupported
   #:token-already-used
   #:token-in-use
   #:transfers-not-allowed
   #:upstream-order-creation-failed
   #:url-invalid
   #:webhook-invalid-header
   #:webhook-no-valid-signature
   #:webhook-not-signed
   #:webhook-timestamp-too-old)
  ;; accessors
  (:export
   #:active
   #:account-country
   #:account-name
   #:address
   #:address-line1-check
   #:address-postal-code-check
   #:aggregate-usage
   #:amount
   #:amount-due
   #:amount-off
   #:amount-refunded
   #:amount-remaining
   #:application
   #:arrival-date
   #:attempt-count
   #:attributes
   #:auto-advance
   #:automatic
   #:available
   #:available-on
   #:available-payment-methods
   #:balance
   #:balance-transaction
   #:bank-account
   #:billing-cycle-anchor
   #:billing-details
   #:billing-reason
   #:billing-scheme
   #:brand
   #:cancel-at
   #:cancel-at-period-end
   #:canceled-at
   #:caption
   #:captured
   #:card
   #:carrier
   #:charge
   #:checks
   #:city
   #:client-ip
   #:collection-method
   #:country
   #:coupon
   #:created
   #:credit-note
   #:credit-note-number
   #:credit-note-type
   #:currency
   #:current-period-end
   #:current-period-start
   #:customer
   #:customer-address
   #:customer-balance-transactions
   #:customer-email
   #:customer-name
   #:customer-phone
   #:customer-shipping
   #:cvc-check
   #:date
   #:days-until-due
   #:default-payment-method
   #:default-payment-source
   #:default-source
   #:delinquent
   #:description
   #:destination
   #:discount
   #:discountable
   #:dispute
   #:due-date
   #:duration
   #:duration-in-months
   #:earliest
   #:email
   #:end
   #:ended-at
   #:ending-balance
   #:exchange-rate
   #:exp-month
   #:exp-year
   #:failure-balance-transaction
   #:failure-code
   #:failure-message
   #:failure-reason
   #:fee
   #:fee-details
   #:fee-type
   #:finalized-at
   #:fingerprint
   #:footer
   #:fraud-details
   #:funding
   #:height
   #:hosted-invoice-url
   #:id
   #:images
   #:interval
   #:invoice
   #:invoice-item
   #:invoice-number
   #:invoice-pdf
   #:items
   #:last4
   #:latest
   #:latest-invoice
   #:line1
   #:line2
   #:lines
   #:marked-uncollectible-at
   #:max-redemptions
   #:memo
   #:name
   #:net
   #:network-status
   #:next-payment-attempt
   #:nickname
   #:order
   #:order-item-type
   #:outcome
   #:outcome-type
   #:package-dimensions
   #:package-length
   #:paid
   #:paid-at
   #:parent
   #:payment-intent
   #:payment-method
   #:payment-method-details
   #:payment-status
   #:payout-method
   #:payout-type
   #:pdf
   #:pending
   #:percent-off
   #:period
   #:period-end
   #:period-start
   #:phone
   #:plan
   #:postal-code
   #:post-paymnt-credit-notes-amount
   #:pre-paymnt-credit-notes-amount
   #:price
   #:product
   #:proration
   #:quantity
   #:reason
   #:receipt-email
   #:receipt-number
   #:receipt-url
   #:redeem-by
   #:refund
   #:refunded
   #:refunds
   #:returns
   #:review
   #:risk-level
   #:risk-score
   #:schedule
   #:selected-shipping-method
   #:seller-message
   #:session
   #:session-url
   #:shippable
   #:shipping
   #:shipping-methods
   #:source
   #:source-type
   #:sources
   #:start
   #:start-date
   #:starting-balance
   #:state
   #:statement-descriptor
   #:status
   #:status-transitions
   #:stripe-report
   #:subscription
   #:subscription-item
   #:subscription-proration-date
   #:subscriptions
   #:subtotal
   #:tax
   #:tax-id-type
   #:three-d-secure
   #:times-redeemed
   #:tokenization-method
   #:total
   #:tracking-number
   #:transaction-type
   #:trial-end
   #:trial-period-days
   #:trial-start
   #:unified-proration
   #:unit-amount
   #:unit-label
   #:updated
   #:upstream-id
   #:url
   #:usage-type
   #:used
   #:user-report
   #:valid
   #:value
   #:verification
   #:verified-address
   #:verified-name
   #:voided-at
   #:wallet
   #:webhook-event
   #:webhook-event-type
   #:webhooks-delivered-at
   #:weight
   #:width)
  (:export
   #:construct-webhook-event
   #:parse-signature-header
   #:validate-webhook-payload)
  ;; requests
  (:export
   #:cancel-payout
   #:cancel-subscription
   #:create-card
   #:create-card-token
   #:create-charge
   #:create-coupon
   #:create-credit-note
   #:create-customer
   #:create-customer-balance-transaction
   #:create-customer-tax-id
   #:create-invoice
   #:create-invoice-item
   #:create-order
   #:create-payout
   #:create-plan
   #:create-product
   #:create-refund
   #:create-session
   #:create-subscription
   #:create-subscription-item
   #:delete-card
   #:delete-coupon
   #:delete-customer
   #:delete-customer-discount
   #:delete-customer-tax-id
   #:delete-invoice
   #:delete-invoice-item
   #:delete-plan
   #:delete-product
   #:delete-subscription-discount
   #:delete-subscription-item
   #:finalize-invoice
   #:list-balance-transactions
   #:list-cards
   #:list-charges
   #:list-coupons
   #:list-credit-notes
   #:list-customers
   #:list-customer-balance-transactions
   #:list-customer-tax-ids
   #:list-invoice-items
   #:list-invoices
   #:list-orders
   #:list-order-returns
   #:list-payouts
   #:list-plans
   #:list-products
   #:list-refunds
   #:list-subscriptions
   #:list-subscription-items
   #:mark-invoice-uncollectible
   #:pay-invoice
   #:pay-order
   #:retrieve-balance
   #:retrieve-balance-transaction
   #:retrieve-card
   #:retrieve-card-token
   #:retrieve-coupon
   #:retrieve-charge
   #:retrieve-credit-note
   #:retrieve-customer
   #:retrieve-customer-balance-transaction
   #:retrieve-customer-tax-id
   #:retrieve-invoice
   #:retrieve-invoice-item
   #:retrieve-invoice-lines
   #:retrieve-payout
   #:retrieve-plan
   #:retrieve-product
   #:retrieve-refund
   #:retrieve-session
   #:retrieve-subscription
   #:retrieve-subscription-item
   #:retrieve-upcoming-invoice
   #:retrieve-upcoming-invoice-lines
   #:send-invoice
   #:update-card
   #:update-charge
   #:update-coupon
   #:update-credit-note
   #:update-customer
   #:update-customer-balance-transaction
   #:update-invoice
   #:update-invoice-item
   #:update-order
   #:update-plan
   #:update-product
   #:update-subscription
   #:update-subscription-item
   #:void-credit-note
   #:void-invoice))
