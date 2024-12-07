(in-package #:stripe)

(defun decode-error (condition)
  (let* ((response (jzon:parse (dex:response-body condition)))
         (error (gethash :error response))
         (code (when error (gethash :code error)))
         (message (when error (gethash :message error))))
    (values (or (when code
                  (find-symbol (normalize-string code) :stripe))
                'stripe-error)
            message)))

(define-condition stripe-error (error)
  ((message :reader message
            :initarg :message))
  (:report (lambda (condition stream)
             (format stream "Stripe error: ~a" (message condition)))))

(sera:export-always 'account-already-exists :stripe)
(define-condition account-already-exists (stripe-error) ())

(sera:export-always 'account-country-invalid-address :stripe)
(define-condition account-country-invalid-address (stripe-error) ())

(sera:export-always 'account-invalid :stripe)
(define-condition account-invalid (stripe-error) ())

(sera:export-always 'account-number-invalid :stripe)
(define-condition account-number-invalid (stripe-error) ())

(sera:export-always 'alipay-upgrade-required :stripe)
(define-condition alipay-upgrade-required (stripe-error) ())

(sera:export-always 'amount-too-large :stripe)
(define-condition amount-too-large (stripe-error) ())

(sera:export-always 'amount-too-small :stripe)
(define-condition amount-too-small (stripe-error) ())

(sera:export-always 'api-key-expired :stripe)
(define-condition api-key-expired (stripe-error) ())

(sera:export-always 'balance-insufficient :stripe)
(define-condition balance-insufficient (stripe-error) ())

(sera:export-always 'bank-account-exists :stripe)
(define-condition bank-account-exists (stripe-error) ())

(sera:export-always 'bank-account-unusable :stripe)
(define-condition bank-account-unusable (stripe-error) ())

(sera:export-always 'bank-account-unverified :stripe)
(define-condition bank-account-unverified (stripe-error) ())

(sera:export-always 'bitcoin-upgrade-required :stripe)
(define-condition bitcoin-upgrade-required (stripe-error) ())

(sera:export-always 'card-declined :stripe)
(define-condition card-declined (stripe-error) ())

(sera:export-always 'charge-already-captured :stripe)
(define-condition charge-already-captured (stripe-error) ())

(sera:export-always 'charge-already-refunded :stripe)
(define-condition charge-already-refunded (stripe-error) ())

(sera:export-always 'charge-disputed :stripe)
(define-condition charge-disputed (stripe-error) ())

(sera:export-always 'charge-exceeds-source-limit :stripe)
(define-condition charge-exceeds-source-limit (stripe-error) ())

(sera:export-always 'charge-expired-for-capture :stripe)
(define-condition charge-expired-for-capture (stripe-error) ())

(sera:export-always 'country-unsupported :stripe)
(define-condition country-unsupported (stripe-error) ())

(sera:export-always 'coupon-expired :stripe)
(define-condition coupon-expired (stripe-error) ())

(sera:export-always 'customer-max-subscriptions :stripe)
(define-condition customer-max-subscriptions (stripe-error) ())

(sera:export-always 'email-invalid :stripe)
(define-condition email-invalid (stripe-error) ())

(sera:export-always 'expired-card :stripe)
(define-condition expired-card (stripe-error) ())

(sera:export-always 'idempotency-key-in-use :stripe)
(define-condition idempotency-key-in-use (stripe-error) ())

(sera:export-always 'incorrect-address :stripe)
(define-condition incorrect-address (stripe-error) ())

(sera:export-always 'incorrect-cvc :stripe)
(define-condition incorrect-cvc (stripe-error) ())

(sera:export-always 'incorrect-number :stripe)
(define-condition incorrect-number (stripe-error) ())

(sera:export-always 'incorrect-zip :stripe)
(define-condition incorrect-zip (stripe-error) ())

(sera:export-always 'instant-payouts-unsupported :stripe)
(define-condition instant-payouts-unsupported (stripe-error) ())

(sera:export-always 'invalid-card-type :stripe)
(define-condition invalid-card-type (stripe-error) ())

(sera:export-always 'invalid-charge-amount :stripe)
(define-condition invalid-charge-amount (stripe-error) ())

(sera:export-always 'invalid-cvc :stripe)
(define-condition invalid-cvc (stripe-error) ())

(sera:export-always 'invalid-expiry-month :stripe)
(define-condition invalid-expiry-month (stripe-error) ())

(sera:export-always 'invalid-expiry-year :stripe)
(define-condition invalid-expiry-year (stripe-error) ())

(sera:export-always 'invalid-number :stripe)
(define-condition invalid-number (stripe-error) ())

(sera:export-always 'invalid-source-usage :stripe)
(define-condition invalid-source-usage (stripe-error) ())

(sera:export-always 'invoice-no-customer-line-items :stripe)
(define-condition invoice-no-customer-line-items (stripe-error) ())

(sera:export-always 'invoice-no-subscription-line-items :stripe)
(define-condition invoice-no-subscription-line-items (stripe-error) ())

(sera:export-always 'invoice-not-editable :stripe)
(define-condition invoice-not-editable (stripe-error) ())

(sera:export-always 'invoice-upcoming-none :stripe)
(define-condition invoice-upcoming-none (stripe-error) ())

(sera:export-always 'livemode-mismatch :stripe)
(define-condition livemode-mismatch (stripe-error) ())

(sera:export-always 'missing :stripe)
(define-condition missing (stripe-error) ())

(sera:export-always 'not-allowed-on-standard-account :stripe)
(define-condition not-allowed-on-standard-account (stripe-error) ())

(sera:export-always 'parameter-invalid-empty :stripe)
(define-condition parameter-invalid-empty (stripe-error) ())

(sera:export-always 'parameter-invalid-integer :stripe)
(define-condition parameter-invalid-integer (stripe-error) ())

(sera:export-always 'parameter-invalid-string-blank :stripe)
(define-condition parameter-invalid-string-blank (stripe-error) ())

(sera:export-always 'parameter-invalid-string-empty :stripe)
(define-condition parameter-invalid-string-empty (stripe-error) ())

(sera:export-always 'parameter-missing :stripe)
(define-condition parameter-missing (stripe-error) ())

(sera:export-always 'parameter-unknown :stripe)
(define-condition parameter-unknown (stripe-error) ())

(sera:export-always 'parameters-exclusive :stripe)
(define-condition parameters-exclusive (stripe-error) ())

(sera:export-always 'payment-intent-authentication-failure :stripe)
(define-condition payment-intent-authentication-failure (stripe-error) ())

(sera:export-always 'payment-intent-incompatible-payment-method :stripe)
(define-condition payment-intent-incompatible-payment-method (stripe-error) ())

(sera:export-always 'payment-intent-invalid-parameter :stripe)
(define-condition payment-intent-invalid-parameter (stripe-error) ())

(sera:export-always 'payment-intent-payment-attempt-failed :stripe)
(define-condition payment-intent-payment-attempt-failed (stripe-error) ())

(sera:export-always 'payment-intent-unexpected-state :stripe)
(define-condition payment-intent-unexpected-state (stripe-error) ())

(sera:export-always 'payment-method-unactivated :stripe)
(define-condition payment-method-unactivated (stripe-error) ())

(sera:export-always 'payment-method-unexpected-state :stripe)
(define-condition payment-method-unexpected-state (stripe-error) ())

(sera:export-always 'payouts-not-allowed :stripe)
(define-condition payouts-not-allowed (stripe-error) ())

(sera:export-always 'platform-api-key-expired :stripe)
(define-condition platform-api-key-expired (stripe-error) ())

(sera:export-always 'postal-code-invalid :stripe)
(define-condition postal-code-invalid (stripe-error) ())

(sera:export-always 'processing-error :stripe)
(define-condition processing-error (stripe-error) ())

(sera:export-always 'product-inactive :stripe)
(define-condition product-inactive (stripe-error) ())

(sera:export-always 'rate-limit :stripe)
(define-condition rate-limit (stripe-error) ())

(sera:export-always 'resource-already-exists :stripe)
(define-condition resource-already-exists (stripe-error) ())

(sera:export-always 'resource-missing :stripe)
(define-condition resource-missing (stripe-error) ())

(sera:export-always 'routing-number-invalid :stripe)
(define-condition routing-number-invalid (stripe-error) ())

(sera:export-always 'secret-key-required :stripe)
(define-condition secret-key-required (stripe-error) ())

(sera:export-always 'sepa-unsupported-account :stripe)
(define-condition sepa-unsupported-account (stripe-error) ())

(sera:export-always 'setup-attempt-failed :stripe)
(define-condition setup-attempt-failed (stripe-error) ())

(sera:export-always 'setup-attempt-unexpected-state :stripe)
(define-condition setup-attempt-unexpected-state (stripe-error) ())

(sera:export-always 'shipping-calculation-failed :stripe)
(define-condition shipping-calculation-failed (stripe-error) ())

(sera:export-always 'state-unsupported :stripe)
(define-condition state-unsupported (stripe-error) ())

(sera:export-always 'tax-id-invalid :stripe)
(define-condition tax-id-invalid (stripe-error) ())

(sera:export-always 'taxes-calculation-failed :stripe)
(define-condition taxes-calculation-failed (stripe-error) ())

(sera:export-always 'testmode-charges-only :stripe)
(define-condition testmode-charges-only (stripe-error) ())

(sera:export-always 'tls-version-unsupported :stripe)
(define-condition tls-version-unsupported (stripe-error) ())

(sera:export-always 'token-already-used :stripe)
(define-condition token-already-used (stripe-error) ())

(sera:export-always 'token-in-use :stripe)
(define-condition token-in-use (stripe-error) ())

(sera:export-always 'transfers-not-allowed :stripe)
(define-condition transfers-not-allowed (stripe-error) ())

(sera:export-always 'upstream-order-creation-failed :stripe)
(define-condition upstream-order-creation-failed (stripe-error) ())

(sera:export-always 'url-invalid :stripe)
(define-condition url-invalid (stripe-error) ())

(sera:export-always 'webhook-invalid-header :stripe)
(define-condition webhook-invalid-header (stripe-error) ())

(sera:export-always 'webhook-no-valid-signature :stripe)
(define-condition webhook-no-valid-signature (stripe-error) ())

(sera:export-always 'webhook-not-signed :stripe)
(define-condition webhook-not-signed (stripe-error) ())

(sera:export-always 'webhook-timestamp-too-old :stripe)
(define-condition webhook-timestamp-too-old (stripe-error) ())
