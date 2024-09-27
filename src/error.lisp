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

(define-condition account-already-exists (stripe-error) ())

(define-condition account-country-invalid-address (stripe-error) ())

(define-condition account-invalid (stripe-error) ())

(define-condition account-number-invalid (stripe-error) ())

(define-condition alipay-upgrade-required (stripe-error) ())

(define-condition amount-too-large (stripe-error) ())

(define-condition amount-too-small (stripe-error) ())

(define-condition api-key-expired (stripe-error) ())

(define-condition balance-insufficient (stripe-error) ())

(define-condition bank-account-exists (stripe-error) ())

(define-condition bank-account-unusable (stripe-error) ())

(define-condition bank-account-unverified (stripe-error) ())

(define-condition bitcoin-upgrade-required (stripe-error) ())

(define-condition card-declined (stripe-error) ())

(define-condition charge-already-captured (stripe-error) ())

(define-condition charge-already-refunded (stripe-error) ())

(define-condition charge-disputed (stripe-error) ())

(define-condition charge-exceeds-source-limit (stripe-error) ())

(define-condition charge-expired-for-capture (stripe-error) ())

(define-condition country-unsupported (stripe-error) ())

(define-condition coupon-expired (stripe-error) ())

(define-condition customer-max-subscriptions (stripe-error) ())

(define-condition email-invalid (stripe-error) ())

(define-condition expired-card (stripe-error) ())

(define-condition idempotency-key-in-use (stripe-error) ())

(define-condition incorrect-address (stripe-error) ())

(define-condition incorrect-cvc (stripe-error) ())

(define-condition incorrect-number (stripe-error) ())

(define-condition incorrect-zip (stripe-error) ())

(define-condition instant-payouts-unsupported (stripe-error) ())

(define-condition invalid-card-type (stripe-error) ())

(define-condition invalid-charge-amount (stripe-error) ())

(define-condition invalid-cvc (stripe-error) ())

(define-condition invalid-expiry-month (stripe-error) ())

(define-condition invalid-expiry-year (stripe-error) ())

(define-condition invalid-number (stripe-error) ())

(define-condition invalid-source-usage (stripe-error) ())

(define-condition invoice-no-customer-line-items (stripe-error) ())

(define-condition invoice-no-subscription-line-items (stripe-error) ())

(define-condition invoice-not-editable (stripe-error) ())

(define-condition invoice-upcoming-none (stripe-error) ())

(define-condition livemode-mismatch (stripe-error) ())

(define-condition missing (stripe-error) ())

(define-condition not-allowed-on-standard-account (stripe-error) ())

(define-condition order-creation-failed (stripe-error) ())

(define-condition order-required-settings (stripe-error) ())

(define-condition order-status-invalid (stripe-error) ())

(define-condition order-upstream-timeout (stripe-error) ())

(define-condition out-of-inventory (stripe-error) ())

(define-condition parameter-invalid-empty (stripe-error) ())

(define-condition parameter-invalid-integer (stripe-error) ())

(define-condition parameter-invalid-string-blank (stripe-error) ())

(define-condition parameter-invalid-string-empty (stripe-error) ())

(define-condition parameter-missing (stripe-error) ())

(define-condition parameter-unknown (stripe-error) ())

(define-condition parameters-exclusive (stripe-error) ())

(define-condition payment-intent-authentication-failure (stripe-error) ())

(define-condition payment-intent-incompatible-payment-method (stripe-error) ())

(define-condition payment-intent-invalid-parameter (stripe-error) ())

(define-condition payment-intent-payment-attempt-failed (stripe-error) ())

(define-condition payment-intent-unexpected-state (stripe-error) ())

(define-condition payment-method-unactivated (stripe-error) ())

(define-condition payment-method-unexpected-state (stripe-error) ())

(define-condition payouts-not-allowed (stripe-error) ())

(define-condition platform-api-key-expired (stripe-error) ())

(define-condition postal-code-invalid (stripe-error) ())

(define-condition processing-error (stripe-error) ())

(define-condition product-inactive (stripe-error) ())

(define-condition rate-limit (stripe-error) ())

(define-condition resource-already-exists (stripe-error) ())

(define-condition resource-missing (stripe-error) ())

(define-condition routing-number-invalid (stripe-error) ())

(define-condition secret-key-required (stripe-error) ())

(define-condition sepa-unsupported-account (stripe-error) ())

(define-condition setup-attempt-failed (stripe-error) ())

(define-condition setup-attempt-unexpected-state (stripe-error) ())

(define-condition shipping-calculation-failed (stripe-error) ())

(define-condition sku-inactive (stripe-error) ())

(define-condition state-unsupported (stripe-error) ())

(define-condition tax-id-invalid (stripe-error) ())

(define-condition taxes-calculation-failed (stripe-error) ())

(define-condition testmode-charges-only (stripe-error) ())

(define-condition tls-version-unsupported (stripe-error) ())

(define-condition token-already-used (stripe-error) ())

(define-condition token-in-use (stripe-error) ())

(define-condition transfers-not-allowed (stripe-error) ())

(define-condition upstream-order-creation-failed (stripe-error) ())

(define-condition url-invalid (stripe-error) ())

(define-condition webhook-invalid-header (stripe-error) ())

(define-condition webhook-no-valid-signature (stripe-error) ())

(define-condition webhook-not-signed (stripe-error) ())

(define-condition webhook-timestamp-too-old (stripe-error) ())
