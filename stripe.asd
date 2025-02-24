(defsystem #:stripe
  :description "A client for the Stripe payment API."
  :author ("Michael Fiano <mail@mfiano.net>")
  :maintainer "boogsbunny"
  :license "MIT"
  :homepage "https://github.com/boogsbunny/stripe"
  :encoding :utf-8
  :depends-on (#:alexandria
               #:com.inuoe.jzon
               #:dexador
               #:ironclad
               #:local-time
               #:serapeum)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "error")
   (:file "type")
   (:file "object")
   (:file "query")
   (:module "types"
    :components
    ((:file "account")
     (:file "application")
     (:file "application-fee")
     (:file "balance")
     (:file "balance-transaction")
     (:file "bank-account")
     (:module "billing"
      :components
      ((:file "alert")
       (:file "alert-triggered")
       (:file "meter")))
     (:module "billing-portal"
      :components
      ((:file "configuration")
       (:file "session")))
     (:file "capability")
     (:file "card")
     (:file "cash-balance")
     (:file "cash-balance-transaction")
     (:file "charge")
     (:file "coupon")
     (:file "credit-note")
     (:file "customer")
     (:file "customer-balance-transaction")
     (:file "customer-session")
     (:file "customer-source")
     (:file "customer-tax-id")
     (:file "discount")
     (:file "dispute")
     (:file "event")
     (:file "event-type")
     (:file "external-account")
     (:file "file")
     (:file "file-link")
     (:file "invoice")
     (:file "invoice-item")
     (:file "line-item")
     (:file "mandate")
     (:file "payment-intent")
     (:file "payment-link")
     (:file "payment-method")
     (:file "payout")
     (:file "plan")
     (:file "price")
     (:file "product")
     (:file "refund")
     (:file "review")
     (:file "session")
     (:file "setup-attempt")
     (:file "setup-intent")
     (:file "shipping-rate")
     (:file "source")
     (:file "subscription")
     (:file "subscription-item")
     (:file "tax-code")
     (:file "tax-id")
     (:file "tax-rate")
     (:module "test-helpers"
      :components
      ((:file "test-clock")))
     (:file "token")
     (:file "transfer")
     (:file "transfer-reversal")
     (:file "webhook")))
   (:module "api"
    :components
    ((:file "balance")
     (:file "balance-transaction")
     (:module "billing-portal"
      :components
      ((:file "configuration")
       (:file "session")))
     (:file "capability")
     (:file "card")
     (:file "cash-balance")
     (:file "cash-balance-transaction")
     (:file "charge")
     (:file "coupon")
     (:file "credit-note")
     (:file "customer")
     (:file "customer-balance-transaction")
     (:file "customer-session")
     (:file "customer-tax-id")
     (:file "discount")
     (:file "dispute")
     (:file "file")
     (:file "file-link")
     (:file "invoice")
     (:file "invoice-item")
     (:file "mandate")
     (:file "payout")
     (:file "plan")
     (:file "product")
     (:file "refund")
     (:file "session")
     (:file "subscription")
     (:file "subscription-item")
     (:file "token")))
   (:file "webhook"))
  :in-order-to ((test-op (test-op :stripe/tests))))

(defsystem #:stripe/tests
  :description "A test suite for the Stripe payment API client."
  :license "MIT"
  :homepage "https://github.com/boogsbunny/stripe"
  :encoding :utf-8
  :depends-on (#:fiveam
               #:stripe)
  :pathname "tests"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "balance-tests")
   (:file "balance-transaction-tests")
   (:file "card-tests")
   (:file "charge-tests")
   (:file "coupon-tests")
   (:file "credit-note-tests")
   (:file "customer-balance-transaction-tests")
   (:file "invoice-tests")
   (:file "invoice-item-tests")
   (:file "payout-tests")
   (:file "plan-tests")
   (:file "product-tests")
   (:file "refund-tests")
   (:file "session-tests")
   (:file "subscription-tests")
   (:file "subscription-item-tests")
   (:file "token-tests")
   (:file "webhook-tests")
   (:file "run-tests"))
  :perform (test-op (o c) (symbol-call ':stripe/tests '#:run)))
