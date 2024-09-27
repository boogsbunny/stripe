(defsystem #:stripe
  :description "A client for the Stripe payment API."
  :author ("Michael Fiano <mail@mfiano.net>")
  :maintainer "Atlas Engineer LLC"
  :license "MIT"
  :homepage "https://github.com/atlas-engineer/stripe"
  :encoding :utf-8
  :depends-on (#:alexandria
               #:com.inuoe.jzon
               #:dexador
               #:golden-utils
               #:ironclad
               #:local-time)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "common")
   (:file "error")
   (:file "object")
   (:file "query")
   (:file "balance")
   (:file "balance-transaction")
   (:file "card")
   (:file "charge")
   (:file "coupon")
   (:file "credit-note")
   (:file "customer")
   (:file "customer-balance-transaction")
   (:file "customer-tax-id")
   (:file "discount")
   (:file "invoice")
   (:file "invoice-item")
   (:file "order")
   (:file "payout")
   (:file "plan")
   (:file "product")
   (:file "refund")
   (:file "return")
   (:file "session")
   (:file "sku")
   (:file "subscription")
   (:file "subscription-item")
   (:file "token")
   (:file "webhook"))
  :in-order-to ((test-op (test-op :stripe/tests))))

(defsystem #:stripe/tests
  :description "A test suite for the Stripe payment API client."
  :license "MIT"
  :homepage "https://github.com/atlas-engineer/stripe"
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
