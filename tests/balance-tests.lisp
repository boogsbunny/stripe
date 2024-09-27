(in-package #:stripe/tests)

(def-suite balance-tests)

(in-suite balance-tests)

(defparameter *retrieve-balance-json*
  "{
    \"object\": \"balance\",
    \"available\": [
      {
        \"amount\": 666670,
        \"currency\": \"usd\",
        \"source_types\": {
          \"card\": 666670
        }
      }
    ],
    \"connect_reserved\": [
      {
        \"amount\": 0,
        \"currency\": \"usd\"
      }
    ],
    \"livemode\": false,
    \"pending\": [
      {
        \"amount\": 61414,
        \"currency\": \"usd\",
        \"source_types\": {
          \"card\": 61414
        }
      }
    ]
  }"
  "https://docs.stripe.com/api/balance/balance_retrieve")

(define-mock-query mock-retrieve-balance (:type stripe:balance)
  *retrieve-balance-json*)

(test retrieve-balance
  (let* ((balance (mock-retrieve-balance))
         (available-funds (first (slot-value balance 'stripe::%available)))
         (pending-funds (first (slot-value balance 'stripe::%pending))))
    (is (typep balance 'stripe:balance))
    ;; available funds
    (is (typep available-funds 'stripe::balance-funds))
    (is (= 666670 (slot-value available-funds 'stripe::%amount)))
    (is (string= "usd" (slot-value available-funds 'stripe::%currency)))
    (is (= 666670 (slot-value available-funds 'stripe::%card)))
    (is (= 0 (slot-value available-funds 'stripe::%bank-account)))
    ;; pending funds
    (is (typep pending-funds 'stripe::balance-funds))
    (is (= 61414 (slot-value pending-funds 'stripe::%amount)))
    (is (string= "usd" (slot-value pending-funds 'stripe::%currency)))
    (is (= 61414 (slot-value pending-funds 'stripe::%card)))
    (is (= 0 (slot-value pending-funds 'stripe::%bank-account)))))
