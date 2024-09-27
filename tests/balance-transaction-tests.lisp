(in-package #:stripe/tests)

(def-suite balance-transaction-tests)

(in-suite balance-transaction-tests)

(defparameter *retrieve-balance-transaction-json*
  "{
    \"id\": \"txn_1MiN3gLkdIwHu7ixxapQrznl\",
    \"object\": \"balance_transaction\",
    \"amount\": -400,
    \"available_on\": 1678043844,
    \"created\": 1678043844,
    \"currency\": \"usd\",
    \"description\": null,
    \"exchange_rate\": null,
    \"fee\": 0,
    \"fee_details\": [],
    \"net\": -400,
    \"reporting_category\": \"transfer\",
    \"source\": \"tr_1MiN3gLkdIwHu7ixNCZvFdgA\",
    \"status\": \"available\",
    \"type\": \"transfer\"
  }"
  "https://docs.stripe.com/api/balance_transactions/retrieve")

(define-mock-query mock-retrieve-balance-transaction (:type stripe:balance-transaction)
  *retrieve-balance-transaction-json*)

(test retrieve-balance-transaction
  (let ((transaction (mock-retrieve-balance-transaction "txn_1MiN3gLkdIwHu7ixxapQrznl")))
    (is (typep transaction 'stripe:balance-transaction))
    (is (string= "txn_1MiN3gLkdIwHu7ixxapQrznl" (slot-value transaction 'stripe::%id)))
    (is (= -400 (slot-value transaction 'stripe::%amount)))
    (is (= 1678043844 (local-time:timestamp-to-unix (slot-value transaction 'stripe::%available-on))))
    (is (= 1678043844 (local-time:timestamp-to-unix (slot-value transaction 'stripe::%created))))
    (is (string= "usd" (slot-value transaction 'stripe::%currency)))
    (is (= 0 (slot-value transaction 'stripe::%fee)))
    (is (= -400 (slot-value transaction 'stripe::%net)))
    (is (string= "tr_1MiN3gLkdIwHu7ixNCZvFdgA" (slot-value transaction 'stripe::%source)))
    (is (string= "available" (slot-value transaction 'stripe::%status)))
    (is (string= "transfer" (stripe:transaction-type transaction)))))

(defparameter *list-balance-history-json*
  "{
    \"object\": \"list\",
    \"url\": \"/v1/balance_transactions\",
    \"has_more\": false,
    \"data\": [
      {
        \"id\": \"txn_1MiN3gLkdIwHu7ixxapQrznl\",
        \"object\": \"balance_transaction\",
        \"amount\": -400,
        \"available_on\": 1678043844,
        \"created\": 1678043844,
        \"currency\": \"usd\",
        \"description\": null,
        \"exchange_rate\": null,
        \"fee\": 0,
        \"fee_details\": [],
        \"net\": -400,
        \"reporting_category\": \"transfer\",
        \"source\": \"tr_1MiN3gLkdIwHu7ixNCZvFdgA\",
        \"status\": \"available\",
        \"type\": \"transfer\"
      }
    ]
  }"
  "https://docs.stripe.com/api/balance_transactions/list")

(define-mock-query mock-list-balance-history (:type vector)
  *list-balance-history-json*)

(test list-balance-history
  (let* ((history (mock-list-balance-history))
         (transaction (aref history 0)))
    (is (vectorp history))
    (is (typep transaction 'stripe:balance-transaction))
    (is (string= "txn_1MiN3gLkdIwHu7ixxapQrznl" (slot-value transaction 'stripe::%id)))))
