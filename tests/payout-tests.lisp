(in-package #:stripe/tests)

(def-suite payout-tests)

(in-suite payout-tests)

(defparameter *create-payout-json*
  "{
     \"id\": \"po_1OaFDbEcg9tTZuTgNYmX0PKB\",
     \"object\": \"payout\",
     \"amount\": 1100,
     \"arrival_date\": 1680652800,
     \"automatic\": false,
     \"balance_transaction\": \"txn_1OaFDcEcg9tTZuTgYMR25tSe\",
     \"created\": 1680648691,
     \"currency\": \"usd\",
     \"description\": null,
     \"destination\": \"ba_1MtIhL2eZvKYlo2CAElKwKu2\",
     \"failure_balance_transaction\": null,
     \"failure_code\": null,
     \"failure_message\": null,
     \"livemode\": false,
     \"metadata\": {},
     \"method\": \"standard\",
     \"original_payout\": null,
     \"reconciliation_status\": \"not_applicable\",
     \"reversed_by\": null,
     \"source_type\": \"card\",
     \"statement_descriptor\": null,
     \"status\": \"pending\",
     \"type\": \"bank_account\"
   }"
  "https://docs.stripe.com/api/payouts/create")

(define-mock-query mock-create-payout (:type stripe::payout)
  *create-payout-json*)

(test create-payout
  (let ((payout (mock-create-payout)))
    (is (typep payout 'stripe::payout))
    (is (string= (slot-value payout 'stripe::%id) "po_1OaFDbEcg9tTZuTgNYmX0PKB"))
    (is (= (slot-value payout 'stripe::%amount) 1100))
    (is (local-time:timestamp= (slot-value payout 'stripe::%arrival-date)
                               (stripe::decode-timestamp 1680652800)))
    (is-false (slot-value payout 'stripe::%automatic))
    (is (string= (slot-value payout 'stripe::%balance-transaction) "txn_1OaFDcEcg9tTZuTgYMR25tSe"))
    (is (local-time:timestamp= (slot-value payout 'stripe::%created)
                               (stripe::decode-timestamp 1680648691)))
    (is (string= (slot-value payout 'stripe::%currency) "usd"))
    (null (slot-value payout 'stripe::%description))
    (is (string= (slot-value payout 'stripe::%destination) "ba_1MtIhL2eZvKYlo2CAElKwKu2"))
    (null (slot-value payout 'stripe::%failure-balance-transaction))
    (null (slot-value payout 'stripe::%failure-code))
    (null (slot-value payout 'stripe::%failure-message))
    (is (string= (stripe:payout-method payout) "standard"))
    (is (string= (slot-value payout 'stripe::%source-type) "card"))
    (null (slot-value payout 'stripe::%statement-descriptor))
    (is (string= (slot-value payout 'stripe::%status) "pending"))
    (is (string= (stripe:payout-type payout) "bank_account"))))

(defparameter *list-payouts-json*
  "{
     \"object\": \"list\",
     \"url\": \"/v1/payouts\",
     \"has_more\": false,
     \"data\": [
       {
         \"id\": \"po_1OaFDbEcg9tTZuTgNYmX0PKB\",
         \"object\": \"payout\",
         \"amount\": 1100,
         \"arrival_date\": 1680652800,
         \"automatic\": false,
         \"balance_transaction\": \"txn_1OaFDcEcg9tTZuTgYMR25tSe\",
         \"created\": 1680648691,
         \"currency\": \"usd\",
         \"description\": null,
         \"destination\": \"ba_1MtIhL2eZvKYlo2CAElKwKu2\",
         \"failure_balance_transaction\": null,
         \"failure_code\": null,
         \"failure_message\": null,
         \"livemode\": false,
         \"metadata\": {},
         \"method\": \"standard\",
         \"original_payout\": null,
         \"reconciliation_status\": \"not_applicable\",
         \"reversed_by\": null,
         \"source_type\": \"card\",
         \"statement_descriptor\": null,
         \"status\": \"pending\",
         \"type\": \"bank_account\"
       }
     ]
   }"
  "https://docs.stripe.com/api/payouts/list")

(define-mock-query mock-list-payouts (:type vector)
  *list-payouts-json*)

(test list-payouts
  (let* ((payouts (mock-list-payouts))
         (payout (aref payouts 0)))
    (is (vectorp payouts))
    (is (typep payout 'stripe::payout))
    (is (string= (slot-value payout 'stripe::%id) "po_1OaFDbEcg9tTZuTgNYmX0PKB"))
    (is (= (slot-value payout 'stripe::%amount) 1100))
    (is (local-time:timestamp= (slot-value payout 'stripe::%arrival-date)
                               (stripe::decode-timestamp 1680652800)))
    (is-false (slot-value payout 'stripe::%automatic))
    (is (string= (slot-value payout 'stripe::%balance-transaction) "txn_1OaFDcEcg9tTZuTgYMR25tSe"))
    (is (local-time:timestamp= (slot-value payout 'stripe::%created)
                               (stripe::decode-timestamp 1680648691)))
    (is (string= (slot-value payout 'stripe::%currency) "usd"))
    (null (slot-value payout 'stripe::%description))
    (is (string= (slot-value payout 'stripe::%destination) "ba_1MtIhL2eZvKYlo2CAElKwKu2"))
    (null (slot-value payout 'stripe::%failure-balance-transaction))
    (null (slot-value payout 'stripe::%failure-code))
    (null (slot-value payout 'stripe::%failure-message))
    (is (string= (stripe:payout-method payout) "standard"))
    (is (string= (slot-value payout 'stripe::%source-type) "card"))
    (null (slot-value payout 'stripe::%statement-descriptor))
    (is (string= (slot-value payout 'stripe::%status) "pending"))
    (is (string= (stripe:payout-type payout) "bank_account"))))
