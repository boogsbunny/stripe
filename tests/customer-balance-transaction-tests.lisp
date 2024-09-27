(in-package #:stripe/tests)

(def-suite customer-balance-transaction-tests)

(in-suite customer-balance-transaction-tests)

(defparameter *create-customer-balance-transaction-json*
  "{
     \"id\": \"cbtxn_1MrU9qLkdIwHu7ixhdjxGBgI\",
     \"object\": \"customer_balance_transaction\",
     \"amount\": -500,
     \"created\": 1680216086,
     \"credit_note\": null,
     \"currency\": \"usd\",
     \"customer\": \"cus_NcjdgdwZyI9Rj7\",
     \"description\": null,
     \"ending_balance\": -500,
     \"invoice\": null,
     \"livemode\": false,
     \"metadata\": {},
     \"type\": \"adjustment\"
   }"
  "https://docs.stripe.com/api/customer_balance_transactions/create")

(define-mock-query mock-create-customer-balance-transaction
    (:type stripe::customer-balance-transaction)
  *create-customer-balance-transaction-json*)

(test create-customer-balance-transaction
  (let ((customer-balance-transaction (mock-create-customer-balance-transaction)))
    (is (typep customer-balance-transaction 'stripe::customer-balance-transaction))
    (is (string= (slot-value customer-balance-transaction 'stripe::%id)
                 "cbtxn_1MrU9qLkdIwHu7ixhdjxGBgI"))
    (is (= (slot-value customer-balance-transaction 'stripe::%amount) -500))
    (is (local-time:timestamp= (slot-value customer-balance-transaction 'stripe::%created)
                               (stripe::decode-timestamp 1680216086)))
    (is (null (slot-value customer-balance-transaction 'stripe::%credit-note)))
    (is (string= (slot-value customer-balance-transaction 'stripe::%currency) "usd"))
    (is (string= (slot-value customer-balance-transaction 'stripe::%customer) "cus_NcjdgdwZyI9Rj7"))
    (is (null (slot-value customer-balance-transaction 'stripe::%description)))
    (is (= (slot-value customer-balance-transaction 'stripe::%ending-balance) -500))
    (is (null (slot-value customer-balance-transaction 'stripe::%invoice)))
    (is (string= (stripe:transaction-type customer-balance-transaction) "adjustment"))))

(defparameter *list-customer-balance-transactions-json*
  "{
     \"object\": \"list\",
     \"url\": \"/v1/customers/cus_NcjdgdwZyI9Rj7/balance_transactions\",
     \"has_more\": false,
     \"data\": [
       {
         \"id\": \"cbtxn_1MrU9qLkdIwHu7ixhdjxGBgI\",
         \"object\": \"customer_balance_transaction\",
         \"amount\": -500,
         \"created\": 1680216086,
         \"credit_note\": null,
         \"currency\": \"usd\",
         \"customer\": \"cus_NcjdgdwZyI9Rj7\",
         \"description\": null,
         \"ending_balance\": -500,
         \"invoice\": null,
         \"livemode\": false,
         \"metadata\": {},
         \"type\": \"adjustment\"
       }
     ]
   }"
   "https://docs.stripe.com/api/customer_balance_transactions/list")

(define-mock-query mock-list-customer-balance-transactions (:type vector)
  *list-customer-balance-transactions-json*)

(test list-customer-balance-transactions
  (let* ((customer-balance-transactions (mock-list-customer-balance-transactions))
         (customer-balance-transaction (aref customer-balance-transactions 0)))
    (is (vectorp customer-balance-transactions))
    (is (typep customer-balance-transaction 'stripe::customer-balance-transaction))
    (is (string= (slot-value customer-balance-transaction 'stripe::%id)
                 "cbtxn_1MrU9qLkdIwHu7ixhdjxGBgI"))
    (is (= (slot-value customer-balance-transaction 'stripe::%amount) -500))
    (is (local-time:timestamp= (slot-value customer-balance-transaction 'stripe::%created)
                               (stripe::decode-timestamp 1680216086)))
    (is (null (slot-value customer-balance-transaction 'stripe::%credit-note)))
    (is (string= (slot-value customer-balance-transaction 'stripe::%currency) "usd"))
    (is (string= (slot-value customer-balance-transaction 'stripe::%customer) "cus_NcjdgdwZyI9Rj7"))
    (is (null (slot-value customer-balance-transaction 'stripe::%description)))
    (is (= (slot-value customer-balance-transaction 'stripe::%ending-balance) -500))
    (is (null (slot-value customer-balance-transaction 'stripe::%invoice)))
    (is (string= (stripe:transaction-type customer-balance-transaction) "adjustment"))))
