(in-package #:stripe/tests)

(def-suite subscription-item-tests)

(in-suite subscription-item-tests)

(defparameter *create-subscription-item-json*
  "{
     \"id\": \"si_NcLYdDxLHxlFo7\",
     \"object\": \"subscription_item\",
     \"billing_thresholds\": null,
     \"created\": 1680126546,
     \"metadata\": {},
     \"price\": {
       \"id\": \"price_1Mr6rdLkdIwHu7ixwPmiybbR\",
       \"object\": \"price\",
       \"active\": true,
       \"billing_scheme\": \"per_unit\",
       \"created\": 1680126545,
       \"currency\": \"usd\",
       \"custom_unit_amount\": null,
       \"discounts\": null,
       \"livemode\": false,
       \"lookup_key\": null,
       \"metadata\": {},
       \"nickname\": null,
       \"product\": \"prod_NcLYGKH0eY5b8s\",
       \"recurring\": {
         \"aggregate_usage\": null,
         \"interval\": \"month\",
         \"interval_count\": 1,
         \"trial_period_days\": null,
         \"usage_type\": \"licensed\"
       },
       \"tax_behavior\": \"unspecified\",
       \"tiers_mode\": null,
       \"transform_quantity\": null,
       \"type\": \"recurring\",
       \"unit_amount\": 1000,
       \"unit_amount_decimal\": \"1000\"
     },
     \"quantity\": 2,
     \"subscription\": \"sub_1Mr6rbLkdIwHu7ix4Xm9Ahtd\",
     \"tax_rates\": []
   }"
  "https://docs.stripe.com/api/subscription_items/create")

(define-mock-query mock-create-subscription-item (:type stripe:subscription-item)
  *create-subscription-item-json*)

(test create-subscription-item
  (let ((subscription-item (mock-create-subscription-item)))
    (is (typep subscription-item 'stripe:subscription-item))
    (is (string= (slot-value subscription-item 'stripe::%id) "si_NcLYdDxLHxlFo7"))
    (is (local-time:timestamp= (slot-value subscription-item 'stripe::%created)
                               (stripe::decode-timestamp 1680126546)))
    (is (= (slot-value subscription-item 'stripe::%quantity) 2))
    (is (string= (slot-value subscription-item 'stripe::%subscription) "sub_1Mr6rbLkdIwHu7ix4Xm9Ahtd"))))

(defparameter *list-subscription-items-json*
  "{
     \"object\": \"list\",
     \"url\": \"/v1/subscription_items\",
     \"has_more\": false,
     \"data\": [
       {
         \"id\": \"si_OCgWsGlqpbN4EP\",
         \"object\": \"subscription_item\",
         \"billing_thresholds\": null,
         \"created\": 1688507587,
         \"metadata\": {},
         \"price\": {
           \"id\": \"price_1NQH9iLkdIwHu7ix3tkaSxhj\",
           \"object\": \"price\",
           \"active\": true,
           \"billing_scheme\": \"per_unit\",
           \"created\": 1688507586,
           \"currency\": \"usd\",
           \"custom_unit_amount\": null,
           \"livemode\": false,
           \"lookup_key\": null,
           \"metadata\": {},
           \"nickname\": null,
           \"product\": \"prod_OCgWE6cbwiSu27\",
           \"recurring\": {
             \"aggregate_usage\": null,
             \"interval\": \"month\",
             \"interval_count\": 1,
             \"trial_period_days\": null,
             \"usage_type\": \"licensed\"
           },
           \"tax_behavior\": \"unspecified\",
           \"tiers_mode\": null,
           \"transform_quantity\": null,
           \"type\": \"recurring\",
           \"unit_amount\": 1000,
           \"unit_amount_decimal\": \"1000\"
         },
         \"quantity\": 1,
         \"subscription\": \"sub_1NQH9iLkdIwHu7ixxhHui9yi\",
         \"tax_rates\": []
       }
     ]
   }"
  "https://docs.stripe.com/api/subscription_items/list")

(define-mock-query mock-list-subscription-items (:type vector)
  *list-subscription-items-json*)

(test list-subscription-items
  (let* ((subscription-items (mock-list-subscription-items))
         (subscription-item (aref subscription-items 0)))
    (is (vectorp subscription-items))
    (is (typep subscription-item 'stripe:subscription-item))
    (is (string= (slot-value subscription-item 'stripe::%id) "si_OCgWsGlqpbN4EP"))
    (is (local-time:timestamp= (slot-value subscription-item 'stripe::%created)
                               (stripe::decode-timestamp 1688507587)))
    (is (= (slot-value subscription-item 'stripe::%quantity) 1))
    (is (string= (slot-value subscription-item 'stripe::%subscription) "sub_1NQH9iLkdIwHu7ixxhHui9yi"))))
