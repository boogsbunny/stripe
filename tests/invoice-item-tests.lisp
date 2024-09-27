(in-package #:stripe/tests)

(def-suite invoice-item-tests)

(in-suite invoice-item-tests)

(defparameter *create-invoice-item-json*
  "{
     \"id\": \"ii_1MtGUtLkdIwHu7ixBYwjAM00\",
     \"object\": \"invoiceitem\",
     \"amount\": 1099,
     \"currency\": \"usd\",
     \"customer\": \"cus_NeZei8imSbMVvi\",
     \"date\": 1680640231,
     \"description\": \"T-shirt\",
     \"discountable\": true,
     \"discounts\": [],
     \"invoice\": null,
     \"livemode\": false,
     \"metadata\": {},
     \"period\": {
       \"end\": 1680640231,
       \"start\": 1680640231
     },
     \"plan\": null,
     \"price\": {
       \"id\": \"price_1MtGUsLkdIwHu7ix1be5Ljaj\",
       \"object\": \"price\",
       \"active\": true,
       \"billing_scheme\": \"per_unit\",
       \"created\": 1680640229,
       \"currency\": \"usd\",
       \"custom_unit_amount\": null,
       \"livemode\": false,
       \"lookup_key\": null,
       \"metadata\": {},
       \"nickname\": null,
       \"product\": \"prod_NeZe7xbBdJT8EN\",
       \"recurring\": null,
       \"tax_behavior\": \"unspecified\",
       \"tiers_mode\": null,
       \"transform_quantity\": null,
       \"type\": \"one_time\",
       \"unit_amount\": 1099,
       \"unit_amount_decimal\": \"1099\"
     },
     \"proration\": false,
     \"quantity\": 1,
     \"subscription\": null,
     \"tax_rates\": [],
     \"test_clock\": null,
     \"unit_amount\": 1099,
     \"unit_amount_decimal\": \"1099\"
   }"
  "https://docs.stripe.com/api/invoiceitems/create")

(define-mock-query mock-create-invoice-item (:type stripe:invoice-item)
  *create-invoice-item-json*)

(test create-invoice-item
  (let ((invoice-item (mock-create-invoice-item)))
    (is (typep invoice-item 'stripe:invoice-item))
    (is (string= (slot-value invoice-item 'stripe::%id) "ii_1MtGUtLkdIwHu7ixBYwjAM00"))
    (is (= (slot-value invoice-item 'stripe::%amount) 1099))
    (is (string= (slot-value invoice-item 'stripe::%currency) "usd"))
    (is (string= (slot-value invoice-item 'stripe::%customer) "cus_NeZei8imSbMVvi"))
    (is (local-time:timestamp= (slot-value invoice-item 'stripe::%date)
                               (stripe::decode-timestamp 1680640231)))
    (is (string= (slot-value invoice-item 'stripe::%description) "T-shirt"))
    (is-true (slot-value invoice-item 'stripe::%discountable))
    (null (slot-value invoice-item 'stripe::%invoice))
    (is (local-time:timestamp= (slot-value invoice-item 'stripe::%period-end)
                               (stripe::decode-timestamp 1680640231)))
    (is (local-time:timestamp= (slot-value invoice-item 'stripe::%period-start)
                               (stripe::decode-timestamp 1680640231)))
    (null (slot-value invoice-item 'stripe::%plan))
    (is-false (slot-value invoice-item 'stripe::%proration))
    (is (= (slot-value invoice-item 'stripe::%quantity) 1))
    (null (slot-value invoice-item 'stripe::%subscription))
    (is (= (slot-value invoice-item 'stripe::%unit-amount) 1099))))

(defparameter *list-invoice-items-json*
  "{
     \"object\": \"list\",
     \"url\": \"/v1/invoiceitems\",
     \"has_more\": false,
     \"data\": [
       {
         \"id\": \"ii_1MtGUtLkdIwHu7ixBYwjAM00\",
         \"object\": \"invoiceitem\",
         \"amount\": 1099,
         \"currency\": \"usd\",
         \"customer\": \"cus_NeZei8imSbMVvi\",
         \"date\": 1680640231,
         \"description\": \"T-shirt\",
         \"discountable\": true,
         \"discounts\": [],
         \"invoice\": null,
         \"livemode\": false,
         \"metadata\": {},
         \"period\": {
           \"end\": 1680640231,
           \"start\": 1680640231
         },
         \"plan\": null,
         \"price\": {
           \"id\": \"price_1MtGUsLkdIwHu7ix1be5Ljaj\",
           \"object\": \"price\",
           \"active\": true,
           \"billing_scheme\": \"per_unit\",
           \"created\": 1680640229,
           \"currency\": \"usd\",
           \"custom_unit_amount\": null,
           \"livemode\": false,
           \"lookup_key\": null,
           \"metadata\": {},
           \"nickname\": null,
           \"product\": \"prod_NeZe7xbBdJT8EN\",
           \"recurring\": null,
           \"tax_behavior\": \"unspecified\",
           \"tiers_mode\": null,
           \"transform_quantity\": null,
           \"type\": \"one_time\",
           \"unit_amount\": 1099,
           \"unit_amount_decimal\": \"1099\"
         },
         \"proration\": false,
         \"quantity\": 1,
         \"subscription\": null,
         \"tax_rates\": [],
         \"test_clock\": null,
         \"unit_amount\": 1099,
         \"unit_amount_decimal\": \"1099\"
       }
     ]
   }"
  "https://docs.stripe.com/api/invoiceitems/list")

(define-mock-query mock-list-invoice-items (:type vector)
  *list-invoice-items-json*)

(test list-invoice-items ()
  (let* ((invoice-items (mock-list-invoice-items))
         (invoice-item (aref invoice-items 0)))
    (is (vectorp invoice-items))
    (is (typep invoice-item 'stripe:invoice-item))
    (is (string= (slot-value invoice-item 'stripe::%id) "ii_1MtGUtLkdIwHu7ixBYwjAM00"))
    (is (= (slot-value invoice-item 'stripe::%amount) 1099))
    (is (string= (slot-value invoice-item 'stripe::%currency) "usd"))
    (is (string= (slot-value invoice-item 'stripe::%customer) "cus_NeZei8imSbMVvi"))
    (is (local-time:timestamp= (slot-value invoice-item 'stripe::%date)
                               (stripe::decode-timestamp 1680640231)))
    (is (string= (slot-value invoice-item 'stripe::%description) "T-shirt"))
    (is-true (slot-value invoice-item 'stripe::%discountable))
    (null (slot-value invoice-item 'stripe::%invoice))
    (is (local-time:timestamp= (slot-value invoice-item 'stripe::%period-end)
                               (stripe::decode-timestamp 1680640231)))
    (is (local-time:timestamp= (slot-value invoice-item 'stripe::%period-start)
                               (stripe::decode-timestamp 1680640231)))
    (null (slot-value invoice-item 'stripe::%plan))
    (is-false (slot-value invoice-item 'stripe::%proration))
    (is (= (slot-value invoice-item 'stripe::%quantity) 1))
    (null (slot-value invoice-item 'stripe::%subscription))
    (is (= (slot-value invoice-item 'stripe::%unit-amount) 1099))))
