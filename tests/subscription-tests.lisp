(in-package #:stripe/tests)

(def-suite subscription-tests)

(in-suite subscription-tests)

(defparameter *create-subscription-json*
  "{
     \"id\": \"sub_1MowQVLkdIwHu7ixeRlqHVzs\",
     \"object\": \"subscription\",
     \"application\": null,
     \"application_fee_percent\": null,
     \"automatic_tax\": {
       \"enabled\": false,
       \"liability\": null
     },
     \"billing_cycle_anchor\": 1679609767,
     \"billing_thresholds\": null,
     \"cancel_at\": null,
     \"cancel_at_period_end\": false,
     \"canceled_at\": null,
     \"cancellation_details\": {
       \"comment\": null,
       \"feedback\": null,
       \"reason\": null
     },
     \"collection_method\": \"charge_automatically\",
     \"created\": 1679609767,
     \"currency\": \"usd\",
     \"current_period_end\": 1682288167,
     \"current_period_start\": 1679609767,
     \"customer\": \"cus_Na6dX7aXxi11N4\",
     \"days_until_due\": null,
     \"default_payment_method\": null,
     \"default_source\": null,
     \"default_tax_rates\": [],
     \"description\": null,
     \"discount\": null,
     \"discounts\": null,
     \"ended_at\": null,
     \"invoice_settings\": {
       \"issuer\": {
         \"type\": \"self\"
       }
     },
     \"items\": {
       \"object\": \"list\",
       \"data\": [
         {
           \"id\": \"si_Na6dzxczY5fwHx\",
           \"object\": \"subscription_item\",
           \"billing_thresholds\": null,
           \"created\": 1679609768,
           \"metadata\": {},
           \"plan\": {
             \"id\": \"price_1MowQULkdIwHu7ixraBm864M\",
             \"object\": \"plan\",
             \"active\": true,
             \"aggregate_usage\": null,
             \"amount\": 1000,
             \"amount_decimal\": \"1000\",
             \"billing_scheme\": \"per_unit\",
             \"created\": 1679609766,
             \"currency\": \"usd\",
             \"discounts\": null,
             \"interval\": \"month\",
             \"interval_count\": 1,
             \"livemode\": false,
             \"metadata\": {},
             \"nickname\": null,
             \"product\": \"prod_Na6dGcTsmU0I4R\",
             \"tiers_mode\": null,
             \"transform_usage\": null,
             \"trial_period_days\": null,
             \"usage_type\": \"licensed\"
           },
           \"price\": {
             \"id\": \"price_1MowQULkdIwHu7ixraBm864M\",
             \"object\": \"price\",
             \"active\": true,
             \"billing_scheme\": \"per_unit\",
             \"created\": 1679609766,
             \"currency\": \"usd\",
             \"custom_unit_amount\": null,
             \"livemode\": false,
             \"lookup_key\": null,
             \"metadata\": {},
             \"nickname\": null,
             \"product\": \"prod_Na6dGcTsmU0I4R\",
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
           \"subscription\": \"sub_1MowQVLkdIwHu7ixeRlqHVzs\",
           \"tax_rates\": []
         }
       ],
       \"has_more\": false,
       \"total_count\": 1,
       \"url\": \"/v1/subscription_items?subscription=sub_1MowQVLkdIwHu7ixeRlqHVzs\"
     },
     \"latest_invoice\": \"in_1MowQWLkdIwHu7ixuzkSPfKd\",
     \"livemode\": false,
     \"metadata\": {},
     \"next_pending_invoice_item_invoice\": null,
     \"on_behalf_of\": null,
     \"pause_collection\": null,
     \"payment_settings\": {
       \"payment_method_options\": null,
       \"payment_method_types\": null,
       \"save_default_payment_method\": \"off\"
     },
     \"pending_invoice_item_interval\": null,
     \"pending_setup_intent\": null,
     \"pending_update\": null,
     \"schedule\": null,
     \"start_date\": 1679609767,
     \"status\": \"active\",
     \"test_clock\": null,
     \"transfer_data\": null,
     \"trial_end\": null,
     \"trial_settings\": {
       \"end_behavior\": {
         \"missing_payment_method\": \"create_invoice\"
       }
     },
     \"trial_start\": null
   }"
  "https://docs.stripe.com/api/subscriptions/create")

(define-mock-query mock-create-subscription (:type stripe:subscription)
  *create-subscription-json*)

(test create-subscription
  (let ((subscription (mock-create-subscription)))
    (is (typep subscription 'stripe:subscription))
    (is (string= (slot-value subscription 'stripe::%id) "sub_1MowQVLkdIwHu7ixeRlqHVzs"))
    (is (local-time:timestamp= (slot-value subscription 'stripe::%billing-cycle-anchor)
                               (stripe::decode-timestamp 1679609767)))
    (is (null (slot-value subscription 'stripe::%cancel-at)))
    (is (not (slot-value subscription 'stripe::%cancel-at-period-end)))
    (is (null (slot-value subscription 'stripe::%canceled-at)))
    (is (string= (slot-value subscription 'stripe::%collection-method) "charge_automatically"))
    (is (local-time:timestamp= (slot-value subscription 'stripe::%created)
                               (stripe::decode-timestamp 1679609767)))
    (is (local-time:timestamp= (slot-value subscription 'stripe::%current-period-end)
                               (stripe::decode-timestamp 1682288167)))
    (is (local-time:timestamp= (slot-value subscription 'stripe::%current-period-start)
                               (stripe::decode-timestamp 1679609767)))
    (is (string= (slot-value subscription 'stripe::%customer) "cus_Na6dX7aXxi11N4"))
    (is (null (slot-value subscription 'stripe::%default-payment-method)))
    (is (null (slot-value subscription 'stripe::%discount)))
    (is (null (slot-value subscription 'stripe::%ended-at)))
    (is (string= (slot-value subscription 'stripe::%latest-invoice) "in_1MowQWLkdIwHu7ixuzkSPfKd"))
    (is (local-time:timestamp= (slot-value subscription 'stripe::%start-date)
                               (stripe::decode-timestamp 1679609767)))
    (is (string= (slot-value subscription 'stripe::%status) "active"))
    (is (null (slot-value subscription 'stripe::%trial-end)))
    (is (null (slot-value subscription 'stripe::%trial-start)))))

(defparameter *list-subscriptions-json*
  "{
     \"object\": \"list\",
     \"url\": \"/v1/subscriptions\",
     \"has_more\": false,
     \"data\": [
       {
         \"id\": \"sub_1MowQVLkdIwHu7ixeRlqHVzs\",
         \"object\": \"subscription\",
         \"application\": null,
         \"application_fee_percent\": null,
         \"automatic_tax\": {
           \"enabled\": false,
           \"liability\": null
         },
         \"billing_cycle_anchor\": 1679609767,
         \"billing_thresholds\": null,
         \"cancel_at\": null,
         \"cancel_at_period_end\": false,
         \"canceled_at\": null,
         \"cancellation_details\": {
           \"comment\": null,
           \"feedback\": null,
           \"reason\": null
         },
         \"collection_method\": \"charge_automatically\",
         \"created\": 1679609767,
         \"currency\": \"usd\",
         \"current_period_end\": 1682288167,
         \"current_period_start\": 1679609767,
         \"customer\": \"cus_Na6dX7aXxi11N4\",
         \"days_until_due\": null,
         \"default_payment_method\": null,
         \"default_source\": null,
         \"default_tax_rates\": [],
         \"description\": null,
         \"discount\": null,
         \"discounts\": null,
         \"ended_at\": null,
         \"invoice_settings\": {
           \"issuer\": {
             \"type\": \"self\"
           }
         },
         \"items\": {
           \"object\": \"list\",
           \"data\": [
             {
               \"id\": \"si_Na6dzxczY5fwHx\",
               \"object\": \"subscription_item\",
               \"billing_thresholds\": null,
               \"created\": 1679609768,
               \"metadata\": {},
               \"plan\": {
                 \"id\": \"price_1MowQULkdIwHu7ixraBm864M\",
                 \"object\": \"plan\",
                 \"active\": true,
                 \"aggregate_usage\": null,
                 \"amount\": 1000,
                 \"amount_decimal\": \"1000\",
                 \"billing_scheme\": \"per_unit\",
                 \"created\": 1679609766,
                 \"currency\": \"usd\",
                 \"discounts\": null,
                 \"interval\": \"month\",
                 \"interval_count\": 1,
                 \"livemode\": false,
                 \"metadata\": {},
                 \"nickname\": null,
                 \"product\": \"prod_Na6dGcTsmU0I4R\",
                 \"tiers_mode\": null,
                 \"transform_usage\": null,
                 \"trial_period_days\": null,
                 \"usage_type\": \"licensed\"
               },
               \"price\": {
                 \"id\": \"price_1MowQULkdIwHu7ixraBm864M\",
                 \"object\": \"price\",
                 \"active\": true,
                 \"billing_scheme\": \"per_unit\",
                 \"created\": 1679609766,
                 \"currency\": \"usd\",
                 \"custom_unit_amount\": null,
                 \"livemode\": false,
                 \"lookup_key\": null,
                 \"metadata\": {},
                 \"nickname\": null,
                 \"product\": \"prod_Na6dGcTsmU0I4R\",
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
               \"subscription\": \"sub_1MowQVLkdIwHu7ixeRlqHVzs\",
               \"tax_rates\": []
             }
           ],
           \"has_more\": false,
           \"total_count\": 1,
           \"url\": \"/v1/subscription_items?subscription=sub_1MowQVLkdIwHu7ixeRlqHVzs\"
         },
         \"latest_invoice\": \"in_1MowQWLkdIwHu7ixuzkSPfKd\",
         \"livemode\": false,
         \"metadata\": {},
         \"next_pending_invoice_item_invoice\": null,
         \"on_behalf_of\": null,
         \"pause_collection\": null,
         \"payment_settings\": {
           \"payment_method_options\": null,
           \"payment_method_types\": null,
           \"save_default_payment_method\": \"off\"
         },
         \"pending_invoice_item_interval\": null,
         \"pending_setup_intent\": null,
         \"pending_update\": null,
         \"schedule\": null,
         \"start_date\": 1679609767,
         \"status\": \"active\",
         \"test_clock\": null,
         \"transfer_data\": null,
         \"trial_end\": null,
         \"trial_settings\": {
           \"end_behavior\": {
             \"missing_payment_method\": \"create_invoice\"
           }
         },
         \"trial_start\": null
       }
     ]
   }"
  "https://docs.stripe.com/api/subscriptions/list")

(define-mock-query mock-list-subscriptions (:type vector)
  *list-subscriptions-json*)

(test list-subscriptions
 (let* ((subscriptions (mock-list-subscriptions))
        (subscription (aref subscriptions 0)))
   (is (vectorp subscriptions))
   (is (typep subscription 'stripe:subscription))
   (is (string= (slot-value subscription 'stripe::%id) "sub_1MowQVLkdIwHu7ixeRlqHVzs"))
   (is (local-time:timestamp= (slot-value subscription 'stripe::%billing-cycle-anchor)
                              (stripe::decode-timestamp 1679609767)))
   (is (null (slot-value subscription 'stripe::%cancel-at)))
   (is (not (slot-value subscription 'stripe::%cancel-at-period-end)))
   (is (null (slot-value subscription 'stripe::%canceled-at)))
   (is (string= (slot-value subscription 'stripe::%collection-method) "charge_automatically"))
   (is (local-time:timestamp= (slot-value subscription 'stripe::%created)
                              (stripe::decode-timestamp 1679609767)))
   (is (local-time:timestamp= (slot-value subscription 'stripe::%current-period-end)
                              (stripe::decode-timestamp 1682288167)))
   (is (string= (slot-value subscription 'stripe::%customer) "cus_Na6dX7aXxi11N4"))
   (is (null (slot-value subscription 'stripe::%default-payment-method)))
   (is (null (slot-value subscription 'stripe::%discount)))
   (is (null (slot-value subscription 'stripe::%ended-at)))
   (is (vectorp (slot-value subscription 'stripe::%items)))
   (is (string= (slot-value subscription 'stripe::%latest-invoice) "in_1MowQWLkdIwHu7ixuzkSPfKd"))
   (is (local-time:timestamp= (slot-value subscription 'stripe::%start-date)
                              (stripe::decode-timestamp 1679609767)))
   (is (string= (slot-value subscription 'stripe::%status) "active"))
   (is (null (slot-value subscription 'stripe::%trial-end)))
   (is (null (slot-value subscription 'stripe::%trial-start)))))
