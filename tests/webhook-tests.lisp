(in-package #:stripe/tests)

(def-suite webhook-tests)

(in-suite webhook-tests)

(defparameter *webhook-event-json*
  "{
    \"id\": \"evt_1MqqbKLt4dXK03v5qaIbiNCC\",
    \"object\": \"event\",
    \"api_version\": \"2024-06-20\",
    \"created\": 1680064028,
    \"type\": \"customer.subscription.updated\",
    \"data\": {
      \"object\": {
        \"id\": \"sub_1Mqqb6Lt4dXK03v50OA219Ya\",
        \"object\": \"subscription\",
        \"application\": null,
        \"application_fee_percent\": null,
        \"automatic_tax\": {
          \"enabled\": false
        },
        \"billing_cycle_anchor\": 1680668814,
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
        \"created\": 1680064014,
        \"currency\": \"usd\",
        \"current_period_end\": 1683260814,
        \"current_period_start\": 1680668814,
        \"customer\": \"cus_Nc4kL4EPtG5SKe\",
        \"days_until_due\": null,
        \"default_payment_method\": null,
        \"default_source\": null,
        \"default_tax_rates\": [],
        \"description\": \"A test subscription\",
        \"discount\": null,
        \"ended_at\": null,
        \"invoice_customer_balance_settings\": {
          \"consume_applied_balance_on_void\": true
        },
        \"items\": {
          \"object\": \"list\",
          \"data\": [
            {
              \"id\": \"si_Nc4kEcMHd3vRTS\",
              \"object\": \"subscription_item\",
              \"billing_thresholds\": null,
              \"created\": 1680064014,
              \"metadata\": {},
              \"plan\": {
                \"id\": \"price_1Mqqb5Lt4dXK03v5cK9prani\",
                \"object\": \"plan\",
                \"active\": true,
                \"aggregate_usage\": null,
                \"amount\": 4242,
                \"amount_decimal\": \"4242\",
                \"billing_scheme\": \"per_unit\",
                \"created\": 1680064015,
                \"currency\": \"usd\",
                \"interval\": \"month\",
                \"interval_count\": 1,
                \"livemode\": false,
                \"metadata\": {},
                \"nickname\": null,
                \"product\": \"prod_Nc4kjj2XYpywZV\",
                \"tiers\": null,
                \"tiers_mode\": null,
                \"transform_usage\": null,
                \"trial_period_days\": null,
                \"usage_type\": \"licensed\"
              },
              \"price\": {
                \"id\": \"price_1Mqqb5Lt4dXK03v5cK9prani\",
                \"object\": \"price\",
                \"active\": true,
                \"billing_scheme\": \"per_unit\",
                \"created\": 1680064015,
                \"currency\": \"usd\",
                \"custom_unit_amount\": null,
                \"livemode\": false,
                \"lookup_key\": null,
                \"metadata\": {},
                \"migrate_to\": null,
                \"nickname\": null,
                \"product\": \"prod_Nc4kjj2XYpywZV\",
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
                \"unit_amount\": 4242,
                \"unit_amount_decimal\": \"4242\"
              },
              \"quantity\": 1,
              \"subscription\": \"sub_1Mqqb6Lt4dXK03v50OA219Ya\",
              \"tax_rates\": []
            }
          ],
          \"has_more\": false,
          \"total_count\": 1,
          \"url\": \"/v1/subscription_items?subscription=sub_1Mqqb6Lt4dXK03v50OA219Ya\"
        },
        \"latest_invoice\": \"in_1MqqbILt4dXK03v5cbbciqFZ\",
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
        \"plan\": {
          \"id\": \"price_1Mqqb5Lt4dXK03v5cK9prani\",
          \"object\": \"plan\",
          \"active\": true,
          \"aggregate_usage\": null,
          \"amount\": 4242,
          \"amount_decimal\": \"4242\",
          \"billing_scheme\": \"per_unit\",
          \"created\": 1680064015,
          \"currency\": \"usd\",
          \"interval\": \"month\",
          \"interval_count\": 1,
          \"livemode\": false,
          \"metadata\": {},
          \"nickname\": null,
          \"product\": \"prod_Nc4kjj2XYpywZV\",
          \"tiers\": null,
          \"tiers_mode\": null,
          \"transform_usage\": null,
          \"trial_period_days\": null,
          \"usage_type\": \"licensed\"
        },
        \"quantity\": 1,
        \"schedule\": null,
        \"start_date\": 1680064014,
        \"status\": \"active\",
        \"tax_percent\": null,
        \"test_clock\": \"clock_1Mqqb4Lt4dXK03v5NOFiPg4R\",
        \"transfer_data\": null,
        \"trial_end\": 1680668814,
        \"trial_settings\": {
          \"end_behavior\": {
            \"missing_payment_method\": \"create_invoice\"
          }
        },
        \"trial_start\": 1680064014
      },
      \"previous_attributes\": {
        \"current_period_end\": 1680668814,
        \"current_period_start\": 1680064014,
        \"latest_invoice\": \"in_1Mqqb6Lt4dXK03v5Xn79tY8i\",
        \"status\": \"trialing\"
      }
    },
    \"livemode\": false,
    \"pending_webhooks\": 1,
    \"request\": {
      \"id\": null,
      \"idempotency_key\": null
    }
  }"
  "https://docs.stripe.com/api/events/object")

(defparameter *webhook-secret** "whsec_test_secret")

(defparameter *webhook-headers**
  "t=1614294653,v1=5257a869e7ecebeda32affa62cdca3fa51cad7e77a0e56ff536d0ce8e108d8bd,v1=6ffbb59b2300aae63f272406069a9788598b792a944a07aba816edb039989a39")

(defun generate-header (timestamp signature &optional (scheme "v1"))
  (format nil "t=~A,~A=~A"
          (local-time:timestamp-to-unix timestamp)
          scheme
          (ironclad:byte-array-to-hex-string signature)))

(test validate-webhook-payload-valid
  (let* ((timestamp (local-time:now))
         (signature (stripe::compute-signature
                     timestamp
                     (dexador.util:ascii-string-to-octets *webhook-event-json*)
                     *webhook-secret**))
         (header (generate-header timestamp signature)))
    (is (stripe:validate-webhook-payload (dexador.util:ascii-string-to-octets *webhook-event-json*)
                                         header
                                         *webhook-secret**))))

(test validate-webhook-payload-invalid-signature
  (let* ((timestamp (local-time:now))
         (signature (stripe::compute-signature
                     timestamp
                     (dexador.util:ascii-string-to-octets *webhook-event-json*)
                     "false-secret"))
         (header (generate-header timestamp signature)))
    (is (not (stripe:validate-webhook-payload
              (dexador.util:ascii-string-to-octets *webhook-event-json*)
              header
              *webhook-secret**)))))

(test validate-webhook-payload-timestamp-too-old
  (let* ((timestamp (local-time:now))
         (signature (stripe::compute-signature
                     timestamp
                     (dexador.util:ascii-string-to-octets *webhook-event-json*)
                     *webhook-secret**))
         (header (generate-header timestamp signature)))
    (signals stripe:webhook-timestamp-too-old
      (stripe:validate-webhook-payload (dexador.util:ascii-string-to-octets *webhook-event-json*)
                                       header
                                       *webhook-secret**
                                       :tolerance -1))))

(test construct-webhook-event-valid
  (let* ((timestamp (local-time:now))
         (signature (stripe::compute-signature
                     timestamp
                     (dexador.util:ascii-string-to-octets *webhook-event-json*)
                     *webhook-secret**))
         (header (generate-header timestamp signature))
         (event (stripe:construct-webhook-event
                 (dexador.util:ascii-string-to-octets *webhook-event-json*)
                 header
                 *webhook-secret**
                 :ignore-api-version-mismatch t)))
    (is (typep event 'stripe:webhook-event))
    (is (string= (slot-value event 'stripe::%id) "evt_1MqqbKLt4dXK03v5qaIbiNCC"))
    (is (string= (slot-value event 'stripe::%object) "event"))
    (is (string= (slot-value event 'stripe::%api-version) "2024-06-20"))
    (is (local-time:timestamp= (slot-value event 'stripe::%created)
                               (stripe::decode-timestamp 1680064028)))
    (is (string= (stripe:webhook-event-type event) "customer.subscription.updated"))
    (is (= (slot-value event 'stripe::%pending-webhooks) 1))))

(test construct-webhook-event-invalid-header
  (signals stripe:webhook-invalid-header
    (stripe:construct-webhook-event *webhook-event-json* "invalid-header" *webhook-secret**)))

(test construct-webhook-event-no-valid-signature
  (let* ((timestamp (local-time:now))
         (payload (dexador.util:ascii-string-to-octets *webhook-event-json*))
         (signature (stripe::compute-signature timestamp payload "invalid-secret"))
         (header (generate-header timestamp signature)))
    (signals stripe:webhook-no-valid-signature
      (stripe:construct-webhook-event payload header *webhook-secret**))))

(test construct-webhook-event-api-version-mismatch
  (let* ((timestamp (local-time:now))
         (signature (stripe::compute-signature
                     timestamp
                     (dexador.util:ascii-string-to-octets *webhook-event-json*)
                     *webhook-secret**))
         (header (generate-header timestamp signature)))
    (signals simple-error
      (stripe:construct-webhook-event (dexador.util:ascii-string-to-octets *webhook-event-json*)
                                      header
                                      *webhook-secret**))))

(test parse-signature-header-valid
  (let* ((timestamp (local-time:now))
         (payload (dexador.util:ascii-string-to-octets *webhook-event-json*))
         (signature (stripe::compute-signature timestamp payload *webhook-secret**))
         (header (generate-header timestamp signature)))
    (destructuring-bind (parsed-timestamp parsed-signatures)
        (stripe:parse-signature-header header)
      (is (<= (abs (local-time:timestamp-difference timestamp parsed-timestamp))
              stripe::+default-tolerance+))
      (is (= (length parsed-signatures) 1))
      (is (equalp (first parsed-signatures) signature)))))

(test parse-signature-header-invalid
  (signals stripe:webhook-invalid-header
    (stripe:parse-signature-header "invalid-header")))
