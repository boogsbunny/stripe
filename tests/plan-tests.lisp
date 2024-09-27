(in-package #:stripe/tests)

(def-suite plan-tests)

(in-suite plan-tests)

(defparameter *create-plan-json*
  "{
     \"id\": \"plan_NjpIbv3g3ZibnD\",
     \"object\": \"plan\",
     \"active\": true,
     \"aggregate_usage\": null,
     \"amount\": 1200,
     \"amount_decimal\": \"1200\",
     \"billing_scheme\": \"per_unit\",
     \"created\": 1681851647,
     \"currency\": \"usd\",
     \"interval\": \"month\",
     \"interval_count\": 1,
     \"livemode\": false,
     \"metadata\": {},
     \"nickname\": null,
     \"product\": \"prod_NjpI7DbZx6AlWQ\",
     \"tiers_mode\": null,
     \"transform_usage\": null,
     \"trial_period_days\": null,
     \"usage_type\": \"licensed\"
   }"
  "https://docs.stripe.com/api/plans/create")

(define-mock-query mock-create-plan (:type stripe:plan)
  *create-plan-json*)

(test create-plan
  (let ((plan (mock-create-plan)))
    (is (typep plan 'stripe:plan))
    (is (string= (slot-value plan 'stripe::%id) "plan_NjpIbv3g3ZibnD"))
    (is-true (slot-value plan 'stripe::%active))
    (null (slot-value plan 'stripe::%aggregate-usage))
    (is (= (slot-value plan 'stripe::%amount) 1200))
    (is (string= (slot-value plan 'stripe::%billing-scheme) "per_unit"))
    (is (local-time:timestamp= (slot-value plan 'stripe::%created)
                               (stripe::decode-timestamp 1681851647)))
    (is (string= (slot-value plan 'stripe::%currency) "usd"))
    (is (string= (slot-value plan 'stripe::%interval) "month"))
    (null (slot-value plan 'stripe::%nickname))
    (is (string= (slot-value plan 'stripe::%product) "prod_NjpI7DbZx6AlWQ"))
    (null (slot-value plan 'stripe::%trial-period-days))
    (is (string= (slot-value plan 'stripe::%usage-type) "licensed"))))

(defparameter *list-plans-json*
  "{
     \"object\": \"list\",
     \"url\": \"/v1/plans\",
     \"has_more\": false,
     \"data\": [
       {
         \"id\": \"plan_NjpIbv3g3ZibnD\",
         \"object\": \"plan\",
         \"active\": true,
         \"aggregate_usage\": null,
         \"amount\": 1200,
         \"amount_decimal\": \"1200\",
         \"billing_scheme\": \"per_unit\",
         \"created\": 1681851647,
         \"currency\": \"usd\",
         \"interval\": \"month\",
         \"interval_count\": 1,
         \"livemode\": false,
         \"metadata\": {},
         \"nickname\": null,
         \"product\": \"prod_NjpI7DbZx6AlWQ\",
         \"tiers_mode\": null,
         \"transform_usage\": null,
         \"trial_period_days\": null,
         \"usage_type\": \"licensed\"
       }
     ]
   }"
  "https://docs.stripe.com/api/plans/list")

(define-mock-query mock-list-plans (:type vector)
  *list-plans-json*)

(test list-plans
  (let* ((plans (mock-list-plans))
         (plan (aref plans 0)))
    (is (vectorp plans))
    (is (typep plan 'stripe:plan))
    (is (string= (slot-value plan 'stripe::%id) "plan_NjpIbv3g3ZibnD"))
    (is-true (slot-value plan 'stripe::%active))
    (null (slot-value plan 'stripe::%aggregate-usage))
    (is (= (slot-value plan 'stripe::%amount) 1200))
    (is (string= (slot-value plan 'stripe::%billing-scheme) "per_unit"))
    (is (local-time:timestamp= (slot-value plan 'stripe::%created)
                               (stripe::decode-timestamp 1681851647)))
    (is (string= (slot-value plan 'stripe::%currency) "usd"))
    (is (string= (slot-value plan 'stripe::%interval) "month"))
    (null (slot-value plan 'stripe::%nickname))
    (is (string= (slot-value plan 'stripe::%product) "prod_NjpI7DbZx6AlWQ"))
    (null (slot-value plan 'stripe::%trial-period-days))
    (is (string= (slot-value plan 'stripe::%usage-type) "licensed"))))
