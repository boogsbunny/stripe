(in-package #:stripe/tests)

(def-suite invoice-tests)

(in-suite invoice-tests)

(defparameter *create-invoice-json*
  "{
     \"id\": \"in_1MtHbELkdIwHu7ixl4OzzPMv\",
     \"object\": \"invoice\",
     \"account_country\": \"US\",
     \"account_name\": \"Stripe Docs\",
     \"account_tax_ids\": null,
     \"amount_due\": 0,
     \"amount_paid\": 0,
     \"amount_remaining\": 0,
     \"amount_shipping\": 0,
     \"application\": null,
     \"application_fee_amount\": null,
     \"attempt_count\": 0,
     \"attempted\": false,
     \"auto_advance\": false,
     \"automatic_tax\": {
       \"enabled\": false,
       \"liability\": null,
       \"status\": null
     },
     \"billing_reason\": \"manual\",
     \"charge\": null,
     \"collection_method\": \"charge_automatically\",
     \"created\": 1680644467,
     \"currency\": \"usd\",
     \"custom_fields\": null,
     \"customer\": \"cus_NeZwdNtLEOXuvB\",
     \"customer_address\": null,
     \"customer_email\": \"jennyrosen@example.com\",
     \"customer_name\": \"Jenny Rosen\",
     \"customer_phone\": null,
     \"customer_shipping\": null,
     \"customer_tax_exempt\": \"none\",
     \"customer_tax_ids\": [],
     \"default_payment_method\": null,
     \"default_source\": null,
     \"default_tax_rates\": [],
     \"description\": null,
     \"discount\": null,
     \"discounts\": [],
     \"due_date\": null,
     \"ending_balance\": null,
     \"footer\": null,
     \"from_invoice\": null,
     \"hosted_invoice_url\": null,
     \"invoice_pdf\": null,
     \"issuer\": {
       \"type\": \"self\"
     },
     \"last_finalization_error\": null,
     \"latest_revision\": null,
     \"lines\": {
       \"object\": \"list\",
       \"data\": [],
       \"has_more\": false,
       \"total_count\": 0,
       \"url\": \"/v1/invoices/in_1MtHbELkdIwHu7ixl4OzzPMv/lines\"
     },
     \"livemode\": false,
     \"metadata\": {},
     \"next_payment_attempt\": null,
     \"number\": null,
     \"on_behalf_of\": null,
     \"paid\": false,
     \"paid_out_of_band\": false,
     \"payment_intent\": null,
     \"payment_settings\": {
       \"default_mandate\": null,
       \"payment_method_options\": null,
       \"payment_method_types\": null
     },
     \"period_end\": 1680644467,
     \"period_start\": 1680644467,
     \"post_payment_credit_notes_amount\": 0,
     \"pre_payment_credit_notes_amount\": 0,
     \"quote\": null,
     \"receipt_number\": null,
     \"rendering_options\": null,
     \"shipping_cost\": null,
     \"shipping_details\": null,
     \"starting_balance\": 0,
     \"statement_descriptor\": null,
     \"status\": \"draft\",
     \"status_transitions\": {
       \"finalized_at\": null,
       \"marked_uncollectible_at\": null,
       \"paid_at\": null,
       \"voided_at\": null
     },
     \"subscription\": null,
     \"subtotal\": 0,
     \"subtotal_excluding_tax\": 0,
     \"tax\": null,
     \"test_clock\": null,
     \"total\": 0,
     \"total_discount_amounts\": [],
     \"total_excluding_tax\": 0,
     \"total_tax_amounts\": [],
     \"transfer_data\": null,
     \"webhooks_delivered_at\": 1680644467
   }"
  "https://docs.stripe.com/api/invoices/create")

(define-mock-query mock-create-invoice (:type stripe:invoice)
  *create-invoice-json*)

(test create-invoice
  (let ((invoice (mock-create-invoice)))
    (is (typep invoice 'stripe:invoice))
    (is (string= (slot-value invoice 'stripe::%id) "in_1MtHbELkdIwHu7ixl4OzzPMv"))
    (is (string= (slot-value invoice 'stripe::%account-country) "US"))
    (is (string= (slot-value invoice 'stripe::%account-name) "Stripe Docs"))
    (is (= (slot-value invoice 'stripe::%amount-due) 0))
    (is (= (slot-value invoice 'stripe::%amount-remaining) 0))
    (is (= (slot-value invoice 'stripe::%attempt-count) 0))
    (is-false (slot-value invoice 'stripe::%attempted))
    (is-false (slot-value invoice 'stripe::%auto-advance))
    (is (string= (slot-value invoice 'stripe::%billing-reason) "manual"))
    (null (slot-value invoice 'stripe::%charge))
    (is (string= (slot-value invoice 'stripe::%collection-method) "charge_automatically"))
    (is (local-time:timestamp= (slot-value invoice 'stripe::%created)
                               (stripe::decode-timestamp 1680644467)))
    (is (string= (slot-value invoice 'stripe::%currency) "usd"))
    (is (string= (slot-value invoice 'stripe::%customer) "cus_NeZwdNtLEOXuvB"))
    (null (slot-value invoice 'stripe::%customer-address))
    (is (string= (slot-value invoice 'stripe::%customer-email) "jennyrosen@example.com"))
    (is (string= (slot-value invoice 'stripe::%customer-name) "Jenny Rosen"))
    (null (slot-value invoice 'stripe::%customer-phone))
    (null (slot-value invoice 'stripe::%customer-shipping))
    (null (slot-value invoice 'stripe::%default-payment-method))
    (null (slot-value invoice 'stripe::%default-source))
    (null (slot-value invoice 'stripe::%description))
    (null (slot-value invoice 'stripe::%discount))
    (null (slot-value invoice 'stripe::%due-date))
    (null (slot-value invoice 'stripe::%ending-balance))
    (null (slot-value invoice 'stripe::%footer))
    (null (slot-value invoice 'stripe::%hosted-invoice-url))
    (null (slot-value invoice 'stripe::%invoice-pdf))
    (is (hash-table-p (slot-value invoice 'stripe::%lines)))
    (null (slot-value invoice 'stripe::%next-payment-attempt))
    (null (stripe:invoice-number invoice))
    (is-false (slot-value invoice 'stripe::%paid))
    (null (slot-value invoice 'stripe::%payment-intent))
    (is (local-time:timestamp= (slot-value invoice 'stripe::%period-end)
                               (stripe::decode-timestamp 1680644467)))
    (is (local-time:timestamp= (slot-value invoice 'stripe::%period-start)
                               (stripe::decode-timestamp 1680644467)))
    (is (= (slot-value invoice 'stripe::%post-payment-credit-notes-amount) 0))
    (is (= (slot-value invoice 'stripe::%pre-payment-credit-notes-amount) 0))
    (null (slot-value invoice 'stripe::%receipt-number))
    (is (= (slot-value invoice 'stripe::%starting-balance) 0))
    (is (string= (slot-value invoice 'stripe::%status) "draft"))
    (is (typep (slot-value invoice 'stripe::%status-transitions) 'stripe::invoice-status-transition))
    (null (slot-value invoice 'stripe::%subscription))
    (is (= (slot-value invoice 'stripe::%subtotal) 0))
    (null (slot-value invoice 'stripe::%tax))
    (is (= (slot-value invoice 'stripe::%total) 0))
    (is (local-time:timestamp= (slot-value invoice 'stripe::%webhooks-delivered-at)
                               (stripe::decode-timestamp 1680644467)))))

(defparameter *list-invoices-json*
  "{
     \"object\": \"list\",
     \"url\": \"/v1/invoices\",
     \"has_more\": false,
     \"data\": [
       {
         \"id\": \"in_1MtHbELkdIwHu7ixl4OzzPMv\",
         \"object\": \"invoice\",
         \"account_country\": \"US\",
         \"account_name\": \"Stripe Docs\",
         \"account_tax_ids\": null,
         \"amount_due\": 0,
         \"amount_paid\": 0,
         \"amount_remaining\": 0,
         \"amount_shipping\": 0,
         \"application\": null,
         \"application_fee_amount\": null,
         \"attempt_count\": 0,
         \"attempted\": false,
         \"auto_advance\": false,
         \"automatic_tax\": {
           \"enabled\": false,
           \"liability\": null,
           \"status\": null
         },
         \"billing_reason\": \"manual\",
         \"charge\": null,
         \"collection_method\": \"charge_automatically\",
         \"created\": 1680644467,
         \"currency\": \"usd\",
         \"custom_fields\": null,
         \"customer\": \"cus_NeZwdNtLEOXuvB\",
         \"customer_address\": null,
         \"customer_email\": \"jennyrosen@example.com\",
         \"customer_name\": \"Jenny Rosen\",
         \"customer_phone\": null,
         \"customer_shipping\": null,
         \"customer_tax_exempt\": \"none\",
         \"customer_tax_ids\": [],
         \"default_payment_method\": null,
         \"default_source\": null,
         \"default_tax_rates\": [],
         \"description\": null,
         \"discount\": null,
         \"discounts\": [],
         \"due_date\": null,
         \"ending_balance\": null,
         \"footer\": null,
         \"from_invoice\": null,
         \"hosted_invoice_url\": null,
         \"invoice_pdf\": null,
         \"issuer\": {
           \"type\": \"self\"
         },
         \"last_finalization_error\": null,
         \"latest_revision\": null,
         \"lines\": {
           \"object\": \"list\",
           \"data\": [],
           \"has_more\": false,
           \"total_count\": 0,
           \"url\": \"/v1/invoices/in_1MtHbELkdIwHu7ixl4OzzPMv/lines\"
         },
         \"livemode\": false,
         \"metadata\": {},
         \"next_payment_attempt\": null,
         \"number\": null,
         \"on_behalf_of\": null,
         \"paid\": false,
         \"paid_out_of_band\": false,
         \"payment_intent\": null,
         \"payment_settings\": {
           \"default_mandate\": null,
           \"payment_method_options\": null,
           \"payment_method_types\": null
         },
         \"period_end\": 1680644467,
         \"period_start\": 1680644467,
         \"post_payment_credit_notes_amount\": 0,
         \"pre_payment_credit_notes_amount\": 0,
         \"quote\": null,
         \"receipt_number\": null,
         \"rendering_options\": null,
         \"shipping_cost\": null,
         \"shipping_details\": null,
         \"starting_balance\": 0,
         \"statement_descriptor\": null,
         \"status\": \"draft\",
         \"status_transitions\": {
           \"finalized_at\": null,
           \"marked_uncollectible_at\": null,
           \"paid_at\": null,
           \"voided_at\": null
         },
         \"subscription\": null,
         \"subtotal\": 0,
         \"subtotal_excluding_tax\": 0,
         \"tax\": null,
         \"test_clock\": null,
         \"total\": 0,
         \"total_discount_amounts\": [],
         \"total_excluding_tax\": 0,
         \"total_tax_amounts\": [],
         \"transfer_data\": null,
         \"webhooks_delivered_at\": 1680644467
       }
     ]
   }"
  "https://docs.stripe.com/api/invoices/list")

(define-mock-query mock-list-invoices (:type vector)
  *list-invoices-json*)

(test list-invoices
  (let* ((invoices (mock-list-invoices))
         (invoice (aref invoices 0)))
    (is (vectorp invoices))
    (is (typep invoice 'stripe:invoice))
    (is (string= (slot-value invoice 'stripe::%id) "in_1MtHbELkdIwHu7ixl4OzzPMv"))
    (is (string= (slot-value invoice 'stripe::%account-country) "US"))
    (is (string= (slot-value invoice 'stripe::%account-name) "Stripe Docs"))
    (is (= (slot-value invoice 'stripe::%amount-due) 0))
    (is (= (slot-value invoice 'stripe::%amount-remaining) 0))
    (is (= (slot-value invoice 'stripe::%attempt-count) 0))
    (is-false (slot-value invoice 'stripe::%attempted))
    (is-false (slot-value invoice 'stripe::%auto-advance))
    (is (string= (slot-value invoice 'stripe::%billing-reason) "manual"))
    (null (slot-value invoice 'stripe::%charge))
    (is (string= (slot-value invoice 'stripe::%collection-method) "charge_automatically"))
    (is (local-time:timestamp= (slot-value invoice 'stripe::%created)
                               (stripe::decode-timestamp 1680644467)))
    (is (string= (slot-value invoice 'stripe::%currency) "usd"))
    (is (string= (slot-value invoice 'stripe::%customer) "cus_NeZwdNtLEOXuvB"))
    (null (slot-value invoice 'stripe::%customer-address))
    (is (string= (slot-value invoice 'stripe::%customer-email) "jennyrosen@example.com"))
    (is (string= (slot-value invoice 'stripe::%customer-name) "Jenny Rosen"))
    (null (slot-value invoice 'stripe::%customer-phone))
    (null (slot-value invoice 'stripe::%customer-shipping))
    (null (slot-value invoice 'stripe::%default-payment-method))
    (null (slot-value invoice 'stripe::%default-source))
    (null (slot-value invoice 'stripe::%description))
    (null (slot-value invoice 'stripe::%discount))
    (null (slot-value invoice 'stripe::%due-date))
    (null (slot-value invoice 'stripe::%ending-balance))
    (null (slot-value invoice 'stripe::%footer))
    (null (slot-value invoice 'stripe::%hosted-invoice-url))
    (null (slot-value invoice 'stripe::%invoice-pdf))
    (is (hash-table-p (slot-value invoice 'stripe::%lines)))
    (null (slot-value invoice 'stripe::%next-payment-attempt))
    (null (stripe:invoice-number invoice))
    (is-false (slot-value invoice 'stripe::%paid))
    (null (slot-value invoice 'stripe::%payment-intent))
    (is (local-time:timestamp= (slot-value invoice 'stripe::%period-end)
                               (stripe::decode-timestamp 1680644467)))
    (is (local-time:timestamp= (slot-value invoice 'stripe::%period-start)
                               (stripe::decode-timestamp 1680644467)))
    (is (= (slot-value invoice 'stripe::%post-payment-credit-notes-amount) 0))
    (is (= (slot-value invoice 'stripe::%pre-payment-credit-notes-amount) 0))
    (null (slot-value invoice 'stripe::%receipt-number))
    (is (= (slot-value invoice 'stripe::%starting-balance) 0))
    (is (string= (slot-value invoice 'stripe::%status) "draft"))
    (is (typep (slot-value invoice 'stripe::%status-transitions) 'stripe::invoice-status-transition))
    (null (slot-value invoice 'stripe::%subscription))
    (is (= (slot-value invoice 'stripe::%subtotal) 0))
    (null (slot-value invoice 'stripe::%tax))
    (is (= (slot-value invoice 'stripe::%total) 0))
    (is (local-time:timestamp= (slot-value invoice 'stripe::%webhooks-delivered-at)
                               (stripe::decode-timestamp 1680644467)))))
