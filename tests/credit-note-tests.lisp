(in-package #:stripe/tests)

(def-suite credit-note-tests)

(in-suite credit-note-tests)

(defparameter *create-credit-note-json*
  "{
     \"id\": \"cn_1MxvRqLkdIwHu7ixY0xbUcxk\",
     \"object\": \"credit_note\",
     \"amount\": 1099,
     \"amount_shipping\": 0,
     \"created\": 1681750958,
     \"currency\": \"usd\",
     \"customer\": \"cus_NjLgPhUokHubJC\",
     \"customer_balance_transaction\": null,
     \"discount_amount\": 0,
     \"discount_amounts\": [],
     \"invoice\": \"in_1MxvRkLkdIwHu7ixABNtI99m\",
     \"lines\": {
       \"object\": \"list\",
       \"data\": [
         {
           \"id\": \"cnli_1MxvRqLkdIwHu7ixFpdhBFQf\",
           \"object\": \"credit_note_line_item\",
           \"amount\": 1099,
           \"amount_excluding_tax\": 1099,
           \"description\": \"T-shirt\",
           \"discount_amount\": 0,
           \"discount_amounts\": [],
           \"invoice_line_item\": \"il_1MxvRlLkdIwHu7ixnkbntxUV\",
           \"livemode\": false,
           \"quantity\": 1,
           \"tax_amounts\": [],
           \"tax_rates\": [],
           \"type\": \"invoice_line_item\",
           \"unit_amount\": 1099,
           \"unit_amount_decimal\": \"1099\",
           \"unit_amount_excluding_tax\": \"1099\"
         }
       ],
       \"has_more\": false,
       \"url\": \"/v1/credit_notes/cn_1MxvRqLkdIwHu7ixY0xbUcxk/lines\"
     },
     \"livemode\": false,
     \"memo\": null,
     \"metadata\": {},
     \"number\": \"C9E0C52C-0036-CN-01\",
     \"out_of_band_amount\": null,
     \"pdf\": \"https://pay.stripe.com/credit_notes/acct_1M2JTkLkdIwHu7ix/test_YWNjdF8xTTJKVGtMa2RJd0h1N2l4LF9Oak9FOUtQNFlPdk52UXhFd2Z4SU45alpEd21kd0Y4LDcyMjkxNzU50200cROQsSK2/pdf?s=ap\",
     \"reason\": null,
     \"refund\": null,
     \"shipping_cost\": null,
     \"status\": \"issued\",
     \"subtotal\": 1099,
     \"subtotal_excluding_tax\": 1099,
     \"tax_amounts\": [],
     \"total\": 1099,
     \"total_excluding_tax\": 1099,
     \"type\": \"pre_payment\",
     \"voided_at\": null
   }"
  "https://docs.stripe.com/api/credit_notes/create")

(define-mock-query mock-create-credit-note (:type stripe:credit-note)
  *create-credit-note-json*)

(test create-credit-note
  (let ((credit-note (mock-create-credit-note)))
    (is (typep credit-note 'stripe:credit-note))
    (is (string= (slot-value credit-note 'stripe::%id) "cn_1MxvRqLkdIwHu7ixY0xbUcxk"))
    (is (= (slot-value credit-note 'stripe::%amount) 1099))
    (is (local-time:timestamp= (slot-value credit-note 'stripe::%created)
                               (stripe::decode-timestamp 1681750958)))
    (is (string= (slot-value credit-note 'stripe::%currency) "usd"))
    (is (string= (slot-value credit-note 'stripe::%customer) "cus_NjLgPhUokHubJC"))
    (is (null (slot-value credit-note 'stripe::%customer-balance-transaction)))
    (is (string= (slot-value credit-note 'stripe::%invoice) "in_1MxvRkLkdIwHu7ixABNtI99m"))
    (is (null (slot-value credit-note 'stripe::%memo)))
    (is (string= (stripe:credit-note-number credit-note) "C9E0C52C-0036-CN-01"))
    (is (string= (slot-value credit-note 'stripe::%pdf)
                 "https://pay.stripe.com/credit_notes/acct_1M2JTkLkdIwHu7ix/test_YWNjdF8xTTJKVGtMa2RJd0h1N2l4LF9Oak9FOUtQNFlPdk52UXhFd2Z4SU45alpEd21kd0Y4LDcyMjkxNzU50200cROQsSK2/pdf?s=ap"))
    (is (null (slot-value credit-note 'stripe::%reason)))
    (is (null (slot-value credit-note 'stripe::%refund)))
    (is (string= (slot-value credit-note 'stripe::%status) "issued"))
    (is (string= (stripe:credit-note-type credit-note) "pre_payment"))))

(defparameter *list-credit-notes-json*
  "{
     \"object\": \"list\",
     \"url\": \"/v1/credit_notes\",
     \"has_more\": false,
     \"data\": [
       {
         \"id\": \"cn_1MxvRqLkdIwHu7ixY0xbUcxk\",
         \"object\": \"credit_note\",
         \"amount\": 1099,
         \"amount_shipping\": 0,
         \"created\": 1681750958,
         \"currency\": \"usd\",
         \"customer\": \"cus_NjLgPhUokHubJC\",
         \"customer_balance_transaction\": null,
         \"discount_amount\": 0,
         \"discount_amounts\": [],
         \"invoice\": \"in_1MxvRkLkdIwHu7ixABNtI99m\",
         \"lines\": {
           \"object\": \"list\",
           \"data\": [
             {
               \"id\": \"cnli_1MxvRqLkdIwHu7ixFpdhBFQf\",
               \"object\": \"credit_note_line_item\",
               \"amount\": 1099,
               \"amount_excluding_tax\": 1099,
               \"description\": \"T-shirt\",
               \"discount_amount\": 0,
               \"discount_amounts\": [],
               \"invoice_line_item\": \"il_1MxvRlLkdIwHu7ixnkbntxUV\",
               \"livemode\": false,
               \"quantity\": 1,
               \"tax_amounts\": [],
               \"tax_rates\": [],
               \"type\": \"invoice_line_item\",
               \"unit_amount\": 1099,
               \"unit_amount_decimal\": \"1099\",
               \"unit_amount_excluding_tax\": \"1099\"
             }
           ],
           \"has_more\": false,
           \"url\": \"/v1/credit_notes/cn_1MxvRqLkdIwHu7ixY0xbUcxk/lines\"
         },
         \"livemode\": false,
         \"memo\": null,
         \"metadata\": {},
         \"number\": \"C9E0C52C-0036-CN-01\",
         \"out_of_band_amount\": null,
         \"pdf\": \"https://pay.stripe.com/credit_notes/acct_1M2JTkLkdIwHu7ix/test_YWNjdF8xTTJKVGtMa2RJd0h1N2l4LF9Oak9FOUtQNFlPdk52UXhFd2Z4SU45alpEd21kd0Y4LDcyMjkxNzU50200cROQsSK2/pdf?s=ap\",
         \"reason\": null,
         \"refund\": null,
         \"shipping_cost\": null,
         \"status\": \"issued\",
         \"subtotal\": 1099,
         \"subtotal_excluding_tax\": 1099,
         \"tax_amounts\": [],
         \"total\": 1099,
         \"total_excluding_tax\": 1099,
         \"type\": \"pre_payment\",
         \"voided_at\": null
       }
     ]
   }"
  "https://docs.stripe.com/api/credit_notes/list")

(define-mock-query mock-list-credit-notes (:type vector)
  *list-credit-notes-json*)

(test list-credit-notes
  (let* ((credit-notes (mock-list-credit-notes))
         (credit-note (aref credit-notes 0)))
    (is (vectorp credit-notes))
    (is (typep credit-note 'stripe:credit-note))
    (is (string= (slot-value credit-note 'stripe::%id) "cn_1MxvRqLkdIwHu7ixY0xbUcxk"))
    (is (= (slot-value credit-note 'stripe::%amount) 1099))
    (is (local-time:timestamp= (slot-value credit-note 'stripe::%created)
                               (stripe::decode-timestamp 1681750958)))
    (is (string= (slot-value credit-note 'stripe::%currency) "usd"))
    (is (string= (slot-value credit-note 'stripe::%customer) "cus_NjLgPhUokHubJC"))
    (is (null (slot-value credit-note 'stripe::%customer-balance-transaction)))
    (is (string= (slot-value credit-note 'stripe::%invoice) "in_1MxvRkLkdIwHu7ixABNtI99m"))
    (is (null (slot-value credit-note 'stripe::%memo)))
    (is (string= (stripe:credit-note-number credit-note) "C9E0C52C-0036-CN-01"))
    (is (string= (slot-value credit-note 'stripe::%pdf)
                 "https://pay.stripe.com/credit_notes/acct_1M2JTkLkdIwHu7ix/test_YWNjdF8xTTJKVGtMa2RJd0h1N2l4LF9Oak9FOUtQNFlPdk52UXhFd2Z4SU45alpEd21kd0Y4LDcyMjkxNzU50200cROQsSK2/pdf?s=ap"))
    (is (null (slot-value credit-note 'stripe::%reason)))
    (is (null (slot-value credit-note 'stripe::%refund)))
    (is (string= (slot-value credit-note 'stripe::%status) "issued"))
    (is (string= (stripe:credit-note-type credit-note) "pre_payment"))))
