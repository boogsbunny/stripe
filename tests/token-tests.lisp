(in-package #:stripe/tests)

(def-suite token-tests)

(in-suite token-tests)

(defparameter *create-card-token-json*
  "{
     \"id\": \"tok_1N3T00LkdIwHu7ixt44h1F8k\",
     \"object\": \"token\",
     \"card\": {
       \"id\": \"card_1N3T00LkdIwHu7ixRdxpVI1Q\",
       \"object\": \"card\",
       \"address_city\": null,
       \"address_country\": null,
       \"address_line1\": null,
       \"address_line1_check\": null,
       \"address_line2\": null,
       \"address_state\": null,
       \"address_zip\": null,
       \"address_zip_check\": null,
       \"brand\": \"Visa\",
       \"country\": \"US\",
       \"cvc_check\": \"unchecked\",
       \"dynamic_last4\": null,
       \"exp_month\": 5,
       \"exp_year\": 2024,
       \"fingerprint\": \"mToisGZ01V71BCos\",
       \"funding\": \"credit\",
       \"last4\": \"4242\",
       \"metadata\": {},
       \"name\": null,
       \"tokenization_method\": null,
       \"wallet\": null
     },
     \"client_ip\": \"52.35.78.6\",
     \"created\": 1683071568,
     \"livemode\": false,
     \"type\": \"card\",
     \"used\": false
   }"
  "https://docs.stripe.com/api/tokens/create_card")

(define-mock-query mock-create-card-token (:type stripe::card-token)
  *create-card-token-json*)

(test create-card-token
  (let ((card-token (mock-create-card-token)))
    (is (typep card-token 'stripe::card-token))
    (is (string= (slot-value card-token 'stripe::%id) "tok_1N3T00LkdIwHu7ixt44h1F8k"))
    (is (typep (slot-value card-token 'stripe::%card) 'stripe:card))
    (is (local-time:timestamp= (slot-value card-token 'stripe::%created)
                               (stripe::decode-timestamp 1683071568)))
    (is (string= (slot-value card-token 'stripe::%client-ip) "52.35.78.6"))
    (is-false (slot-value card-token 'stripe::%used))))
