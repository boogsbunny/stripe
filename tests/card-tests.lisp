(in-package #:stripe/tests)

(def-suite card-tests)

(in-suite card-tests)

(defparameter *create-card-json*
  "{
    \"id\": \"card_1NGTaT2eZvKYlo2CZWSctn5n\",
    \"object\": \"card\",
    \"address_city\": \"Houston\",
    \"address_country\": \"United States\",
    \"address_line1\": null,
    \"address_line1_check\": null,
    \"address_line2\": null,
    \"address_state\": \"Texas\",
    \"address_zip\": null,
    \"address_zip_check\": null,
    \"brand\": \"Visa\",
    \"country\": \"US\",
    \"customer\": \"cus_9s6XGDTHzA66Po\",
    \"cvc_check\": \"pass\",
    \"dynamic_last4\": null,
    \"exp_month\": 8,
    \"exp_year\": 2024,
    \"fingerprint\": \"Xt5EWLLDS7FJjR1c\",
    \"funding\": \"credit\",
    \"last4\": \"4242\",
    \"metadata\": {},
    \"name\": null,
    \"redaction\": null,
    \"tokenization_method\": null,
    \"wallet\": null
  }"
  "https://docs.stripe.com/api/cards/create")

(define-mock-query mock-create-card (:type stripe:card)
  *create-card-json*)

(test create-card
  (let ((card (mock-create-card)))
    (is (typep card 'stripe:card))
    (is (string= "card_1NGTaT2eZvKYlo2CZWSctn5n" (slot-value card 'stripe::%id)))
    (is (string= "Houston" (slot-value card 'stripe::%address-city)))
    (is (string= "Texas" (slot-value card 'stripe::%address-state)))
    (is (string= "United States" (slot-value card 'stripe::%address-country)))
    (is (string= "cus_9s6XGDTHzA66Po" (slot-value card 'stripe::%customer)))
    (is (string= "Visa" (slot-value card 'stripe::%brand)))
    (is (= 8 (slot-value card 'stripe::%exp-month)))
    (is (= 2024 (slot-value card 'stripe::%exp-year)))
    (is (string= "4242" (slot-value card 'stripe::%last4)))))

(defparameter *list-cards-json*
  "{
    \"object\": \"list\",
    \"url\": \"/v1/customers/cus_NhD8HD2bY8dP3V/cards\",
    \"has_more\": false,
    \"data\": [
      {
        \"id\": \"card_1MvoiELkdIwHu7ixOeFGbN9D\",
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
        \"customer\": \"cus_NhD8HD2bY8dP3V\",
        \"cvc_check\": null,
        \"dynamic_last4\": null,
        \"exp_month\": 4,
        \"exp_year\": 2024,
        \"fingerprint\": \"mToisGZ01V71BCos\",
        \"funding\": \"credit\",
        \"last4\": \"4242\",
        \"metadata\": {},
        \"name\": null,
        \"tokenization_method\": null,
        \"wallet\": null
      }
    ]
  }"
  "https://docs.stripe.com/api/cards/list")

(define-mock-query mock-list-cards (:type vector)
  *list-cards-json*)

(test list-cards
  (let* ((cards (mock-list-cards))
        (card (aref cards 0)))
    (is (vectorp cards))
    (is (typep card 'stripe:card))
    (is (string= "card_1MvoiELkdIwHu7ixOeFGbN9D" (slot-value card 'stripe::%id)))
    (is (string= "cus_NhD8HD2bY8dP3V" (slot-value card 'stripe::%customer)))
    (is (string= "Visa" (slot-value card 'stripe::%brand)))
    (is (= 4 (slot-value card 'stripe::%exp-month)))
    (is (= 2024 (slot-value card 'stripe::%exp-year)))
    (is (string= "4242" (slot-value card 'stripe::%last4)))))
