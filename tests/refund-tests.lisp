(in-package #:stripe/tests)

(def-suite refund-tests)

(in-suite refund-tests)

(defparameter *create-refund-json*
  "{
     \"id\": \"re_1Nispe2eZvKYlo2Cd31jOCgZ\",
     \"object\": \"refund\",
     \"amount\": 1000,
     \"balance_transaction\": \"txn_1Nispe2eZvKYlo2CYezqFhEx\",
     \"charge\": \"ch_1NirD82eZvKYlo2CIvbtLWuY\",
     \"created\": 1692942318,
     \"currency\": \"usd\",
     \"destination_details\": {
       \"card\": {
         \"reference\": \"123456789012\",
         \"reference_status\": \"available\",
         \"reference_type\": \"acquirer_reference_number\",
         \"type\": \"refund\"
       },
       \"type\": \"card\"
     },
     \"metadata\": {},
     \"payment_intent\": \"pi_1GszsK2eZvKYlo2CfhZyoZLp\",
     \"reason\": null,
     \"receipt_number\": null,
     \"source_transfer_reversal\": null,
     \"status\": \"succeeded\",
     \"transfer_reversal\": null
   }"
  "https://docs.stripe.com/api/refunds/create")

(define-mock-query mock-create-refund (:type stripe:refund)
  *create-refund-json*)

(test create-refund
  (let ((refund (mock-create-refund)))
    (is (typep refund 'stripe:refund))
    (is (string= (slot-value refund 'stripe::%id) "re_1Nispe2eZvKYlo2Cd31jOCgZ"))
    (is (= (slot-value refund 'stripe::%amount) 1000))
    (is (string= (slot-value refund 'stripe::%balance-transaction) "txn_1Nispe2eZvKYlo2CYezqFhEx"))
    (is (string= (slot-value refund 'stripe::%charge) "ch_1NirD82eZvKYlo2CIvbtLWuY"))
    (is (local-time:timestamp= (slot-value refund 'stripe::%created)
                               (stripe::decode-timestamp 1692942318)))
    (is (string= (slot-value refund 'stripe::%currency) "usd"))
    (null (slot-value refund 'stripe::%reason))
    (null (slot-value refund 'stripe::%receipt-number))
    (is (string= (slot-value refund 'stripe::%status) "succeeded"))))

(defparameter *list-refunds-json*
  "{
     \"object\": \"list\",
     \"url\": \"/v1/refunds\",
     \"has_more\": false,
     \"data\": [
       {
         \"id\": \"re_1Nispe2eZvKYlo2Cd31jOCgZ\",
         \"object\": \"refund\",
         \"amount\": 1000,
         \"balance_transaction\": \"txn_1Nispe2eZvKYlo2CYezqFhEx\",
         \"charge\": \"ch_1NirD82eZvKYlo2CIvbtLWuY\",
         \"created\": 1692942318,
         \"currency\": \"usd\",
         \"destination_details\": {
           \"card\": {
             \"reference\": \"123456789012\",
             \"reference_status\": \"available\",
             \"reference_type\": \"acquirer_reference_number\",
             \"type\": \"refund\"
           },
           \"type\": \"card\"
         },
         \"metadata\": {},
         \"payment_intent\": \"pi_1GszsK2eZvKYlo2CfhZyoZLp\",
         \"reason\": null,
         \"receipt_number\": null,
         \"source_transfer_reversal\": null,
         \"status\": \"succeeded\",
         \"transfer_reversal\": null
       }
     ]
   }"
  "https://docs.stripe.com/api/refunds/list")

(define-mock-query mock-list-refunds (:type vector)
  *list-refunds-json*)

(test list-refunds
  (let* ((refunds (mock-list-refunds))
         (refund (aref refunds 0)))
    (is (vectorp refunds))
    (is (typep refund 'stripe:refund))
    (is (string= (slot-value refund 'stripe::%id) "re_1Nispe2eZvKYlo2Cd31jOCgZ"))
    (is (= (slot-value refund 'stripe::%amount) 1000))
    (is (string= (slot-value refund 'stripe::%balance-transaction) "txn_1Nispe2eZvKYlo2CYezqFhEx"))
    (is (string= (slot-value refund 'stripe::%charge) "ch_1NirD82eZvKYlo2CIvbtLWuY"))
    (is (local-time:timestamp= (slot-value refund 'stripe::%created)
                               (stripe::decode-timestamp 1692942318)))
    (is (string= (slot-value refund 'stripe::%currency) "usd"))
    (null (slot-value refund 'stripe::%reason))
    (null (slot-value refund 'stripe::%receipt-number))
    (is (string= (slot-value refund 'stripe::%status) "succeeded"))))
