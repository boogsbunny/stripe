(in-package #:stripe/tests)

(def-suite coupon-tests)

(in-suite coupon-tests)

(defparameter *create-coupon-json*
  "{
     \"id\": \"jMT0WJUD\",
     \"object\": \"coupon\",
     \"amount_off\": 1000,
     \"created\": 1622628049,
     \"currency\": \"usd\",
     \"duration\": \"repeating\",
     \"duration_in_months\": 3,
     \"livemode\": false,
     \"max_redemptions\": 100,
     \"metadata\": {},
     \"name\": \"Summer Sale\",
     \"percent_off\": 25.5,
     \"redeem_by\": 1625216049,
     \"times_redeemed\": 0,
     \"valid\": true
   }"
   "https://docs.stripe.com/api/coupons/create")

(define-mock-query mock-create-coupon (:type stripe:coupon)
  *create-coupon-json*)

(test create-coupon
  (let ((coupon (mock-create-coupon)))
    (is (typep coupon 'stripe:coupon))
    (is (string= "jMT0WJUD" (slot-value coupon 'stripe::%id)))
    (is (= 1000 (slot-value coupon 'stripe::%amount-off)))
    (is (string= "usd" (slot-value coupon 'stripe::%currency)))
    (is (string= "repeating" (slot-value coupon 'stripe::%duration)))
    (is (= 3 (slot-value coupon 'stripe::%duration-in-months)))
    (is (= 100 (slot-value coupon 'stripe::%max-redemptions)))
    (is (string= "Summer Sale" (slot-value coupon 'stripe::%name)))))

(defparameter *list-coupons-json*
  "{
    \"object\": \"list\",
    \"url\": \"/v1/coupons\",
    \"has_more\": false,
    \"data\": [
      {
        \"id\": \"jMT0WJUD\",
        \"object\": \"coupon\",
        \"amount_off\": null,
        \"created\": 1678037688,
        \"currency\": null,
        \"duration\": \"repeating\",
        \"duration_in_months\": 3,
        \"livemode\": false,
        \"max_redemptions\": null,
        \"metadata\": {},
        \"name\": null,
        \"percent_off\": 25.5,
        \"redeem_by\": null,
        \"times_redeemed\": 0,
        \"valid\": true
      }
    ]
  }"
  "https://docs.stripe.com/api/coupons/list")

(define-mock-query mock-list-coupons (:type vector)
  *list-coupons-json*)

(test list-coupons
  (let* ((coupons (mock-list-coupons))
         (coupon (aref coupons 0)))
    (is (vectorp coupons))
    (is (typep coupon 'stripe:coupon))
    (is (string= "jMT0WJUD" (slot-value coupon 'stripe::%id)))
    (is (string= "repeating" (slot-value coupon 'stripe::%duration)))
    (is (= 3 (slot-value coupon 'stripe::%duration-in-months)))
    (is (= 25.5 (slot-value coupon 'stripe::%percent-off)))))
