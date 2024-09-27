(in-package #:stripe/tests)

(def-suite product-tests)

(in-suite product-tests)

(defparameter *create-product-json*
  "{
     \"id\": \"prod_NWjs8kKbJWmuuc\",
     \"object\": \"product\",
     \"active\": true,
     \"created\": 1678833149,
     \"default_price\": null,
     \"description\": null,
     \"images\": [],
     \"features\": [],
     \"livemode\": false,
     \"metadata\": {},
     \"name\": \"Gold Plan\",
     \"package_dimensions\": null,
     \"shippable\": null,
     \"statement_descriptor\": null,
     \"tax_code\": null,
     \"unit_label\": null,
     \"updated\": 1678833149,
     \"url\": null
   }"
  "https://docs.stripe.com/api/products/create")

(define-mock-query mock-create-product (:type stripe:product)
  *create-product-json*)

(test create-product
  (let ((product (mock-create-product)))
    (is (typep product 'stripe:product))
    (is (string= (slot-value product 'stripe::%id) "prod_NWjs8kKbJWmuuc"))
    (is-true (slot-value product 'stripe::%active))
    (is (local-time:timestamp= (slot-value product 'stripe::%created)
                               (stripe::decode-timestamp 1678833149)))
    (is (null (slot-value product 'stripe::%description)))
    (is (vectorp (slot-value product 'stripe::%images)))
    (is (string= (slot-value product 'stripe::%name) "Gold Plan"))
    (is (null (slot-value product 'stripe::%package-dimensions)))
    (is (null (slot-value product 'stripe::%shippable)))
    (is (null (slot-value product 'stripe::%statement-descriptor)))
    (is (null (slot-value product 'stripe::%unit-label)))
    (is (local-time:timestamp= (slot-value product 'stripe::%updated)
                               (stripe::decode-timestamp 1678833149)))
    (is (null (slot-value product 'stripe::%url)))))

(defparameter *list-products-json*
  "{
     \"object\": \"list\",
     \"url\": \"/v1/products\",
     \"has_more\": false,
     \"data\": [
       {
         \"id\": \"prod_NWjs8kKbJWmuuc\",
         \"object\": \"product\",
         \"active\": true,
         \"created\": 1678833149,
         \"default_price\": null,
         \"description\": null,
         \"images\": [],
         \"features\": [],
         \"livemode\": false,
         \"metadata\": {},
         \"name\": \"Gold Plan\",
         \"package_dimensions\": null,
         \"shippable\": null,
         \"statement_descriptor\": null,
         \"tax_code\": null,
         \"unit_label\": null,
         \"updated\": 1678833149,
         \"url\": null
       }
     ]
   }"
  "https://docs.stripe.com/api/products/list")

(define-mock-query mock-list-products (:type vector)
  *list-products-json*)

(test list-products
  (let* ((products (mock-list-products))
         (product (aref products 0)))
    (is (vectorp products))
    (is (typep product 'stripe:product))
    (is (string= (slot-value product 'stripe::%id) "prod_NWjs8kKbJWmuuc"))
    (is-true (slot-value product 'stripe::%active))
    (is (local-time:timestamp= (slot-value product 'stripe::%created)
                               (stripe::decode-timestamp 1678833149)))
    (is (null (slot-value product 'stripe::%description)))
    (is (vectorp (slot-value product 'stripe::%images)))
    (is (string= (slot-value product 'stripe::%name) "Gold Plan"))
    (is (null (slot-value product 'stripe::%package-dimensions)))
    (is (null (slot-value product 'stripe::%shippable)))
    (is (null (slot-value product 'stripe::%statement-descriptor)))
    (is (null (slot-value product 'stripe::%unit-label)))
    (is (local-time:timestamp= (slot-value product 'stripe::%updated)
                               (stripe::decode-timestamp 1678833149)))
    (is (null (slot-value product 'stripe::%url)))))
