(in-package #:stripe/tests)

(defmacro define-mock-query (name (&key type) json-response)
  "A mock implementation of the `stripe:define-query` macro."
  `(defun ,name (&rest args)
     (declare (ignore args))
     (let ((response (jzon:parse ,json-response :key-fn #'stripe::normalize-json-key)))
       ,(case type
          (vector `(stripe::decode-hash-table response))
          ((nil) `response)
          (t `(make-instance ',type :data response))))))

(def-suite common-tests)

(in-suite common-tests)

(test normalize-invoiceitem
  (is (string= (stripe::normalize-object-type "invoiceitem") "invoice-item")))

(test normalize-subscription_item
  (is (string= (stripe::normalize-object-type "subscription_item") "subscription_item")))
