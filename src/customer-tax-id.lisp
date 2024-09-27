(in-package #:stripe)

(define-object customer-tax-id ()
  id
  country
  created
  customer
  (type :reader tax-id-type)
  value
  verification)

(define-object tax-id-verification ()
  status
  verified-address
  verified-name)

(defmethod initialize-instance :after ((instance customer-tax-id) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:verification
           (when value
             (setf (slot-value instance '%verification)
                   (make-instance 'tax-id-verification :data value)))))))))

(define-query create-customer-tax-id (:type customer-tax-id)
  (:post "customers/~a/tax-ids" customer)
  type
  value)

(define-query retrieve-customer-tax-id (:type customer-tax-id)
  (:get "customers/~a/tax-ids/~a" customer tax-id))

(define-query delete-customer-tax-id ()
  (:delete "customers/~a/tax-ids/~a" customer tax-id))

(define-query list-customer-tax-ids (:type customer-tax-id)
  (:get "customers/~a/tax-ids" customer)
  ending-before
  limit
  starting-after)
