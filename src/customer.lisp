(in-package #:stripe)

(define-object customer ()
  id
  address
  balance
  created
  currency
  default-source
  delinquent
  description
  discount
  email
  name
  phone
  shipping
  sources
  subscriptions)

(defmethod initialize-instance :after ((instance customer) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:address
           (unless (eql 'null value)
             (setf (slot-value instance '%address)
                   (make-instance 'address :data value))))
          (:shipping
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping)
                   (make-instance 'shipping :data value))))
          (:sources
           (when value
             (setf (slot-value instance '%sources)
                   (decode-hash-table value))))
          (:subscriptions
           (when value
             (setf (slot-value instance '%subscriptions)
                   (decode-hash-table value)))))))))

(define-query create-customer (:type customer)
  (:post "customers")
  balance
  description
  name
  email
  phone
  address
  shipping
  source)

(define-query retrieve-customer (:type customer)
  (:get "customers/~a" customer))

(define-query update-customer (:type customer)
  (:post "customers/~a" customer)
  address
  balance
  coupon
  default-source
  description
  email
  name
  phone
  shipping
  source)

(define-query delete-customer ()
  (:delete "customers/~a" customer))

(define-query list-customers (:type vector)
  (:get "customers")
  created
  email
  ending-before
  limit
  starting-after)
