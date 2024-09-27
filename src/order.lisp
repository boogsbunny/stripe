(in-package #:stripe)

;;;; NOTE: The Order API is DEPRECATED.

(define-object order ()
  id
  amount
  amount-returned
  charge
  created
  currency
  customer
  email
  external-coupon-code
  items
  returns
  selected-shipping-method
  shipping
  shipping-methods
  status
  status-transitions
  updated
  upstream-id)

(define-object order-shipping (shipping)
  carrier
  tracking-number)

(define-object order-shipping-methods ()
  id
  amount
  currency
  delivery-estimate
  description)

(defmethod initialize-instance :after ((instance order-shipping-methods) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:delivery-estimate
           (unless (eql 'null value)
             (setf (slot-value instance '%delivery-estimate)
                   (make-instance 'order-delivery-estimate :data value)))))))))

(define-object order-delivery-estimate ()
  date
  earliest
  latest
  (type :reader delivery-estimate-type))

(define-object order-item ()
  amount
  currency
  description
  parent
  quantity
  (type :reader order-item-type))

(defmethod initialize-instance :after ((instance order) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:items
           (setf (slot-value instance '%items)
                 (when value
                   (map 'list
                        (lambda (x)
                          (make-instance 'order-item :data x))
                        value))))
          (:returns
           (unless (eql 'null value)
             (setf (slot-value instance '%returns)
                   (decode-hash-table value))))
          (:shipping
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping)
                   (make-instance 'order-shipping :data value))))
          (:shipping-methods
           (setf (slot-value instance '%shipping-methods)
                 (when value
                   (map 'list
                        (lambda (x)
                          (make-instance 'order-shipping-methods :data x))
                        value)))))))))

(define-query create-order (:type order)
  (:post "orders")
  currency
  coupon
  customer
  email
  items
  shipping)

(define-query retrieve-order (:type order)
  (:get "orders/~a" order))

(define-query update-order (:type order)
  (:post "orders/~a" order)
  coupon
  selected-shipping-method
  shipping
  status)

(define-query pay-order (:type order)
  (:post "orders/~a/pay" order)
  customer
  source
  email)

(define-query list-orders (:type vector)
  (:get "orders")
  created
  customer
  ending-before
  ids
  limit
  starting-after
  status
  status-transitions
  upstream-ids)

(define-query return-order (:type order-return)
  (:post "orders/~a/returns" order)
  items)
