(in-package #:stripe)

(define-object subscription ()
  billing-cycle-anchor
  cancel-at
  cancel-at-period-end
  canceled-at
  collection-method
  created
  current-period-end
  current-period-start
  customer
  days-until-due
  default-payment-method
  discount
  ended-at
  id
  items
  latest-invoice
  quantity
  schedule
  start-date
  status
  trial-end
  trial-start)

(defmethod initialize-instance :after ((instance subscription) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:billing-cycle-anchor
           (setf (slot-value instance '%billing-cycle-anchor) (decode-timestamp value)))
          (:cancel-at
           (setf (slot-value instance '%cancel-at) (decode-timestamp value)))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:current-period-end
           (setf (slot-value instance '%current-period-end) (decode-timestamp value)))
          (:current-period-start
           (setf (slot-value instance '%current-period-start) (decode-timestamp value)))
          (:discount
           (unless (eql 'null value)
             (setf (slot-value instance '%discount) (make-instance 'discount :data value))))
          (:ended-at
           (setf (slot-value instance '%ended-at) (decode-timestamp value)))
          (:items
           (when value
             (setf (slot-value instance '%items) (decode-hash-table value))))
          (:start-date
           (setf (slot-value instance '%start-date) (decode-timestamp value)))
          (:trial-end
           (setf (slot-value instance '%trial-end) (decode-timestamp value)))
          (:trial-start
           (setf (slot-value instance '%trial-start) (decode-timestamp value))))))))

(define-query create-subscription (:type subscription)
  (:post "subscriptions")
  customer
  backdate-start-date
  billing-cycle-anchor
  cancel-at
  cancel-at-period-end
  collection-method
  coupon
  days-until-due
  default-payment-method
  default-source
  items
  prorate
  trial-end
  trial-from-plan
  trial-period-days)

(define-query retrieve-subscription (:type subscription)
  (:get "subscriptions/~a" subscription))

(define-query update-subscription (:type subscription)
  (:post "subscriptions/~a" subscription)
  billing-cycle-anchor
  cancel-at
  cancel-at-period-end
  collection-method
  coupon
  days-until-due
  default-payment-method
  default-source
  items
  prorate
  proration-date
  trial-end
  trial-from-plan)

(define-query cancel-subscription (:type subscription)
  (:delete "subscriptions/~a" subscription)
  invoice-now
  prorate)

(define-query list-subscriptions (:type vector)
  (:get "subscriptions")
  collection-method
  created
  current-period-end
  current-period-start
  customer
  ending-before
  limit
  starting-after
  status)
