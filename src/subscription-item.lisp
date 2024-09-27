(in-package #:stripe)

(define-object subscription-item ()
  id
  created
  quantity
  subscription)

(defmethod initialize-instance :after ((instance subscription-item) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value))))))))

(define-query create-subscription-item (:type subscription-item)
  (:post "subscription-items")
  subscription
  prorate
  proration-date
  quantity)

(define-query retrieve-subscription-item (:type subscription-item)
  (:get "subscription-items/~a" subscription-item))

(define-query update-subscription-item (:type subscription-item)
  (:post "subscription-items/~a" subscription-item)
  prorate
  proration-date
  quantity)

(define-query delete-subscription-item ()
  (:delete "subscription-items/~a" subscription-item)
  clear-usage
  prorate
  proration-date)

(define-query list-subscription-items (:type vector)
  (:get "subscription-items")
  ending-before
  limit
  starting-after)
