(in-package #:stripe)

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
