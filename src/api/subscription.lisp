(in-package #:stripe)

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
