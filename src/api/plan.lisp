(in-package #:stripe)

(define-query create-plan (:type plan)
  (:post "plans")
  active
  aggregate-usage
  amount
  billing-scheme
  currency
  interval
  interval-count
  nickname
  product
  trial-period-days
  usage-type)

(define-query retrieve-plan (:type plan)
  (:get "plans/~a" plan))

(define-query update-plan (:type plan)
  (:post "plans/~a" plan)
  active
  nickname
  product
  trial-period-days)

(define-query delete-plan ()
  (:delete "plans/~a" plan))

(define-query list-plans (:type vector)
  (:get "plans")
  active
  created
  ending-before
  limit
  product
  starting-after)
