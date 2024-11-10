(in-package #:stripe)

(define-query create-coupon (:type coupon)
  (:post "coupons")
  id
  duration
  amount-off
  currency
  duration-in-months
  max-redemptions
  name
  percent-off
  redeem-by)

(define-query retrieve-coupon (:type coupon)
  (:get "coupons/~a" coupon))

(define-query update-coupon (:type coupon)
  (:post "coupons/~a" coupon)
  name)

(define-query delete-coupon ()
  (:delete "coupons/~a" coupon))

(define-query list-coupons (:type vector)
  (:get "coupons")
  created
  ending-before
  limit
  starting-after)
