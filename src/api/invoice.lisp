(in-package #:stripe)

(define-query create-invoice (:type invoice)
  (:post "invoices")
  auto-advance
  collection-method
  customer
  days-until-due
  default-payment-method
  default-source
  description
  due-date
  footer
  statement-descriptor
  subscription)

(define-query retrieve-invoice (:type invoice)
  (:get "invoices/~a" invoice))

(define-query update-invoice (:type invoice)
  (:post "invoices/~a" invoice)
  auto-advance
  collection-method
  days-until-due
  default-payment-method
  default-source
  description
  due-date
  footer
  statement-descriptor)

(define-query delete-invoice (:type invoice)
  (:delete "invoices/~a" invoice))

(define-query finalize-invoice (:type invoice)
  (:post "invoices/~a/finalize" invoice)
  auto-advance)

(define-query pay-invoice (:type invoice)
  (:post "invoices/~a/pay" invoice)
  forgive
  paid-out-of-band
  payment-method
  source)

(define-query send-invoice (:type invoice)
  (:post "invoices/~a/send" invoice))

(define-query void-invoice (:type invoice)
  (:post "invoices/~a/void" invoice))

(define-query mark-invoice-uncollectible (:type invoie)
  (:post "invoices/~a/mark-uncollectible" invoice))

(define-query retrieve-invoice-lines (:type vector)
  (:get "invoices/~a/lines" invoice)
  ending-before
  limit
  starting-after)

(define-query retrieve-upcoming-invoice (:type invoice)
  (:get "invoices/upcoming")
  coupon
  customer
  invoice-items
  schedule
  subscription
  subscription-billing-cycle-anchor
  subscription-cancel-at
  subscription-cancel-at-period-end
  subscription-cancel-now
  subscription-items
  subscription-prorate
  subscription-proration-date
  subscription-start-date
  subscription-trial-end
  subscription-trial-from-plan)

(define-query retrieve-upcoming-invoice-lines (:type vector)
  (:get "invoices/upcoming/lines")
  coupon
  customer
  ending-before
  invoice-items
  limit
  schedule
  starting-after
  subscription
  subscription-billing-anchor
  subscription-cancel-at
  subscription-cancel-at-period-end
  subscription-cancel-now
  subscription-items
  subscription-prorate
  subscription-proration-date
  subscription-start-date
  subscription-trial-end
  subscription-trial-from-plan)

(define-query list-invoices (:type vector)
  (:get "invoices")
  collection-method
  created
  customer
  due-date
  ending-before
  limit
  starting-after
  status
  subscription)
