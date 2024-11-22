(in-package #:stripe)

(define-query retrieve-mandate (:type mandate)
  (:get "mandates/~a" mandate))
