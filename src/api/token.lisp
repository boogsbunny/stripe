(in-package #:stripe)

(define-query create-card-token (:type card-token)
  (:post "tokens")
  card)

(define-query retrieve-card-token (:type card-token)
  (:get "tokens/~a" card-token))
