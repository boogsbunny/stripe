(in-package :stripe)

(define-query retrieve-balance (:type balance)
  (:get "balance"))
