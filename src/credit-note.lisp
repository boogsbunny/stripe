(in-package #:stripe)

(define-object credit-note ()
  id
  amount
  created
  currency
  customer
  customer-balance-transaction
  invoice
  memo
  (number :reader credit-note-number)
  pdf
  reason
  refund
  status
  (type :reader credit-note-type))

(defmethod initialize-instance :after ((instance credit-note) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created)
                 (decode-timestamp value))))))))

(define-query create-credit-note (:type credit-note)
  (:post "credit-notes")
  amount
  invoice
  credit-amount
  memo
  reason
  refund
  refund-amount)

(define-query retrieve-credit-note (:type credit-note)
  (:get "credit-notes/~a" credit-note))

(define-query update-credit-note (:type credit-note)
  (:post "credit-notes/~a" credit-note)
  memo)

(define-query void-credit-note (:type credit-note)
  (:post "credit-notes/~a/void" credit-note))

(define-query list-credit-notes (:type vector)
  (:get "credit-notes")
  ending-before
  invoice
  limit
  starting-after)
