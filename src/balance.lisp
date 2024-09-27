(in-package #:stripe)

(define-object balance ()
  available
  pending)

(define-object balance-funds ()
  amount
  currency
  bank-account
  card)

(defmethod initialize-instance :after ((instance balance-funds) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:amount
           (setf (slot-value instance '%amount) value))
          (:currency
           (setf (slot-value instance '%currency) value))
          (:source-types
           (setf (slot-value instance '%bank-account) (gethash :bank-account value 0)
                 (slot-value instance '%card) (gethash :card value 0))))))))

(defmethod initialize-instance :after ((instance balance) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:available
           (setf (slot-value instance '%available)
                 (map 'list
                      (lambda (x)
                        (make-instance 'balance-funds :data x))
                      value)))
          (:pending
           (setf (slot-value instance '%pending)
                 (map 'list
                      (lambda (x)
                        (make-instance 'balance-funds :data x))
                      value))))))))

(define-query retrieve-balance (:type balance)
  (:get "balance"))
