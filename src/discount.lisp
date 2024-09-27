(in-package #:stripe)

(define-object discount ()
  coupon
  customer
  end
  start
  subscription)

(defmethod initialize-instance :after ((instance discount) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:end
           (setf (slot-value instance '%end) (decode-timestamp value)))
          (:start
           (setf (slot-value instance '%start) (decode-timestamp value))))))))

(define-query delete-customer-discount ()
  (:delete "customers/~a/discount" customer))

(define-query delete-subscription-discount ()
  (:delete "subscriptions/~a/discount" subscription))
