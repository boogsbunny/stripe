(in-package #:stripe)

(define-object subscription-item ()
  "Subscription items allow you to create customer subscriptions with
more than one plan, making it easy to represent complex billing
relationships."
  id
  created
  quantity
  subscription)

(defmethod initialize-instance :after ((instance subscription-item) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value))))))))
