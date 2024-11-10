(in-package #:stripe)

(define-object plan ()
  id
  active
  aggregate-usage
  amount
  billing-scheme
  created
  currency
  interval
  nickname
  product
  trial-period-days
  usage-type)

(defmethod initialize-instance :after ((instance plan) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value))))))))
