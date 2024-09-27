(in-package #:stripe)

;;;; NOTE: The Order API is DEPRECATED.

(define-object order-return ()
  id
  amount
  created
  currency
  items
  order
  refund)

(defmethod initialize-instance :after ((instance order-return) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:items
           (setf (slot-value instance '%items)
                 (when value
                   (map 'list
                        (lambda (x)
                          (make-instance 'order-item :data x))
                        value)))))))))

(define-query retrieve-order-return (:type order-return)
  (:get "order-returns/~a" order-return))

(define-query list-order-returns (:type vector)
  (:get "order-returns")
  created
  ending-before
  limit
  order
  starting-after)
