(in-package #:stripe)

(define-object customer-tax-id ()
  id
  country
  created
  customer
  (type :reader tax-id-type)
  value
  verification)

(define-object tax-id-verification ()
  status
  verified-address
  verified-name)

(defmethod initialize-instance :after ((instance customer-tax-id) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:verification
           (when value
             (setf (slot-value instance '%verification)
                   (make-instance 'tax-id-verification :data value)))))))))
