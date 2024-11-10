(in-package #:stripe)

(define-object customer ()
  id
  address
  balance
  created
  currency
  default-source
  delinquent
  description
  discount
  email
  name
  phone
  shipping
  sources
  subscriptions)

(define-object deleted-customer ()
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "customer"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (deleted
   :type boolean
   :initform t
   :documentation "Always true for a deleted object."))

(defmethod initialize-instance :after ((instance customer) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:address
           (unless (eql 'null value)
             (setf (slot-value instance '%address)
                   (make-instance 'address :data value))))
          (:shipping
           (unless (eql 'null value)
             (setf (slot-value instance '%shipping)
                   (make-instance 'shipping :data value))))
          (:sources
           (when value
             (setf (slot-value instance '%sources)
                   (decode-hash-table value))))
          (:subscriptions
           (when value
             (setf (slot-value instance '%subscriptions)
                   (decode-hash-table value)))))))))
