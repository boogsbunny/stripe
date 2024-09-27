(in-package #:stripe)

;;;; NOTE: The SKU API is DEPRECATED.

(define-object sku ()
  id
  active
  attributes
  created
  currency
  image
  inventory
  package-dimensions
  price
  product
  updated)

(define-object sku-inventory ()
  quantity
  (type :reader inventory-type)
  value)

(defmethod initialize-instance :after ((instance sku) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:inventory
           (unless (eql 'null value)
             (setf (slot-value instance '%inventory) (make-instance 'sku-inventory :data value))))
          (:package-dimensions
           (unless (eql 'null value)
             (setf (slot-value instance '%package-dimensions)
                   (make-instance 'package-dimensions :data value))))
          (:updated
           (setf (slot-value instance '%updated) (decode-timestamp value))))))))

(define-query create-sku (:type sku)
  (:post "skus")
  currency
  inventory
  price
  product
  active
  attributes
  image
  package-dimensions)

(define-query retrieve-sku (:type sku)
  (:get "skus/~a" sku))

(define-query update-sku (:type sku)
  (:post "skus/~a" sku)
  active
  attributes
  currency
  image
  inventory
  package-dimensions
  price
  product)

(define-query list-skus (:type vector)
  (:get "skus")
  active
  attributes
  ending-before
  ids
  in-stock
  limit
  product
  starting-after)

(define-query delete-sku ()
  (:delete "skus/~a" sku))
