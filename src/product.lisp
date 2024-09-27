(in-package #:stripe)

(define-object product ()
  id
  active
  created
  description
  images
  name
  package-dimensions
  shippable
  statement-descriptor
  unit-label
  updated
  url)

(define-object package-dimensions ()
  height
  (length :reader package-length)
  weight
  width)

(defmethod initialize-instance :after ((instance product) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:package-dimensions
           (unless (eql 'null value)
             (setf (slot-value instance '%package-dimensions)
                   (make-instance 'package-dimensions :data value))))
          (:updated
           (setf (slot-value instance '%updated) (decode-timestamp value))))))))

(define-query create-product (:type product)
  (:post "products")
  name
  active
  attributes
  caption
  description
  images
  package-dimensions
  shippable
  url)

(define-query retrieve-product (:type product)
  (:get "products/~a" product))

(define-query update-product (:type product)
  (:post "products/~a" product)
  active
  attributes
  caption
  description
  images
  name
  package-dimensions
  shippable
  statement-descriptor
  unit-label
  url)

(define-query list-products (:type vector)
  (:get "products")
  active
  created
  ending-before
  ids
  limit
  shippable
  starting-after
  url)

(define-query delete-product ()
  (:delete "products" product))
