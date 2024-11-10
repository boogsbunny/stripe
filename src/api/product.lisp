(in-package #:stripe)

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
