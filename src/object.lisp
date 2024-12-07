(in-package #:stripe)

(defmacro define-object (name super-classes &body args)
  "Defines a Stripe API object class and its corresponding paginated
list container.

For each object defined (e.g., CHARGE), this macro:
1. Creates the main class inheriting from STRIPE-OBJECT
2. Automatically creates a paginated list container class (e.g.,
LIST-CHARGE)
3. Defines type predicates and collection types for both classes

The list container follows Stripe's ApiList<T> pattern, containing:
- data: A vector of the defined objects.
- has-more: Boolean indicating if more items exist beyond this list.
- object: Always set to \"list\".
- url: The URL where this list can be accessed.

Syntax:
  (define-object name super-classes &rest fields)

Fields are defined with the following syntax:
  (field-name &key reader type initform extra-initargs documentation)

Example:
  (define-object charge ()
    (id
     :type string
     :documentation \"Unique identifier for the charge.\")
    (amount
     :type integer
     :documentation \"Amount in cents.\"))

This creates:
- CHARGE class with specified fields.
- LIST-CHARGE class for paginated lists.
- Type predicates:
  - CHARGE-P
  - CHARGE-NULLABLE-P
  - LIST-CHARGE-P
  - LIST-CHARGE-NULLABLE-P
- Collection types:
  - CHARGE-COLLECTION
  - CHARGE-NULLABLE-COLLECTION
  - LIST-CHARGE-COLLECTION
  - LIST-CHARGE-NULLABLE-COLLECTION

The list container can be used in other objects:
  (define-object customer ()
    (charges
     :type list-charge-collection
     :documentation \"List of charges for this customer.\"))

See: https://stripe.com/docs/api/pagination"
  (alex:with-gensyms (stream)
    (let* ((doc-string (when (stringp (first args)) (first args)))
           (fields (if doc-string (rest args) args))
           (slot-readers
             (remove nil
                     (mapcar (lambda (x)
                               (let* ((name (alex:ensure-list x))
                                      (slot-name (first name))
                                      (explicit-reader (getf (rest name) :reader)))
                                 (or explicit-reader slot-name)))
                             fields)))
           (slots (mapcar
                   (lambda (x)
                     (let ((name (alex:ensure-list x)))
                       (destructuring-bind (name
                                            &key (reader name) (type t)
                                              (initform nil) extra-initargs (documentation ""))
                           name
                         `(,(alex:symbolicate '#:% name)
                           :reader ,reader
                           :initarg ,(make-keyword name)
                           :type ,type
                           ,@(when (and initform (not (eq initform nil)))
                               `(:initform ,initform))
                           ,@(when extra-initargs
                               `(,@(mapcan
                                    (lambda (x)
                                      `(:initarg ,x))
                                    extra-initargs)))
                           :documentation ,documentation))))
                   fields))
           (list-name (alex:symbolicate 'list- name))
           (list-slots `((,(alex:symbolicate '#:% 'object)
                          :reader object
                          :initarg :object
                          :type string
                          :initform "list")
                         (,(alex:symbolicate '#:% 'data)
                          :reader data
                          :initarg :data
                          :type (vector ',name)
                          :documentation "The array of objects contained in the list.")
                         (,(alex:symbolicate '#:% 'has-more)
                          :reader has-more
                          :initarg :has-more
                          :type boolean
                          :documentation "Indicates whether there are more items beyond the ones in
this list.")
                         (,(alex:symbolicate '#:% 'url)
                          :reader url
                          :initarg :url
                          :type string
                          :documentation "The URL where this list can be accessed.")))
           (export-symbols (append (list name list-name)
                                   slot-readers
                                   (list 'object 'data 'has-more 'url)
                                   (list (alex:symbolicate name '-p)
                                         (alex:symbolicate name '-nullable-p)
                                         (alex:symbolicate list-name '-p)
                                         (alex:symbolicate list-name '-nullable-p)
                                         (alex:symbolicate name '-collection)
                                         (alex:symbolicate name '-nullable-collection)
                                         (alex:symbolicate list-name '-collection)
                                         (alex:symbolicate list-name '-nullable-collection)))))
      `(progn
         (defclass ,name
             ,@(if super-classes
                   `(,super-classes)
                   `((stripe-object)))
           ,slots
           ,@(when doc-string
               `((:documentation ,doc-string))))
         (define-printer (,name ,stream :type nil)
           (if (and (slot-exists-p ,name '%id)
                    (slot-boundp ,name '%id))
               (format ,stream "~a ~a" ',name (id ,name))
               (format ,stream "~a" ',name)))
         (define-type ,name)
         (defclass ,list-name (stripe-object)
           ,list-slots)
         (define-printer (,list-name ,stream :type nil)
           (format ,stream "~a" ',list-name))
         (define-type ,list-name)
         ,@(mapcar (lambda (symbol)
                     `(sera:export-always ',symbol :stripe))
                   export-symbols)))))

(defclass stripe-object () ())

(defmethod initialize-instance :after ((instance stripe-object) &key data &allow-other-keys)
  (apply #'reinitialize-instance
         instance
         :allow-other-keys t
         (if (hash-table-p data)
             (let ((plist (alex:hash-table-plist data)))
               (loop for (key value) on plist by #'cddr
                     if (eql value 'null)
                       collect key and collect nil
                     else
                       collect key and collect value))
             (loop for (key value) on data by #'cddr
                   if (eql value 'null)
                     collect key and collect nil
                   else
                     collect key and collect value))))

(define-object address ()
  (line1
   :extra-initargs (:address-line1)
   :type (or string null))
  (line2
   :extra-initargs (:address-line2)
   :type (or string null))
  (city
   :extra-initargs (:address-city)
   :type (or string null))
  (state
   :extra-initargs (:address-state)
   :type (or string null))
  (postal-code
   :extra-initargs (:address-zip)
   :type (or string null))
  (country
   :extra-initargs (:address-country)
   :type (or string null)))

(define-object shipping ()
  (address
   :type (or address null))
  (carrier
   :type (or string null)
   :documentation "The delivery service that shipped a physical product, such as
Fedex, UPS, USPS, etc.")
  (name
   :type (or string null)
   :documentation "Recipient name.")
  (phone
   :type (or string null)
   :documentation "Recipient phone (including extension).")
  (tracking-number
   :type (or string null)
   :documentation "The tracking number for a physical product, obtained
from the delivery service. If multiple tracking numbers were generated
for this purchase, please separate them with commas."))

(defmethod initialize-instance :after ((instance shipping) &key data &allow-other-keys)
  (let ((address (gethash :address data)))
    (when address
      (reinitialize-instance
       instance
       :address (make-instance 'address :data address)))))

(define-object billing-details (shipping)
  (email
   :type (or string null)))

(define-object api-date-filter ()
  "Date range filter using Unix timestamps."
  (gt
   :type (or local-time:timestamp null)
   :documentation "Minimum value to filter by (exclusive).")
  (gte
   :type (or local-time:timestamp null)
   :documentation "Minimum value to filter by (inclusive).")
  (lt
   :type (or local-time:timestamp null)
   :documentation "Maximum value to filter by (exclusive).")
  (lte
   :type (or local-time:timestamp null)
   :documentation "Maximum value to filter by (inclusive)."))
