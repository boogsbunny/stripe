(in-package #:stripe)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (object-spec (:predicate object-spec-p))
    (list-type-disabled-p nil :type boolean)
    (slots nil :type list)
    (slot-readers nil :type list)))

(defmacro define-object (name super-classes &body args)
  "Defines a Stripe API object class and its paginated list container.

All top-level API resources support bulk fetches through \"list\" API
methods, so by default this macro creates both the main class and its
list container. The list container can be disabled with
(:list-type nil) for non-top-level objects.

For each object defined (e.g., CHARGE), this macro:
1. Creates the main class inheriting from STRIPE-OBJECT.
2. Creates a paginated list container class (e.g., LIST-CHARGE) format
by default.
3. Defines type predicates for both classes.

List Container Format (as per https://stripe.com/docs/api/pagination):
- object: String, always set to \"list\"
- data: Vector of objects, paginated by any request parameters
- has-more: Boolean indicating if more elements exist beyond this set
- url: The URL for accessing this list

Arguments:
  NAME          Symbol, the base name of the type to define
  SUPER-CLASSES List of parent classes (defaults to STRIPE-OBJECT)
  ARGS         Can include (:list-type nil) to disable list container,
               and field definitions:
                 (field-name &key reader type initform
                  extra-initargs documentation)

Example:
  (define-object charge ()
    (id
     :type string
     :documentation \"Unique identifier for the charge.\")
    (amount
     :type integer
     :documentation \"Amount in cents.\"))

  ;; For non-top-level objects, disable list container:
  (define-object event-data-object ()
    (:list-type nil) ; Can appear anywhere in body
    (object
     :type event-data-object
     :documentation \"The event data object.\"))

This creates:
- CHARGE class with specified fields
- LIST-CHARGE class for paginated lists
- Type predicates:
  - CHARGE-P, CHARGE-NULLABLE-P
  - LIST-CHARGE-P, LIST-CHARGE-NULLABLE-P
- Collection types:
  - CHARGE-COLLECTION, CHARGE-NULLABLE-COLLECTION

The list container can be used in other objects:
  (define-object customer ()
    (charges
     :type list-charge
     :documentation \"Paginated list of charges for this customer.\"))

List containers support Stripe's cursor-based pagination using:
- limit: Number of objects to return (1-100, default 10)
- starting_after: Object ID for fetching next page
- ending_before: Object ID for fetching previous page

All generated symbols are automatically exported from the :stripe
package, including readers, type predicates, and collection types."
  (alex:with-gensyms (stream)
    (labels ((compute-slot-readers (slots)
               "Extract reader names from slot specifications."
               (remove nil
                       (mapcar (lambda (slot)
                                 (let* ((spec (alex:ensure-list slot))
                                        (name (first spec))
                                        (explicit-reader (getf (rest spec) :reader)))
                                   (or explicit-reader name)))
                               slots)))

             (parse-object-specification (args)
               "Parse object specifications, extracting list-type directive and slots."
               (let* ((list-type-spec (find :list-type args :key #'alex:ensure-car))
                      (list-disabled-p (when list-type-spec
                                         (not (second list-type-spec))))
                      (slots (remove :list-type args :key #'alex:ensure-car))
                      (slot-readers (compute-slot-readers slots)))
                 (make-object-spec
                  :list-type-disabled-p list-disabled-p
                  :slots slots
                  :slot-readers slot-readers)))

             (compute-export-symbols (name list-name spec)
               "Compute symbols to export based on object specification."
               (append (list name)
                       (unless (object-spec-list-type-disabled-p spec)
                         (list list-name
                               'object 'data 'has-more 'url
                               (alex:symbolicate list-name '-p)
                               (alex:symbolicate list-name '-nullable-p)))
                       (object-spec-slot-readers spec)
                       (list (alex:symbolicate name '-p)
                             (alex:symbolicate name '-nullable-p)
                             (alex:symbolicate name '-collection)
                             (alex:symbolicate name '-nullable-collection)
                             (alex:symbolicate name '-collection-p)
                             (alex:symbolicate name '-nullable-collection-p))))

             (make-slot-definition (spec)
               "Generate a single slot definition with proper options."
               (destructuring-bind (name &key (reader name) (type t)
                                           initform extra-initargs (documentation ""))
                   (alex:ensure-list spec)
                 `(,(alex:symbolicate '#:% name)
                   :reader ,reader
                   :initarg ,(make-keyword name)
                   :type ,type
                   ,@(when initform
                       `(:initform ,initform))
                   ,@(when extra-initargs
                       (mapcan (lambda (x) `(:initarg ,x)) extra-initargs))
                   :documentation ,documentation)))

             (generate-slot-definitions (slots)
               "Generate slot definitions from specifications."
               (mapcar #'make-slot-definition slots))

             (generate-list-slots (element-type)
               "Generate standard list container slots for pagination."
               `((,(alex:symbolicate '#:% 'object)
                  :reader object
                  :initarg :object
                  :type string
                  :initform "list")
                 (,(alex:symbolicate '#:% 'data)
                  :reader data
                  :initarg :data
                  :type (vector ,element-type))
                 (,(alex:symbolicate '#:% 'has-more)
                  :reader has-more
                  :initarg :has-more
                  :type boolean)
                 (,(alex:symbolicate '#:% 'url)
                  :reader url
                  :initarg :url
                  :type string)))

             (generate-printer-methods (name list-name object-spec stream)
               "Generate printer methods using define-printer."
               `((define-printer (,name ,stream :type nil)
                   (if (and (slot-exists-p ,name '%id)
                            (slot-boundp ,name '%id))
                       (format ,stream "~A ~A" ',name (id ,name))
                       (format ,stream "~A" ',name)))
                 ,@(unless (object-spec-list-type-disabled-p object-spec)
                     `((define-printer (,list-name ,stream :type nil)
                         (format ,stream "~A" ',list-name))))))

             (generate-type-definitions (name list-name spec)
               `((define-type ,name)
                 ,@(unless (object-spec-list-type-disabled-p spec)
                     `((define-type ,list-name :list-type t))))))
      (let* ((documentation (when (stringp (first args)) (pop args)))
             (list-name (alex:symbolicate 'list- name))
             (object-spec (parse-object-specification args))
             (slots (generate-slot-definitions (object-spec-slots object-spec)))
             (list-slots (generate-list-slots name))
             (export-symbols (compute-export-symbols name list-name object-spec)))
        `(progn
           ;; define the main class
           (defclass ,name ,(or super-classes '(stripe-object))
             ,slots
             ,@(when documentation `((:documentation ,documentation))))
           ;; define the list container class if enabled
           ,@(unless (object-spec-list-type-disabled-p object-spec)
               `((defclass ,list-name (stripe-object)
                   ,list-slots)))
           ;; generate printer methods
           ,@(generate-printer-methods name list-name object-spec stream)
           ;; generate type definitions
           ,@(generate-type-definitions name list-name object-spec)
           ;; export all generated symbols
           ,@(mapcar (lambda (symbol)
                       `(sera:export-always ',symbol :stripe))
                     export-symbols))))))

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

(defmacro define-event (name (&key type) &body args)
  "Define a Stripe event class extending the base EVENT class.

NAME: Symbol, the event class name to define
TYPE: String, the specific event type (e.g., \"invoice.created\")
ARGS: Event field definitions

Example:
  (define-event invoice-created-event (:type \"invoice.created\")
    \"Occurs whenever a new invoice is created.\"
    (data
     :type invoice
     :documentation \"The created invoice object.\"))"
  `(define-object ,name (event)
     ,@(if (stringp (car args))
           (cons (car args)
                 (cons `(type
                         :reader event-type
                         :type string
                         :initform ,type
                         :documentation "The specific event type.")
                       (cdr args)))
           (cons `(type
                   :reader event-type
                   :type string
                   :initform ,type
                   :documentation "The specific event type.")
                 args))
     (:list-type nil)))

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
   :type (or time:timestamp null)
   :documentation "Minimum value to filter by (exclusive).")
  (gte
   :type (or time:timestamp null)
   :documentation "Minimum value to filter by (inclusive).")
  (lt
   :type (or time:timestamp null)
   :documentation "Maximum value to filter by (exclusive).")
  (lte
   :type (or time:timestamp null)
   :documentation "Maximum value to filter by (inclusive)."))
