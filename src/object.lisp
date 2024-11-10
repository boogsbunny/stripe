(in-package #:stripe)

(defmacro define-object (name super-classes &body fields)
  (u:with-gensyms (stream)
    (let ((slots (mapcar
                  (lambda (x)
                    (let ((name (u:ensure-list x)))
                      (destructuring-bind (name
                                           &key (reader name) (type t)
                                             (initform nil) extra-initargs (documentation ""))
                          name
                        `(,(u:symbolicate '#:% name)
                          :reader ,reader
                          :initarg ,(u:make-keyword name)
                          :type ,type
                          ,@(when (and initform (not (eq initform nil)))
                              `(:initform ,initform))
                          ,@(when extra-initargs
                              `(,@(mapcan
                                   (lambda (x)
                                     `(:initarg ,x))
                                   extra-initargs)))
                          :documentation ,documentation))))
                  fields)))
      `(progn
         (defclass ,name
             ,@(if super-classes
                   `(,super-classes)
                   `((stripe-object)))
           ,slots)
         (u:define-printer (,name ,stream :type nil)
           (if (and (slot-exists-p ,name '%id)
                    (slot-boundp ,name '%id))
               (format ,stream "~a ~a" ',name (id ,name))
               (format ,stream "~a" ',name)))
         (define-type ,name)))))

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
   :type address-list)
  (name
   :type (or string null))
  (phone
   :type (or string null)))

(defmethod initialize-instance :after ((instance shipping) &key data &allow-other-keys)
  (let ((address (gethash :address data)))
    (when address
      (reinitialize-instance
       instance
       :address (make-instance 'address :data address)))))

(define-object billing-details (shipping)
  (email
   :type (or string null)))
