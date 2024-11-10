(in-package #:stripe)

(define-object application ()
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "application"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (deleted
   :documentation "Indicates whether the object is deleted. Presence
indicates deletion.")
  (name
   :type (or string null)
   :documentation "The name of the application."))

(define-object deleted-application ()
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "application"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (deleted
   :initform t
   :documentation "Always true for a deleted object.")
  (name
   :type (or string null)
   :documentation "The name of the application."))
