(in-package #:stripe)

(alex:define-constant +default-tolerance+ 300
  :documentation "Default tolerance for signatures in seconds.")

(alex:define-constant +signing-version+ "v1"
  :test #'equal
  :documentation "Current signature version.")

(define-object webhook-event ()
  (id
   :type string)
  (object
   :type string)
  (api-version
   :type string)
  (created
   :type local-time:timestamp)
  (type
   :reader webhook-event-type
   :type string)
  ;; TODO: Parse data into appropriate Stripe object, remove data field, and add all possible
  ;; Stripe objects that a webhook event can hold as slots.
  (data
   :type (hash-table :key-type string :value-type t))
  (livemode
   :type boolean)
  (pending-webhooks
   :type integer)
  (request
   :type (hash-table :key-type string :value-type t)))

(defmethod initialize-instance :after ((instance webhook-event) &key args &allow-other-keys)
  (if (hash-table-p args)
      (loop for key being the hash-keys of args
            for value being the hash-values of args
            for slot-name = (find-symbol (concatenate 'string "%" (string key)) :stripe)
            when (and slot-name (slot-exists-p instance slot-name))
              do (setf (slot-value instance slot-name)
                       (case slot-name
                         ('%created (decode-timestamp value))
                         (otherwise value)))
            else
              do (error "Slot ~S does not exist in class ~S"
                        (intern (concatenate 'string "%" (string key)) :keyword)
                        (class-name (class-of instance))))
      args))
