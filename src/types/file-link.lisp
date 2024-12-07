(in-package #:stripe)

(define-object file-link ()
  "To share the contents of a `File` object with non-Stripe users, you
can create a `FileLink`. `FileLink`s contain a URL that you can use to
retrieve the contents of the file without authentication."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "file_link"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (created
   :type local-time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (expired
   :type boolean
   :documentation "Returns if the link is already expired.")
  (expires-at
   :type (or local-time:timestamp null)
   :documentation "Time that the link expires.")
  (file
   :type (or string file)
   :documentation "The file object this link points to.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format.")
  (url
   :type (or string null)
   :documentation "The publicly accessible URL to download the file."))

(defmethod initialize-instance :after ((instance file-link) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:expires-at
           (unless (eql 'null value)
             (setf (slot-value instance '%expires-at) (decode-timestamp value))))
          (:file
           (when (and value (not (eql 'null value)) (not (stringp value)))
             (setf (slot-value instance '%file)
                   (make-instance 'file :data value)))))))))
