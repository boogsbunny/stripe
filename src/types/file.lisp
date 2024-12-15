(in-package #:stripe)

(define-object file ()
  "This object represents files hosted on Stripe's servers. You can
upload files with the
[create file](https://stripe.com/docs/api#create_file) request
(for example, when uploading dispute evidence). Stripe also creates
files independently (for example, the results of a [Sigma scheduled
query](https://stripe.com/docs/api#scheduled_queries)).

Related guide: [File upload guide](https://stripe.com/docs/file-upload)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "file"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (expires-at
   :type (or time:timestamp null)
   :documentation "The file expires and isn't available at this time in
epoch seconds.")
  (filename
   :type (or string null)
   :documentation "The suitable name for saving the file to a
filesystem.")
  (links
   :type (or list-file-link null)
   :documentation "A list of [file links]
(https://stripe.com/docs/api#file_links) that point at this file.")
  (purpose
   :type string
   :documentation "The [purpose]
(https://stripe.com/docs/file-upload#uploading-a-file) of the uploaded
file. One of `account_requirement`, `additional_verification`,
`business_icon`, `business_logo`, `customer_signature`,
`dispute_evidence`, `document_provider_identity_document`,
`finance_report_run`, `identity_document`,
`identity_document_downloadable`, `issuing_regulatory_reporting`,
`pci_document`, `selfie`, `sigma_scheduled_query`,
`tax_document_user_upload`, or `terminal_reader_splashscreen`.")
  (size
   :type integer
   :documentation "The size of the file object in bytes.")
  (title
   :type (or string null)
   :documentation "A suitable title for the document.")
  (type
   :reader file-type
   :type (or string null)
   :documentation "The returned file type (for example, `csv`, `pdf`,
`jpg`, or `png`).")
  (url
   :type (or string null)
   :documentation "Use your live secret API key to download the file
from this URL."))

(defmethod initialize-instance :after ((instance file) &key data &allow-other-keys)
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
          (:links
           (unless (eql 'null value)
             (setf (slot-value instance '%links)
                   (make-instance 'list-file-link :data value)))))))))

(define-object file-create-file-link-data ()
  (create
   :type boolean
   :documentation "Set this to `true` to create a file link for the
newly created file. Creating a link is only possible when the file’s
`purpose` is one of the following: `business_icon`, `business_logo`,
`customer_signature`, `dispute_evidence`,
`issuing_regulatory_reporting`, `pci_document`,
`tax_document_user_upload`, or `terminal_reader_splashscreen`.")
  (expires-at
   :type time:timestamp
   :documentation "The link isn’t available after this future
timestamp.")
  (metadata
   :type (hash-table :key-type string :value-type string)
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format."))
