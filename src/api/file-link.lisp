(in-package #:stripe)

(define-query create-file-link (:type file-link)
  "Creates a new file link object."
  (:post "file_links")
  (file
   :type string
   :required t
   :documentation "The ID of the file. The file's `purpose` must be one
of the following: `business_icon`, `business_logo`,
`customer_signature`, `dispute_evidence`, `pci_document`, or
`tax_document_user_upload`.")
  (expires-at
   :type time:timestamp
   :documentation "The timestamp at which the link expires.")
  (metadata
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format. Individual keys can be unset by posting
an empty value to them. All keys can be unset by posting an empty value
o `metadata`."))

(define-query update-file-link (:type file-link)
  "Updates an existing file link object. Expired links can no longer be
updated."
  (:post
   "file_links/~a"
   (id
    :type string
    :documentation "The ID of the file link to update."))
  (expires-at
   :type (or string time:timestamp)
   :documentation "A future timestamp after which the link will no
longer be usable, or `now` to expire the link immediately.")
  (metadata
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format. Individual keys can be unset by posting
an empty value to them. All keys can be unset by posting an empty value
o `metadata`."))

(define-query retrieve-file-link (:type file-link)
  "Retrieves the file link with the given ID."
  (:get "file_links/~a"
   (id
    :type string
    :documentation "The ID of the file link to retrieve.")))

(define-query list-file-links (:type vector)
  "Returns a list of file links."
  (:get "file_links")
  (created
   :type api-date-filter
   :documentation "Only return file links that were created during the
given date interval.")
  (ending-before
   :type string
   :documentation "A cursor for use in pagination. `ending_before` is
an object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, starting with `obj_bar`,
your subsequent call can include `ending_before=obj_bar` in order to
fetch the previous page of the list.")
  (expired
   :type boolean
   :documentation "Filter links by their expiration status. By default,
Stripe returns all links.")
  (file
   :type string
   :documentation "Only return file links for the file specified by
this ID.")
  (limit
   :type integer
   :documentation "A limit on the number of objects to be returned.
Limit can range between 1 and 100, and the default is 10.")
  (starting-after
   :type string
   :documentation "A cursor for use in pagination. `starting_after` is
an object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, ending with `obj_foo`,
your subsequent call can include `starting_after=obj_foo` in order to
fetch the next page of the list."))
