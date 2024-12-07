(in-package #:stripe)

(define-query create-file (:type file)
  "To upload a file to Stripe, you need to send a request of type
`multipart/form-data`. Include the file you want to upload in the
request, and the parameters for creating a file.

All of Stripe’s officially supported Client libraries support sending
`multipart/form-data`."
  (:post "files")
  (file
   :type file
   :required t
   :documentation "A file to upload. Make sure that the specifications
follow RFC 2388, which defines file transfers for the
`multipart/form-data protocol`.")
  (purpose
   :type string
   :required t
   :documentation "The [purpose]
(https://docs.stripe.com/file-upload#uploading-a-file) of the uploaded
file. One of:

`account_requirement` - Additional documentation requirements that can
be requested for an account.

`additional_verification` - Additional verification for custom
accounts.

`business_icon` - A business icon.

`business_logo` - A business logo.

`customer_signature` - Customer signature image.

`dispute_evidence` - Evidence to submit with a dispute response.

`identity_document` - A document to verify the identity of an account
owner during account provisioning.

`issuing_regulatory_reporting` - Additional regulatory reporting
requirements for Issuing.

`pci_document` - A self-assessment PCI questionnaire.

`tax_document_user_upload` - A user-uploaded tax document.

`terminal_reader_splashscreen` - Splashscreen to be displayed on
Terminal readers.")
  (file-link-data
   :type file-create-file-link-data
   :documentation "Optional parameters that automatically create a
[file link](https://docs.stripe.com/api/files/create#file_links) for
the newly created file."))

(define-query retrieve-file (:type file)
  "Retrieves the details of an existing file object. After you supply
a unique file ID, Stripe returns the corresponding file object. Learn
how to [access file contents]
(https://docs.stripe.com/file-upload#download-file-contents)."
  (:get
   "files/~a"
   (id
    :type string
    :documentation "The ID of the file to retrieve.")))

(define-query list-files (:type vector)
  "Returns a list of the files that your account has access to. Stripe
sorts and returns the files by their creation dates, placing the most
recently created files at the top."
  (:get "files")
  (purpose
   :type string
   :documentation "Filter queries by the file purpose. If you don’t
provide a purpose, the queries return unfiltered files.")
  (created
   :type api-date-filter
   :documentation "Only return files that were created during the given
date interval.")
  (ending-before
   :type string
   :documentation "A cursor for use in pagination. `ending_before` is
an object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, starting with `obj_bar`,
your subsequent call can include `ending_before=obj_bar` in order to
fetch the previous page of the list.")
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
