(in-package #:stripe)

(define-query update-dispute (:type dispute)
  "When you get a dispute, contacting your customer is always the best
first step. If that doesn’t work, you can submit evidence to help us
resolve the dispute in your favor. You can do this in your [dashboard]
(https://dashboard.stripe.com/disputes), but if you prefer, you can use
the API to submit evidence programmatically.

Depending on your dispute type, different evidence fields will give you
a better chance of winning your dispute. To figure out which evidence
fields to provide, see our
[guide to dispute types](https://docs.stripe.com/disputes/categories)."
  (:post
   "disputes/~a"
   (id
    :type string
    :documentation "The ID of the dispute to update."))
  (evidence
   :type dispute-update-evidence
   :documentation "Evidence to upload, to respond to a dispute.
Updating any field in the hash will submit all fields in the hash for
review. The combined character count of all fields is limited to
150,000.")
  (metadata
   :documentation "Set of [key-value pairs]
(https://stripe.com/docs/api/metadata) that you can attach to an
object. This can be useful for storing additional information about the
object in a structured format. Individual keys can be unset by posting
an empty value to them. All keys can be unset by posting an empty value
o `metadata`.")
  (submit
   :type boolean
   :documentation "Whether to immediately submit evidence to the bank.
If `false`, evidence is staged on the dispute. Staged evidence is
visible in the API and Dashboard, and can be submitted to the bank by
making another request with this attribute set to `true` (the default)."))

(define-query retrieve-dispute (:type dispute)
  "Retrieves the dispute with the given ID."
  (:get
   "disputes/~a"
   (id
    :type string
    :documentation "The ID of the dispute to retrieve.")))

(define-query list-disputes (:type vector)
  "Returns a list of your disputes."
  (:get "disputes")
  (charge
   :type string
   :documentation "Only return disputes associated to the charge
specified by this charge ID.")
  (payment-intent
   :type string
   :documentation "Only return disputes associated to the PaymentIntent
specified by this PaymentIntent ID.")
  (created
   :type api-date-filter
   :documentation "Only return disputes that were created during the
given date interval.")
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

(define-query close-dispute (:type dispute)
  "Closing the dispute for a charge indicates that you do not have any
evidence to submit and are essentially dismissing the dispute,
acknowledging it as lost.

The status of the dispute will change from needs_response to lost.
Closing a dispute is irreversible."
  (:post
   "disputes/~a/close"
   (id
    :type string
    :documentation "The ID of the dispute to close.")))
