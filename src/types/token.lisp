(in-package #:stripe)

(define-object card-token ()
  "Tokenization is the process Stripe uses to collect sensitive card or
bank account details, or personally identifiable information (PII),
directly from your customers in a secure manner. A token representing
this information is returned to your server to use. Use our
[recommended payments integrations](https://stripe.com/docs/payments)
to perform this process on the client-side. This guarantees that no
sensitive card data touches your server, and allows your integration to
operate in a PCI-compliant way.

If you can't use client-side tokenization, you can also create tokens
using the API with either your publishable or secret API key. If your
integration uses this method, you're responsible for any PCI compliance
that it might require, and you must keep your secret API key safe.
Unlike with client-side tokenization, your customer's information isn't
sent directly to Stripe, so we can't determine how it's handled or
stored.

You can't store or use tokens more than once. To store card or bank
account information for later use, create [Customer]
(https://stripe.com/docs/api#customers) objects or [External accounts]
(https://stripe.com/api#external_accounts). [Radar]
(https://stripe.com/docs/radar), our integrated solution for automatic
fraud protection, performs best with integrations that use client-side
tokenization."
  id
  card
  client-ip
  created
  used)

(defmethod initialize-instance :after ((instance card-token) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:card
           (unless (eql 'null value)
             (setf (slot-value instance '%card)
                   (make-instance 'card :data value))))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value))))))))
