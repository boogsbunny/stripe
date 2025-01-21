(in-package #:stripe)

(define-object balance-transaction ()
  "Balance transactions represent funds moving through your Stripe
account.

Stripe creates them for every type of transaction that enters or leaves
your Stripe account balance.

Related guide: [Balance transaction types]
(https://stripe.com/docs/reports/balance-transaction-types)"
  (id
   :type string)
  (amount
   :type integer)
  (available-on
   :type time:timestamp)
  (created
   :type time:timestamp)
  (currency
   :type string)
  (description
   :type (or string null))
  exchange-rate
  (fee
   :type integer)
  (fee-details
   :type fee-collection)
  (net
   :type integer)
  (reporting-category
   :type string)
  (source
   :type (or string null))
  (status
   :type string)
  (type
   :reader transaction-type
   :type string)
  (:list-type t))

(define-object fee ()
  (amount
   :type integer)
  (application
   :type (or string null))
  (currency
   :type string)
  (description
   :type (or string null))
  (type
   :reader fee-type
   :type string)
  (:list-type t))

(defmethod initialize-instance :after ((instance balance-transaction) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more key value)
          (next-entry)
        (unless more (return))
        (case key
          (:available-on
           (setf (slot-value instance '%available-on) (decode-timestamp value)))
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:fee-details
           (setf (slot-value instance '%fee-details)
                 (when value
                   (map 'list
                        (lambda (fee-data)
                          (make-instance 'fee :data fee-data))
                        value)))))))))
