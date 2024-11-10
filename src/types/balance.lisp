(in-package #:stripe)

(define-object balance ()
  (available
   :type balance-funds-list
   :documentation "")
  (pending
   :type balance-funds-list
   :documentation "")
  (object
   :type string
   :documentation "")
  (connect-reserved
   :type balance-funds-list
   :documentation "")
  (instant-available
   :type instant-balance-funds-list
   :documentation "")
  (issuing
   :type (or balance-issuing null)
   :documentation "")
  (livemode
   :type boolean
   :documentation ""))

(define-object balance-funds ()
  (amount
   :type integer
   :documentation "")
  (currency
   :type string
   :documentation "")
  (bank-account
   :type (or integer null)
   :documentation "")
  (card
   :type (or integer null)
   :documentation "")
  (fpx
   :type (or integer null)
   :documentation ""))

(define-object net-balance-funds ()
  (amount
   :type integer
   :documentation "")
  (destination
   :type string
   :documentation "")
  (bank-account
   :type (or integer null)
   :documentation "")
  (card
   :type (or integer null)
   :documentation "")
  (fpx
   :type (or integer null)
   :documentation ""))

(define-object instant-balance-funds ()
  (amount
   :type integer
   :documentation "")
  (currency
   :type string
   :documentation "")
  (bank-account
   :type (or integer null)
   :documentation "")
  (card
   :type (or integer null)
   :documentation "")
  (fpx
   :type (or integer null)
   :documentation "")
  (net-available
   :type net-balance-funds-list
   :documentation ""))

(define-object balance-issuing ()
  (available
   :type balance-funds-list
   :documentation ""))

(defmethod initialize-instance :after ((instance balance-funds) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:amount
           (setf (slot-value instance '%amount) value))
          (:currency
           (setf (slot-value instance '%currency) value))
          (:source-types
           (setf (slot-value instance '%bank-account) (gethash :bank-account value nil)
                 (slot-value instance '%card) (gethash :card value nil)
                 (slot-value instance '%fpx) (gethash :card value nil))))))))

(defmethod initialize-instance :after ((instance balance) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:available
           (setf (slot-value instance '%available)
                 (map 'list
                      (lambda (x)
                        (make-instance 'balance-funds :data x))
                      value)))
          (:pending
           (setf (slot-value instance '%pending)
                 (map 'list
                      (lambda (x)
                        (make-instance 'balance-funds :data x))
                      value)))
          (:connect-reserved
           (setf (slot-value instance '%connect-reserved)
                 (map 'list
                      (lambda (x)
                        (make-instance 'balance-funds :data x))
                      value)))
          (:instant-available
           (setf (slot-value instance '%instant-available)
                 (map 'list
                      (lambda (x)
                        (make-instance 'instant-balance-funds :data x))
                      value)))
          (:issuing
           (when value
             (let ((issuing-instance (make-instance 'balance-issuing :data value)))
               (setf (slot-value instance '%issuing) issuing-instance))))
          (:object
           (setf (slot-value instance '%object) value))
          (:livemode
           (setf (slot-value instance '%livemode) value)))))))
