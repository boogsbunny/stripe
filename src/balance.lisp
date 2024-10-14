(in-package #:stripe)

(defun balance-funds-list-p (l)
  (or (null l)
      (and (listp l)
           (every #'balance-funds-p l))))

(defun balance-funds-p (x)
  (typep x 'balance-funds))

(defun instant-balance-funds-list-p (l)
  (or (null l)
      (and (listp l)
           (every #'instant-balance-funds-p l))))

(defun instant-balance-funds-p (x)
  (typep x 'instant-balance-funds))

(defun net-balance-funds-list-p (l)
  (or (null l)
      (and (listp l)
           (every #'net-balance-funds-p l))))

(defun net-balance-funds-p (x)
  (typep x 'net-balance-funds))

(deftype balance-funds-list ()
  '(satisfies balance-funds-list-p))

(deftype instant-balance-funds-list ()
  '(satisfies instant-balance-funds-list-p))

(deftype net-balance-funds-list ()
  '(satisfies net-balance-funds-list-p))

(define-object balance ()
  (available :type balance-funds-list)
  (pending :type balance-funds-list)
  (object :type string)
  (connect-reserved :type balance-funds-list)
  (instant-available :type instant-balance-funds-list)
  (issuing :type (or balance-issuing null))
  (livemode :type boolean))

(define-object balance-funds ()
  (amount :type integer)
  (currency :type string)
  (bank-account :type (or integer null))
  (card :type (or integer null))
  (fpx :type (or integer null)))

(define-object net-balance-funds ()
  (amount :type integer)
  (destination :type string)
  (bank-account :type (or integer null))
  (card :type (or integer null))
  (fpx :type (or integer null)))

(define-object instant-balance-funds ()
  (amount :type integer)
  (currency :type string)
  (bank-account :type (or integer null))
  (card :type (or integer null))
  (fpx :type (or integer null))
  (net-available :type net-balance-funds-list))

(define-object balance-issuing ()
  (available :type balance-funds-list))

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

(define-query retrieve-balance (:type balance)
  (:get "balance"))
