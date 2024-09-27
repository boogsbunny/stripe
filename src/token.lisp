(in-package #:stripe)

(define-object card-token ()
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

(define-query create-card-token (:type card-token)
  (:post "tokens")
  card)

(define-query retrieve-card-token (:type card-token)
  (:get "tokens/~a" card-token))
