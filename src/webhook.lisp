(in-package #:stripe)

(alex:define-constant +default-tolerance+ 300
  :documentation "Default tolerance for signatures in seconds.")

(alex:define-constant +signing-version+ "v1"
  :documentation "Current signature version.")

(define-object webhook-event ()
  (id :type string)
  (object :type string)
  (api-version :type string)
  (created :type local-time:timestamp)
  (type :type string :reader webhook-event-type)
  ;; TODO: Parse data into appropriate Stripe object, remove data field, and add all possible
  ;; Stripe objects that a webhook event can hold as slots.
  (data :type (hash-table :key-type string :value-type t))
  (livemode :type boolean)
  (pending-webhooks :type integer)
  (request :type (hash-table :key-type string :value-type t)))

(defmethod initialize-instance :after ((instance webhook-event) &key args &allow-other-keys)
  (if (hash-table-p args)
      (loop for key being the hash-keys of args
            for value being the hash-values of args
            for slot-name = (find-symbol (concatenate 'string "%" (string key)) :stripe)
            when (and slot-name (slot-exists-p instance slot-name))
              do (setf (slot-value instance slot-name)
                       (case slot-name
                         ('%created (decode-timestamp value))
                         (otherwise value)))
            else
              do (error "Slot ~S does not exist in class ~S"
                        (intern (concatenate 'string "%" (string key)) :keyword)
                        (class-name (class-of instance))))
      args))

(defun compute-signature (timestamp payload secret)
  "Compute a webhook signature using Stripe's v1 signing method."
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array secret) :sha256)))
    (ironclad:update-hmac hmac
                          (concatenate '(vector (unsigned-byte 8))
                                       (ironclad:ascii-string-to-byte-array
                                        (format nil "~D" (local-time:timestamp-to-unix timestamp)))
                                       #(46) ; ASCII code for '.'
                                       payload))
    (ironclad:hmac-digest hmac)))

(defun construct-webhook-event (payload header secret
                                &key (tolerance +default-tolerance+ tolerance-provided-p)
                                  ignore-api-version-mismatch)
  "Initialize an Event object from a JSON webhook payload, validating the
Stripe-Signature header using the specified signing secret and options."
  (when (and tolerance-provided-p (null tolerance))
    (setf tolerance +default-tolerance+))
  (unless (validate-webhook-payload payload
                                    header
                                    secret
                                    :tolerance tolerance
                                    :enforce-tolerance tolerance-provided-p)
    (error 'webhook-no-valid-signature))
  (let* ((json (jzon:parse payload :key-fn #'normalize-json-key))
         (event (make-instance 'webhook-event :args json)))
    (when (and (not ignore-api-version-mismatch)
               (slot-boundp event '%api-version)
               (string/= (api-version event) *api-version*))
      (error "Received webhook event with API version ~A, but stripe package expects API version ~A"
             (api-version event)
             *api-version*))
    event))

(defun validate-webhook-payload (payload header secret
                                 &key (tolerance +default-tolerance+)
                                   (enforce-tolerance t))
  "Validate the payload against the Stripe-Signature header using the specified
signing secret and options."
  (handler-case
      (destructuring-bind (timestamp signatures) (parse-signature-header header)
        (let ((expected-signature (compute-signature timestamp payload secret)))
          (when (and enforce-tolerance
                     (> (local-time:timestamp-difference (local-time:now) timestamp)
                        tolerance))
            (error 'webhook-timestamp-too-old))
          (some (lambda (sig) (equalp expected-signature sig)) signatures)))
    (webhook-no-valid-signature () nil)))

(defun parse-signature-header (header)
  "Parse the Stripe-Signature header into a timestamp and list of signatures."
  (if (string= header "")
      (error 'webhook-not-signed)
      (let ((timestamp nil)
            (signatures nil))
        (loop for pair in (uiop:split-string header :separator ",")
              for (key value) = (uiop:split-string pair :separator "=")
              do (cond ((string= key "t")
                        (setf timestamp (decode-timestamp value)))
                       ((string= key +signing-version+)
                        (push (handler-case (ironclad:hex-string-to-byte-array value)
                                (error () (continue)))
                              signatures))
                       (t (continue))))
        (if (and timestamp signatures)
            (list timestamp (remove nil signatures))
            (error 'webhook-invalid-header)))))
