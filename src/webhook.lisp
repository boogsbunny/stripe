(in-package #:stripe)

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
