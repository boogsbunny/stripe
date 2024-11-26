(in-package #:stripe)

(defun json-boolean-p (x)
  (or (eq x t)
      (eq x :true)
      (eq x :false)))

(deftype json-boolean () '(satisfies json-boolean-p))

(defgeneric encode-type (type value))

(defmethod encode-type ((type (eql :boolean)) value)
  (ecase value
    ((:true t) "true")
    (:false "false")))

(defmethod encode-type ((type (eql :number)) value)
  value)

(defmethod encode-type ((type (eql :string)) value)
  (encode-key value))

(defmethod encode-type ((type (eql :timestamp)) value)
  (local-time:timestamp-to-unix value))

(defmethod encode-type ((type (eql :object)) value)
  (id value))

(defun encode-key (key)
  (flet ((normalize (string)
           (substitute #\_ #\- string :test #'char=)))
    (etypecase key
      (string (normalize key))
      (keyword (normalize (string-downcase (symbol-name key)))))))

(defun encode-value (value)
  (etypecase value
    (json-boolean (encode-type :boolean value))
    (number (encode-type :number value))
    (string value)
    (local-time:timestamp (encode-type :timestamp value))
    (stripe-object (encode-type :object value))))

(defgeneric encode-parameter (type key value))

(defmethod encode-parameter (type key value)
  (cons (encode-key key)
        (encode-value value)))

(defmethod encode-parameter ((type (eql :dictionary)) key value)
  (loop :for (k v) :on value :by #'cddr
        :for parameter = (string-downcase (format nil "~a[~a]" key k))
        :if (typep v 'plist)
          :append (encode-parameter :dictionary parameter v)
        :else
          :collect (encode-parameter nil parameter v)))

(defmethod encode-parameter ((type (eql :array)) key value)
  (loop :for item :in value
        :for i :from 0
        :for parameter = (string-downcase (format nil "~a[~a]" key i))
        :append (encode-parameter :dictionary parameter item)))

(defmethod encode-parameter ((type (eql :list)) key value)
  (loop :for item :in value
        :for i :from 0
        :for parameter = (string-downcase (format nil "~a[~a]" key i))
        :collect (encode-parameter nil parameter item)))

(defun post-parameter (key value)
  (etypecase value
    (json-boolean (list (encode-parameter :boolean key value)))
    (number (list (encode-parameter :number key value)))
    (string (list (encode-parameter :string key value)))
    ((cons plist (or null cons)) (encode-parameter :array key value))
    (plist (encode-parameter :dictionary key value))
    (list (encode-parameter :list key value))
    (local-time:timestamp (list (encode-parameter :timestamp key value)))
    (stripe-object (list (encode-parameter :object key value)))))

(defun post-parameters (&rest parameters)
  (loop :for (k v) :on parameters :by #'cddr
        :append (post-parameter k v)))

(defun query (endpoint method &optional content)
  (let ((url (format nil "~a/~a" *base-url* endpoint)))
    (jzon:parse
     (handler-case
         (dex:request url
                      :method method
                      :basic-auth (list *api-key*)
                      :headers `(("Stripe-Version" . ,*api-version*))
                      :content content)
       (dex:http-request-failed (condition)
         (mvlet ((stripe-condition message (decode-error condition)))
           (error stripe-condition :message message))))
     :key-fn #'normalize-json-key)))

(defun generate-url (template url-args query-args)
  (let* ((query (alist->plist (apply #'post-parameters query-args)))
         (query-char (and query (if (find #\? template :test #'char=)
                                    #\&
                                    #\?))))
    (format nil "~?~@[~c~{~a=~a~^&~}~]"
            template
            (mapcar #'encode-value url-args)
            query-char
            query)))

(defmacro define-query (name (&key type) &body body)
  "Define a Stripe API query function.

This macro generates functions for making HTTP requests to the Stripe
API. It handles URL generation, parameter encoding, and response
processing.

NAME is a symbol naming the function to be defined.

TYPE is an optional response type which can be:
  :VECTOR - Response is decoded as a vector of stripe objects of the
            specified class.
  <class> - Response is instantiated as a single stripe object of the
            specified class.
  NIL     - Raw response is returned.

BODY contains the endpoint definition and parameter specifications:

The endpoint definition takes the form:
  (:GET|:POST url-template &rest url-params)
where URL-TEMPLATE is a format control string and URL-PARAMS are
substituted into the template. For GET requests, remaining parameters
become query parameters. For POST requests, remaining parameters become
the request body.

Parameters can be specified as either:
  symbol
  (name &key type documentation required)

Example:
  (define-query retrieve-customer (:type customer)
    \"Retrieve a customer.\"
    (:get \"customers/~a\"
     (id
      :type string
      :required t
      :documentation \"The ID of the customer to retrieve.\")))"
  (alex:with-gensyms (query-args content response)
    (let* ((doc-string (when (stringp (first body)) (first body)))
           (rest-body (if doc-string (rest body) body))
           (endpoint (first rest-body))
           (fields (rest rest-body)))
      (destructuring-bind (method url-template . url-params) endpoint
        (let* ((get-p (eq method :get))
               (post-p (eq method :post))
               (url-param-names (mapcar (lambda (param)
                                          (if (listp param) (first param) param))
                                        url-params))
               (url-keys (mapcar #'make-keyword url-param-names))
               (all-fields (append url-params fields))
               (field-specs (mapcar (lambda (field)
                                      (if (listp field)
                                          (list (first field)
                                                (getf (cdr field) :type)
                                                (getf (cdr field) :required)
                                                (getf (cdr field) :documentation))
                                          (list field nil nil nil)))
                                    all-fields))
               (field-names (mapcar #'first field-specs))
               ;; Separate required and optional fields
               (required-fields (remove-if-not #'third field-specs))
               (optional-fields (remove-if #'third field-specs))
               ;; Generate compile-time type declarations for required fields
               (required-type-decls
                 (mapcar (lambda (spec)
                           (destructuring-bind (name type _ __) spec
                             (declare (ignore _ __))
                             (when type
                               `(type ,type ,name))))
                         required-fields))
               ;; Generate runtime type checks only for optional fields
               (optional-type-checks
                 (remove nil
                         (mapcar (lambda (spec)
                                   (destructuring-bind (name type _ __) spec
                                     (declare (ignore _ __))
                                     (when type
                                       `(when (member ,(make-keyword name) args)
                                          (check-type ,name ,type)))))
                                 optional-fields))))
          `(defun ,name (&rest args &key ,@field-names)
             ,@(when doc-string
                 `(,doc-string))
             (declare (ignorable args ,@field-names)
                      (optimize (safety 3))
                      ;; Add compile-time type declarations
                      ,@required-type-decls)
             ;; Runtime checks only for optional fields
             ,@optional-type-checks
             (let* (,@(when (or get-p post-p)
                        `((,query-args (plist-remove args ,@url-keys))))
                    ,@(when post-p
                        `((,content (apply #'post-parameters ,query-args))))
                    (,response (query (generate-url ,url-template
                                                    ,(when url-param-names
                                                       `(remove nil (list ,@url-param-names)))
                                                    ,(when get-p
                                                       query-args))
                                      ,method
                                      ,@(when post-p
                                          `(,content)))))
               ,@(case type
                   (vector `((decode-hash-table ,response)))
                   ((nil) `(,response))
                   (t `((make-instance ',type :data ,response)))))))))))
