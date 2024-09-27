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
        :if (typep v 'u:plist)
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
    ((cons u:plist (or null cons)) (encode-parameter :array key value))
    (u:plist (encode-parameter :dictionary key value))
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
         (u:mvlet ((stripe-condition message (decode-error condition)))
           (error stripe-condition :message message))))
     :key-fn #'normalize-json-key)))

(defun generate-url (template url-args query-args)
  (let* ((query (u:alist->plist (apply #'post-parameters query-args)))
         (query-char (and query (if (find #\? template :test #'char=)
                                    #\&
                                    #\?))))
    (format nil "~?~@[~c~{~a=~a~^&~}~]"
            template
            (mapcar #'encode-value url-args)
            query-char
            query)))

(defmacro define-query (name (&key type) &body (endpoint . fields))
  (u:with-gensyms (query-args content response)
    (destructuring-bind (method url-template . url-args) endpoint
      (let ((get-p (eq method :get))
            (post-p (eq method :post))
            (url-keys (mapcar #'u:make-keyword url-args)))
        `(defun ,name (&rest args &key ,@url-args ,@fields)
           (declare (ignorable args ,@fields))
           (let* (,@(when (or get-p post-p)
                      `((,query-args (u:plist-remove args ,@url-keys))))
                  ,@(when post-p
                      `((,content (apply #'post-parameters ,query-args))))
                  (,response (query (generate-url ,url-template
                                                  ,(when url-args
                                                     `(list ,@url-args))
                                                  ,(when get-p
                                                     query-args))
                                    ,method
                                    ,@(when post-p
                                        `(,content)))))
             ,@(case type
                 (vector `((decode-hash-table ,response)))
                 ((nil) `(,response))
                 (t `((make-instance ',type :data ,response))))))))))
