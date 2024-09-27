(in-package #:stripe)

(defvar *base-url* "https://api.stripe.com/v1")

(defvar *api-version* "2019-05-16")

(defvar *api-key*)

(defvar *webhook-secret* "https://docs.stripe.com/webhooks/signatures")

(defun normalize-string (string)
  (substitute #\- #\_ (string-upcase string) :test #'char=))

(defun normalize-json-key (string)
  (u:make-keyword (normalize-string string)))

(defun normalize-object-type (object-type)
  "Normalizes the object type string according to the rules specified.
If the object-type is like \"invoiceitem\" (ends with \"item\" but does not
contain an underscore), the function adds a hyphen before \"item\" to match the
naming convention for the corresponding class (e.g., \"invoiceitem\" becomes
\"invoice-item\"). However, if the object-type is like \"subscription_item\"
(contains an underscore before \"item\"), it is left unchanged."
  (if (and (stringp object-type)
           (not (search "_" object-type :test #'char=))
           (string= "item" (subseq object-type (- (length object-type) 4))))
      (concatenate 'string (subseq object-type 0 (- (length object-type) 4)) "-item")
      object-type))

(defun decode-hash-table (hash-table)
  "Decodes a hash table representing a list response from the Stripe API.

The function extracts the :data and :has-more keys from the hash table and
returns two values:
1. A vector of objects corresponding to the items in the :data array.
2. The value of :has-more key, indicating if there are more items available.

For each item in the :data array, the function determines the object type based
on the :object key. It then finds the corresponding class symbol in the stripe
package and creates an instance of that class using the item hash table as the
:data initialization argument.

Special case handling by `normalize-object-type`:
If the object-type is like \"invoiceitem\" (ends with \"item\" but does not
contain an underscore),the function adds a hyphen before \"item\" to match the
naming convention for the corresponding class (e.g., \"invoiceitem\" becomes
\"invoice-item\"). However, if the object-type is like \"subscription_item\"
(contains an underscore before \"item\"), it is left unchanged.

If no matching class symbol is found, the item hash table is returned as is."
  (let ((data (gethash :data hash-table))
        (has-more (gethash :has-more hash-table)))
    (values
     (map 'vector
          (lambda (item-hash)
            (let* ((object-type (gethash :object item-hash))
                   (normalized-type (normalize-object-type object-type))
                   (type-symbol (and normalized-type
                                     (find-symbol (stripe::normalize-string normalized-type)
                                                  :stripe))))
              (if type-symbol
                  (make-instance type-symbol :data item-hash)
                  item-hash)))
          data)
     has-more)))

(defun decode-timestamp (unix-time)
  "Decodes a Unix timestamp into a local-time:timestamp object.

If unix-time is the symbol 'null, the function returns nil. This is to handle
cases where a null value value might be encountered when parsing data, as some
libraries may represent null values using the 'null symbol.

If unix-time is a valid Unix timestamp (either as an integer or string), then
the function converts it to a local-time:timestamp object using the
local-time:unix-to-timestamp function.

If unix-time is any other value, then the function returns nil."
  (cond
    ((eq unix-time 'null) nil)
    ((integerp unix-time) (local-time:unix-to-timestamp unix-time))
    ((stringp unix-time) (local-time:unix-to-timestamp (parse-integer unix-time)))
    (t nil)))
