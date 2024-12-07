(in-package #:stripe)

(defvar *base-url* "https://api.stripe.com/v1")

(sera:export-always '*api-version* :stripe)
(defvar *api-version* "2019-05-16")

(sera:export-always '*api-key* :stripe)
(defvar *api-key*)

(sera:export-always '*webhook-secret* :stripe)
(defvar *webhook-secret* "https://docs.stripe.com/webhooks/signatures")

(defun make-keyword (object)
  "Interns `OBJECT`, a string designator or number, into the keyword
package."
  (values
   (alex:make-keyword
    (etypecase object
      ((or string symbol)
       object)
      (number
       (format nil "~a" object))))))

(deftype plist () '(satisfies plist-p))

(defun plist-p (item)
  "Check whether or not `ITEM` is a property list."
  (and (listp item)
       (evenp (length item))))

(defun plist-remove (plist &rest keys)
  "Remove all `KEYS` and their associated values from `PLIST`.
Non-destructive."
  (if (plist-p plist)
      (loop :for (key value) :on plist :by #'cddr
            :unless (member key keys :test #'eq)
              :append (list key value))
      (error "~a is not a property list." plist)))

(defun alist->plist (alist)
  "Convert `ALIST` to a property list. A property list in this context
has keyword symbols for its keys."
  (mapcan
   (lambda (x)
     (list (alex:make-keyword (car x)) (cdr x)))
   (copy-list alist)))

(defun dict (&rest keys/values)
  "Create a hash table with `KEYS/VALUES`. If odd number of args, first
is TEST."
  (let* ((length (length keys/values))
         (test (if (oddp length)
                   (pop keys/values)
                   #'eq)))
    (alex:plist-hash-table keys/values
                           :test test
                           :size (truncate length 2))))

(defun href (table &rest keys)
  "Access nested `TABLE` with `KEYS`. Returns value at final key."
  (loop :for (key . rest) :on keys
        :unless rest
          :return (gethash key table)
        :do (setf table (gethash key table))))

(defun (setf href) (value table &rest keys)
  "Set `VALUE` in nested `TABLE` at `KEYS`. Returns `VALUE`."
  (loop :for (key . rest) :on keys
        :unless rest
          :return (setf (gethash key table) value)
        :do (setf table (gethash key table))))

(defun hash-keys (table)
  "Collect a list of all keys in the hash table `TABLE`."
  (let (keys)
    (alex:maphash-keys
     (lambda (x)
       (push x keys))
     table)
    (nreverse keys)))

(defmacro mvlet* ((&rest bindings) &body body)
  "Sequentially bind multiple values. Each binding can be a symbol or
list."
  (destructuring-bind (&optional car . cdr) bindings
    (typecase car
      (null
       `(locally ,@body))
      (list
       (case (length car)
         (0 (error "Missing variable in binding list."))
         ((1 2) `(let (,car) (mvlet* ,cdr ,@body)))
         (t `(multiple-value-bind ,(butlast car) ,(car (last car))
               (declare (ignorable ,@(butlast car)))
               (mvlet* ,cdr ,@body)))))
      (symbol
       `(let (,car)
          (declare (ignorable ,car))
          (mvlet* ,cdr ,@body))))))

(defmacro mvlet ((&rest bindings) &body body)
  "Bind multiple values in parallel. Each binding can be a symbol or
list."
  (labels ((process-list-binding (table binding)
             (loop :for symbol :in (butlast binding)
                   :collect (setf (href table symbol)
                                  (gensym (symbol-name symbol)))
                     :into new-symbols
                   :finally (return (append new-symbols (last binding)))))
           (process-symbol-binding (table binding)
             (setf (href table binding) (gensym (symbol-name binding))))
           (transform-bindings (bindings)
             (loop :with table = (dict)
                   :for binding :in bindings
                   :collect (typecase binding
                              (list (process-list-binding table binding))
                              (symbol (process-symbol-binding table binding)))
                     :into new-bindings
                   :finally (return (values new-bindings table)))))
    (multiple-value-bind (bindings mapping) (transform-bindings bindings)
      (let (new-bindings)
        `(mvlet* (,@bindings)
           (let (,@(progn
                     (maphash
                      (lambda (k v)
                        (push (list k v) new-bindings))
                      mapping)
                     (nreverse new-bindings)))
             (declare (ignorable ,@(hash-keys mapping)))
             ,@body))))))

(defmacro define-printer ((object stream &key (type t) identity) &body body)
  "Define a PRINT-OBJECT method for `OBJECT`."
  `(defmethod print-object ((,object ,object) ,stream)
     (print-unreadable-object (,object ,stream :type ,type :identity ,identity)
       ,@body)))

(defun normalize-string (string)
  (substitute #\- #\_ (string-upcase string) :test #'char=))

(defun normalize-json-key (string)
  (make-keyword (normalize-string string)))

(defun normalize-object-type (object-type)
  "Normalizes the object type string according to the rules specified.
If the object-type is like \"invoiceitem\" (ends with \"item\" but does
not contain an underscore), the function adds a hyphen before \"item\"
to match the naming convention for the corresponding class (e.g.,
\"invoiceitem\" becomes \"invoice-item\"). However, if the object-type
is like \"subscription_item\" (contains an underscore before \"item\"),
it is left unchanged."
  (if (and (stringp object-type)
           (not (search "_" object-type :test #'char=))
           (string= "item" (subseq object-type (- (length object-type) 4))))
      (concatenate 'string (subseq object-type 0 (- (length object-type) 4)) "-item")
      object-type))

(defun decode-hash-table (hash-table)
  "Decodes a hash table representing a list response from the Stripe
API.

The function extracts the :data and :has-more keys from the hash table
and returns two values:
1. A vector of objects corresponding to the items in the :data array.
2. The value of :has-more key, indicating if there are more items
available.

For each item in the :data array, the function determines the object
type based on the :object key. It then finds the corresponding class
symbol in the stripe package and creates an instance of that class
using the item hash table as the :data initialization argument.

Special case handling by `normalize-object-type`:
If the object-type is like \"invoiceitem\" (ends with \"item\" but
does not contain an underscore),the function adds a hyphen before
\"item\" to match the naming convention for the corresponding class
(e.g., \"invoiceitem\" becomes \"invoice-item\"). However, if the
object-type is like \"subscription_item\" (contains an underscore
before \"item\"), it is left unchanged.

If no matching class symbol is found, the item hash table is returned
as is."
  (let ((data (gethash :data hash-table))
        (has-more (gethash :has-more hash-table)))
    (values
     (map 'vector
          (lambda (item-hash)
            (let* ((object-type (gethash :object item-hash))
                   (normalized-type (normalize-object-type object-type))
                   (type-symbol (and normalized-type
                                     (find-symbol (normalize-string normalized-type)
                                                  :stripe))))
              (if type-symbol
                  (make-instance type-symbol :data item-hash)
                  item-hash)))
          data)
     has-more)))

(defun decode-timestamp (unix-time)
  "Decodes a Unix timestamp into a local-time:timestamp object.

If unix-time is the symbol 'null, the function returns nil. This is to
handle cases where a null value value might be encountered when parsing
data, as some libraries may represent null values using the 'null
symbol.

If unix-time is a valid Unix timestamp (either as an integer or
string), then the function converts it to a local-time:timestamp object
using the local-time:unix-to-timestamp function.

If unix-time is any other value, then the function returns nil."
  (cond
    ((eq unix-time 'null) nil)
    ((integerp unix-time) (local-time:unix-to-timestamp unix-time))
    ((stringp unix-time) (local-time:unix-to-timestamp (parse-integer unix-time)))
    (t nil)))
