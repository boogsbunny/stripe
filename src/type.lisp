(in-package #:stripe)

(defmacro define-type (name &key (nullable nil) (doc ""))
  "Define a type-checking function and a corresponding list-checking function
and type.

NAME is the base name of the type.
NULLABLE indicates whether the list may contain NIL values.
DOC is a documentation string for the generated functions (optional)."
  (let* ((name-string (string-upcase (symbol-name name)))
         (type-name (intern (concatenate 'string name-string "-LIST")))
         (type-predicate (intern (concatenate 'string name-string "-LIST-P")))
         (type-specifier (intern (concatenate 'string name-string "-P")))
         (null-check (if nullable
                         `(or (null l) (and (listp l) (every #',type-specifier l)))
                         `(and (listp l) (every #',type-specifier l)))))
    `(progn
       (defun ,type-specifier (x)
         (typep x ',name))
       (defun ,type-predicate (l)
         ,null-check)
       (deftype ,type-name ()
         ,doc
         '(satisfies ,type-predicate)))))
