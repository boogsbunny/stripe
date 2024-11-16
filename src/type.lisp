(in-package #:stripe)

(defmacro define-type (name)
  "Define a type-checking function and a corresponding collection-checking function
and type.

NAME is the base name of the type."
  (let* ((type-p (u:symbolicate name '-p))
         (collection (u:symbolicate name '-collection))
         (collection-p (u:symbolicate name '-collection-p))
         (nullable-p (u:symbolicate name '-nullable-p))
         (nullable-collection (u:symbolicate name '-nullable-collection))
         (nullable-collection-p (u:symbolicate name '-nullable-collection-p))
         (collection-check `(and (listp l) (every #',type-p l)))
         (nullable-check `(or (null l) (and (listp l) (every #',nullable-p l)))))
    `(progn
       (defun ,type-p (x)
         (typep x ',name))
       (defun ,collection-p (l)
         ,collection-check)
       (deftype ,collection ()
         ,(format nil "Collection of ~A." name)
         '(satisfies ,collection-p))
       (defun ,nullable-p (x)
         (typep x ',name))
       (defun ,nullable-collection-p (l)
         ,nullable-check)
       (deftype ,nullable-collection ()
         ,(format nil "Nullable collection of ~A." name)
         '(satisfies ,nullable-collection-p)))))
