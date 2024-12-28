(in-package #:stripe)

(defmacro define-type (name &key list-type)
  "Define type predicates and types for Stripe objects.

For base objects (when :list-type is nil):
- <name>-p: Type predicate for single object
- <name>-nullable-p: Predicate for null or single object
- <name>-collection: Type for sequence of objects
- <name>-nullable-collection: Type for null or sequence of objects

For paginated API list objects (when :list-type is t):
- <name>-p: Type predicate for list container
- <name>-nullable-p: Predicate for null or list container

Arguments:
  NAME: Symbol, the base name of the type to define
  LIST-TYPE: Boolean, whether to define an API list type (default: nil)"
  (flet ((make-predicate (base suffix)
           (alex:symbolicate base '- suffix))
         (make-type (base suffix)
           (alex:symbolicate base '- suffix)))
    (let* ((type-p (make-predicate name 'p))
           (nullable-p (make-predicate name 'nullable-p))
           (vowel-start-p (find (char (string name) 0) "aeiouAEIOU")))
      `(progn
         (defun ,type-p (x)
           ,(format nil "Predicate for ~A objects.~%~
                        Returns T if X is ~A ~A object."
                    name (if vowel-start-p "an" "a") name)
           (typep x ',name))
         (defun ,nullable-p (x)
           ,(format nil "Predicate for nullable ~A objects.~%~
                        Returns T if X is null or ~A ~A object."
                    name (if vowel-start-p "an" "a") name)
           (or (null x) (typep x ',name)))
         ,@(unless list-type
             (let* ((collection (make-type name 'collection))
                    (collection-p (make-predicate name 'collection-p))
                    (nullable-collection (make-type name 'nullable-collection))
                    (nullable-collection-p (make-predicate name 'nullable-collection-p)))
               `((defun ,collection-p (l)
                   ,(format nil "Predicate for sequences of ~A objects.~%~
                              Returns T if L is a sequence where every element ~
                              is ~A ~A object."
                            name (if vowel-start-p "an" "a") name)
                   (and (typep l 'sequence) (every #',type-p l)))
                 (deftype ,collection ()
                   ,(format nil "Type for sequences of ~A objects.~%~
                              Satisfies ~(~A-collection-p~)."
                            name name)
                   '(satisfies ,collection-p))
                 (defun ,nullable-collection-p (l)
                   ,(format nil "Predicate for nullable sequences of ~A objects.~%~
                              Returns T if L is null or a sequence where every ~
                              element is ~A ~A object."
                            name (if vowel-start-p "an" "a") name)
                   (or (null l) (and (listp l) (every #',nullable-p l))))
                 (deftype ,nullable-collection ()
                   ,(format nil "Type for nullable sequences of ~A objects.~%~
                              Satisfies ~(~A-nullable-collection-p~)."
                            name name)
                   '(satisfies ,nullable-collection-p)))))))))
