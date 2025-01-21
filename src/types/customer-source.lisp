(in-package #:stripe)

;; NOTE: The `source` object is deprecated.
(deftype customer-source ()
  '(or account bank-account card source))

(deftype deleted-customer-source ()
  '(or deleted-bank-account deleted-card))
