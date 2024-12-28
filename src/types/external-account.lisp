(in-package #:stripe)

(deftype deleted-external-account ()
  '(or deleted-bank-account deleted-card))

(deftype external-account ()
  '(or bank-account card))
