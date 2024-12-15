(uiop:define-package #:stripe
  (:use :cl)
  (:local-nicknames
   (#:alex #:alexandria)
   (#:jzon #:com.inuoe.jzon)
   (#:sera #:serapeum)
   (#:time #:local-time))
  (:documentation "The core package for Stripe API interaction."))
