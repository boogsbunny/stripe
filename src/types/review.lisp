(in-package #:stripe)

(define-object review ()
  "Reviews can be used to supplement automated fraud detection with
human expertise.

Learn more about [Radar](https://stripe.com/radar) and reviewing
payments [here](https://stripe.com/docs/radar/reviews)."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "review"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (billing-zip
   :type (or string null)
   :documentation "The ZIP or postal code of the card used, if
applicable.")
  (charge
   :type (or string charge null)
   :documentation "The charge associated with this review.")
  (closed-reason
   :type (or string null)
   :documentation "The reason the review was closed, or null if it has
not yet been closed. One of `approved`, `refunded`,
`refunded_as_fraud`, `disputed`, or `redacted`.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (ip-address
   :type (or string null)
   :documentation "The IP address where the payment originated.")
  (ip-address-location
   :type (or review-ip-address-location null)
   :documentation "Information related to the location of the payment.
Note that this information is an approximation and attempts to locate
the nearest population center - it should not be used to determine a
specific address.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (open
   :reader review-open
   :type boolean
   :documentation "If `true`, the review needs action.")
  (opened-reason
   :type string
   :documentation "The reason the review was opened. One of `rule` or
`manual`.")
  (payment-intent
   :type (or string payment-intent null)
   :initform nil
   :documentation "The PaymentIntent ID associated with this review,
if one exists.")
  (reason
   :type string
   :documentation "The reason the review is currently open or closed.
One of `rule`, `manual`, `approved`, `refunded`, `refunded_as_fraud`,
`disputed`, or `redacted`.")
  (session
   :type (or review-session null)
   :documentation "Information related to the browsing session of the
user who initiated the payment."))

(define-object review-ip-address-location ()
  (city
   :type (or string null)
   :documentation "The city where the payment originated.")
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the country where
the payment originated.")
  (latitude
   :type (or integer null)
   :documentation "The geographic latitude where the payment
originated.")
  (longitude
   :type (or integer null)
   :documentation "The geographic longitude where the payment
originated.")
  (region
   :type (or string null)
   :documentation "The state/county/province/region where the payment
originated."))

(define-object review-session ()
  (browser
   :type (or string null)
   :documentation "The browser used in this browser session (e.g.,
`Chrome`).")
  (device
   :type (or string null)
   :documentation "Information about the device used for the browser
session (e.g., `Samsung SM-G930T`).")
  (platform
   :type (or string null)
   :documentation "The platform for the browser session (e.g.,
`Macintosh`).")
  (version
   :type (or string null)
   :documentation "The version for the browser session (e.g.,
`61.0.3163.100`)."))
