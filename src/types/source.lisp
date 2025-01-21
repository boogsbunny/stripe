(in-package :stripe)

(define-object source ()
  "`Source` objects allow you to accept a variety of payment methods.
They represent a customer's payment instrument, and can be used with
the Stripe API just like a `Card` object: once chargeable, they can be
charged, or can be attached to customers.

Stripe doesn't recommend using the deprecated [Sources API](https://stripe.com/docs/api/sources).
We recommend that you adopt the [PaymentMethods API](https://stripe.com/docs/api/payment_methods).
This newer API provides access to our latest features and payment
method types.

Related guides: [Sources API](https://stripe.com/docs/sources) and
[Sources & Customers](https://stripe.com/docs/sources/customers).")
