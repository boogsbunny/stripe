(in-package #:stripe)

(define-query retrieve-cash-balance-transaction (:type cash-balance-transaction)
  "Retrieve a customer's cash balance transaction.

Retrieves a specific cash balance transaction, which updated the
customer's cash balance."
  (:get "customers/~a/cash_balance_transactions/~a"
        (customer-id
         :type string
         :documentation "The ID of the customer who owns the cash balance
transaction.")
        (transaction-id
         :type string
         :documentation "The ID of the cash balance transaction to retrieve.")))

(define-query list-cash-balance-transactions (:type vector)
  "List a customer's cash balance transactions.

Returns a list of transactions that modified the customer's cash balance."
  (:get "customers/~a/cash_balance_transactions"
        (id
         :type string
         :documentation "The ID of the customer whose transactions to
retrieve."))
  (ending-before
   :type string
   :documentation "A cursor for use in pagination. `ending_before` is
an object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, starting with obj_bar,
your subsequent call can include `ending_before=obj_bar` in order to
fetch the previous page of the list.")
  (limit
   :type integer
   :documentation "A limit on the number of objects to be returned.
Limit can range between 1 and 100, and the default is 10.")
  (starting-after
   :type string
   :documentation "A cursor for use in pagination. `starting_after` is
an object ID that defines your place in the list. For instance, if you
make a list request and receive 100 objects, ending with obj_foo, your
subsequent call can include `starting_after=obj_foo` in order to fetch
the next page of the list."))

(define-query fund-customer-cash-balance (:type cash-balance-transaction)
  "Test helper for funding a customer's cash balance.

Creates an incoming testmode bank transfer for the customer's cash
balance."
  (:post "test_helpers/customers/~a/fund_cash_balance"
         (id
          :type string
          :documentation "The ID of the customer whose cash balance to fund."))
  (amount
   :type integer
   :required t
   :documentation "Amount to be used for this test cash balance
transaction. A positive integer representing how much to fund in the
smallest currency unit (e.g., 100 cents to fund $1.00 or 100 to fund
¥100, a zero-decimal currency).")
  (currency
   :type string
   :required t
   :documentation "Three-letter ISO currency code, in lowercase. Must
be a supported currency.")
  (reference
   :type string
   :documentation "A description of the test funding. This simulates
free-text references supplied by customers when making bank transfers
to their cash balance. You can use this to test how Stripe’s
reconciliation algorithm applies to different user inputs."))
