(in-package #:stripe)

(define-object tax-id ()
  "You can add one or multiple tax IDs to a
[customer](https://stripe.com/docs/api/customers) or account. Customer
and account tax IDs get displayed on related invoices and credit notes.

Related guides:
[Customer tax identification numbers](https://stripe.com/docs/billing/taxes/tax-ids),
[Account tax IDs](https://stripe.com/docs/invoicing/connect#account-tax-ids)"
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "tax_id"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (country
   :type (or string null)
   :documentation "Two-letter ISO code representing the country of the
tax ID.")
  (created
   :type time:timestamp
   :documentation "Time at which the object was created. Measured in
seconds since the Unix epoch.")
  (customer
   :type (or string customer null)
   :documentation "ID of the customer.")
  (deleted
   :type (or boolean null)
   :documentation "Always true for a deleted object.")
  (livemode
   :type boolean
   :documentation "Has the value `true` if the object exists in live
mode or the value `false` if the object exists in test mode.")
  (owner
   :type (or tax-id-owner null)
   :documentation "The account or customer the tax ID belongs to.")
  (type
   :reader tax-id-type
   :type string
   :documentation "Type of the tax ID. One of `ad_nrt`, `ae_trn`,
`ar_cuit`, `au_abn`, `au_arn`, `bg_uic`, `bh_vat`, `bo_tin`, `br_cnpj`,
`br_cpf`, `by_tin`, `ca_bn`, `ca_gst_hst`, `ca_pst_bc`, `ca_pst_mb`,
`ca_pst_sk`, `ca_qst`, `ch_uid`, `ch_vat`, `cl_tin`, `cn_tin`,
`co_nit`, `cr_tin`, `de_stn`, `do_rcn`, `ec_ruc`, `eg_tin`, `es_cif`,
`eu_oss_vat`, `eu_vat`, `gb_vat`, `ge_vat`, `hk_br`, `hr_oib`,
`hu_tin`, `id_npwp`, `il_vat`, `in_gst`, `is_vat`, `jp_cn`, `jp_rn`,
`jp_trn`, `ke_pin`, `kr_brn`, `kz_bin`, `li_uid`, `ma_vat`, `md_vat`,
`mx_rfc`, `my_frp`, `my_itn`, `my_sst`, `ng_tin`, `no_vat`, `no_voec`,
`nz_gst`, `om_vat`, `pe_ruc`, `ph_tin`, `ro_tin`, `rs_pib`, `ru_inn`,
`ru_kpp`, `sa_vat`, `sg_gst`, `sg_uen`, `si_tin`, `sv_nit`, `th_vat`,
`tr_tin`, `tw_vat`, `tz_vat`, `ua_vat`, `us_ein`, `uy_ruc`, `uz_tin`,
`uz_vat`, `ve_rif`, `vn_tin`, or `za_vat`. Note that some legacy tax
IDs have type `unknown`.")
  (value
   :type string
   :documentation "Value of the tax ID.")
  (verification
   :type (or tax-id-verification null)
   :documentation "Tax ID verification information.")
  (:list-type t))

(defmethod initialize-instance :after ((instance tax-id) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:created
           (setf (slot-value instance '%created) (decode-timestamp value)))
          (:customer
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%customer)
                   (make-instance 'customer :data value))))
          (:owner
           (unless (eql 'null value)
             (setf (slot-value instance '%owner)
                   (make-instance 'tax-id-owner :data value))))
          (:verification
           (unless (eql 'null value)
             (setf (slot-value instance '%verification)
                   (make-instance 'tax-id-verification :data value)))))))))

(define-object tax-id-owner ()
  (account
   :type (or string account null)
   :documentation "The account being referenced when `type` is
`account`.")
  (application
   :type (or string application null)
   :documentation "The Connect Application being referenced when `type`
is `application`.")
  (customer
   :type (or string customer null)
   :documentation "The customer being referenced when `type` is
`customer`.")
  (type
   :reader owner-type
   :type string
   :documentation "Type of owner referenced. One of `account`,
`application`, `customer`, or `self`."))

(defmethod initialize-instance :after ((instance tax-id-owner) &key data &allow-other-keys)
  (with-hash-table-iterator (next-entry data)
    (loop
      (multiple-value-bind (more-entries key value)
          (next-entry)
        (unless more-entries (return))
        (case key
          (:account
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%account)
                   (make-instance 'account :data value))))
          (:application
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%application)
                   (make-instance 'application :data value))))
          (:customer
           (when (and value (not (stringp value)))
             (setf (slot-value instance '%customer)
                   (make-instance 'customer :data value)))))))))

(define-object tax-id-verification ()
  (status
   :type string
   :documentation "Verification status, one of `pending`, `verified`,
`unverified`, or `unavailable`.")
  (verified-address
   :type (or string null)
   :documentation "Verified address.")
  (verified-name
   :type (or string null)
   :documentation "Verified name."))

(define-object deleted-tax-id ()
  "The DeletedTaxId object."
  (id
   :type string
   :documentation "Unique identifier for the object.")
  (object
   :type string
   :initform "tax_id"
   :documentation "String representing the object's type. Objects of
the same type share the same value.")
  (deleted
   :type boolean
   :initform t
   :documentation "Always true for a deleted object."))
