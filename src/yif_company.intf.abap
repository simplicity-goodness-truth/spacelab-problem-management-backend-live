interface YIF_COMPANY
  public .

  methods:

    get_company_bp_number
      returning
        value(rp_bp_num) type bu_partner,

    get_company_bp_address_string
      returning
        value(rp_bp_address) type string,

    get_company_email_address
      returning
        value(rp_bp_email_address) type ad_smtpadr,

    get_company_name
      returning
        value(rp_bp_company_name) type bu_mcname1,

    get_contact_persons
      returning
        value(rt_contact_persons_bp) type crmt_bu_partner_t.


endinterface.
