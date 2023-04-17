interface YIF_BP_MASTER_DATA
  public .

  methods:

    get_bp_number
      returning
        value(rp_bp_num) type bu_partner,

   get_but000_record
      returning
        value(rs_but000)  type but000,

    get_contact_persons
      returning
        value(rt_contact_persons_bp) type crmt_bu_partner_t,

    is_contact_person_of
      returning
        value(rt_companies_bp) type crmt_bu_partner_t.



endinterface.
