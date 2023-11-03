interface yif_bp_master_data
  public .

  types: begin of ty_ms_but000,
           partner    type but000-partner,
           type       type but000-type,
           name_org2  type but000-name_org2,
           name_last  type but000-name_last,
           name_first type but000-name_first,
           name1_text type but000-name1_text,
           persnumber type but000-persnumber,
           mc_name1   type but000-mc_name1,
           addrcomm   type but000-addrcomm,
         end of ty_ms_but000.

  methods:

    get_bp_number
      returning
        value(rp_bp_num) type bu_partner,

    get_but000_record
      returning
        value(rs_but000) type ty_ms_but000,

    get_contact_persons
      returning
        value(rt_contact_persons_bp) type crmt_bu_partner_t,

    is_contact_person_of
      returning
        value(rt_companies_bp) type crmt_bu_partner_t.



endinterface.
