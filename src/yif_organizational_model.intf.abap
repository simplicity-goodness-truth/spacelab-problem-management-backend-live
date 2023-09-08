interface yif_organizational_model
  public .

  types:
     ty_sub_units_struc type  table of struc.


  methods:

    get_subunits_of_root_org_unit
      exporting
        et_sub_units_struc type  ty_sub_units_struc
        et_sub_units       type crmt_orgman_swhactor_tab,

    get_assig_pos_of_root_org_unit
      importing
        ip_get_upper_levels type abap_bool optional
      returning
        value(rt_positions) type yorg_model_tt_positions,

    get_org_unit_code_and_text
      importing
        ip_org_unit      type pd_objid_r
        ip_otype         type otype
      exporting
        ep_org_unit_code type short_d
        ep_org_unit_text type stext,

    get_bp_of_org_unit
      importing
        ip_org_unit  type pd_objid_r
      returning
        value(ep_bp) type bu_partner,
    get_org_bp_name
      importing
        ip_bp          type bu_partner
      returning
        value(ep_name) type bu_nameor2,

    get_employee_org_unit
      importing
        ip_emp_org_unit    type pd_objid_r
      returning
        value(rt_org_unit) type crmt_orgman_swhactor_tab,

    get_upper_units_of_org_unit
      importing
        ip_org_unit          type pd_objid_r
      exporting
        et_upper_units_struc type  ty_sub_units_struc
        et_upper_units       type crmt_orgman_swhactor_tab,

    get_org_unit_of_bp
      importing
        ip_bp              type bu_partner
      returning
        value(ep_org_unit) type pd_objid_r.

endinterface.
