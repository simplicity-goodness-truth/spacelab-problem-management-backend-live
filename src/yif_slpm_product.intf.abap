interface yif_slpm_product
  public .

  methods:
    is_show_priority_set
      returning
        value(rt_show_priority_set) type abap_bool,

    get_org_unit_for_updates
      returning value(rp_org_unit) type pd_objid_r,

    get_signature_for_updates
      returning value(rp_signature) type lxechar1024.

endinterface.
