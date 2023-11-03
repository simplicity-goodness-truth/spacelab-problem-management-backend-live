interface yif_slpm_sla_hist
  public .

  methods:

    get_last_sla_timestamp
      exporting
        ep_timestamp type  sc_tstfro
        ep_timezone  type sc_zonefro,

    get_sla_perc_of_pending_shift
      importing
        ip_status                 type j_estat
      exporting
        ep_pending_shift_exists   type abap_bool
        ep_pending_shift_sla_perc type int4,

    is_there_pending_shift
      importing
        ip_status                      type j_estat
      returning
        value(rp_pending_shift_exists) type abap_bool.

endinterface.
