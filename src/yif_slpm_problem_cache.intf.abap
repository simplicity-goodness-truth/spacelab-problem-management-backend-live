interface yif_slpm_problem_cache
  public .

  methods:

    add_record
      importing
        is_record type ycrm_order_ts_sl_problem
      raising
        ycx_slpm_configuration_exc,

    get_record
      importing
        ip_guid          type crmt_object_guid
      returning
        value(rs_record) type ycrm_order_ts_sl_problem
      raising
        ycx_slpm_configuration_exc,

    invalidate_record
      importing
        ip_guid type crmt_object_guid,

    get_all_records
      returning
        value(rt_records) type ycrm_order_tt_sl_problems.

endinterface.
