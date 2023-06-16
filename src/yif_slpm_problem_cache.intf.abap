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
        value(rt_records) type ycrm_order_tt_sl_problems,

    get_all_problems_guids
      returning
        value(rt_all_problems_guids) type ycrm_order_tt_guids,

    set_all_problems_guids
      importing
        it_all_problem_guids type ycrm_order_tt_guids,

    add_guid_to_cached_prob_guids
      importing
        ip_guid type crmt_object_guid,

    invalidate_cached_prob_guids.

endinterface.
