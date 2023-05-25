interface yif_slpm_problem_history_store
  public .

  methods:

    add_creation_event_record
      importing
        is_problem type ycrm_order_ts_sl_problem,

    add_update_event_record
      importing
        is_problem type ycrm_order_ts_sl_problem,

    get_problem_history_headers
      returning
        value(rt_yslpm_pr_his_hdr) type yslpm_tt_pr_his_hdr,

    get_problem_history_records
      returning
        value(rt_yslpm_pr_his_rec) type yslpm_tt_pr_his_rec,

    get_problem_history_hierarchy
      returning
        value(rt_yslpm_pr_his_hry) type yslpm_tt_pr_his_hry.

endinterface.
