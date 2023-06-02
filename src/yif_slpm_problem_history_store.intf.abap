interface yif_slpm_problem_history_store
  public .

  methods:

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
