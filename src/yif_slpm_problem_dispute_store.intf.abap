interface yif_slpm_problem_dispute_store
  public .

  methods:

    open_problem_dispute,

    close_problem_dispute,

    get_problem_dispute_history
      returning
        value(rt_dispute_history) type yslpm_tt_dispute_hist,

    is_problem_dispute_open
      returning
        value(rp_dispute_active) type abap_bool.

endinterface.
