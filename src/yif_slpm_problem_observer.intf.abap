interface yif_slpm_problem_observer
  public .
  methods:

    problem_created
      importing
        is_problem type ycrm_order_ts_sl_problem,

    problem_updated
      importing
        is_problem type ycrm_order_ts_sl_problem,

    attachment_uploaded
      importing
        ip_file_name type string,

    attachment_removed
      importing
        ip_file_name type string.

endinterface.
