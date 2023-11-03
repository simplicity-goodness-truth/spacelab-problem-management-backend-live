interface yif_slpm_status_pairs
  public .

  methods:
    get_all_status_pairs
      returning
        value(rt_status_pairs) type yslpm_tt_status_pairs.

endinterface.
