interface yif_slpm_product
  public .

  methods:
    is_show_priority_set
      returning
        value(rt_show_priority_set) type abap_bool.

endinterface.
