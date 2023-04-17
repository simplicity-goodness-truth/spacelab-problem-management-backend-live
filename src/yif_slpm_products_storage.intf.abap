interface yif_slpm_products_storage
  public .

  methods: get_all_slpm_products
    returning
      value(rt_all_slpm_products) type yslpm_tt_products.

endinterface.
