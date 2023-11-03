interface yif_crm_order_auto_stat_setter
  public .

  methods:

    process_orders_status_setting
      raising
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc.

endinterface.
