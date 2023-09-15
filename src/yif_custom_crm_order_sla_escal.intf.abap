interface yif_custom_crm_order_sla_escal
  public .

  methods:

    process_escalations
      raising
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc,

    clear_escal_log
      importing
        ip_process_type type crmt_process_type.

endinterface.
