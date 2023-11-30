interface yif_slpm_service_operations
  public .

  methods:

    clear_attachments_trash_bin,

    clear_problems_history
      importing ip_password type string
      raising
                ycx_crm_order_api_exc
                ycx_assistant_utilities_exc
                ycx_slpm_configuration_exc
                ycx_system_user_exc
                ycx_slpm_data_manager_exc,

    clear_attachments_vsblty_table,

    clear_escalation_log,

    archive_irt_history
      raising
        ycx_slpm_data_manager_exc
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc
        ycx_slpm_configuration_exc
        ycx_system_user_exc,

    clear_irt_history
      importing
        ip_password type string,

    archive_mpt_history
      raising
        ycx_slpm_data_manager_exc
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc
        ycx_slpm_configuration_exc
        ycx_system_user_exc,

    clear_mpt_history
      importing
        ip_password type string,

    archive_dispute_history
      raising
        ycx_slpm_data_manager_exc
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc
        ycx_slpm_configuration_exc
        ycx_system_user_exc,

    clear_dispute_history
      importing
        ip_password type string,

    display_custom_table_status.

endinterface.
