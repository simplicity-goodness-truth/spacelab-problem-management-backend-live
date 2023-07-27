interface yif_slpm_data_manager
  public .

  methods: get_problems_list
    importing
      it_filters       type /iwbep/t_mgw_select_option optional
      it_order         type /iwbep/t_mgw_sorting_order  optional
    returning
      value(et_result) type ycrm_order_tt_sl_problems
    raising
      ycx_crm_order_api_exc
      ycx_assistant_utilities_exc
      ycx_system_user_exc
      ycx_slpm_configuration_exc,

    get_problem
      importing
        ip_guid          type crmt_object_guid
        " io_slpm_problem_api type ref to ycl_slpm_problem_api optional
      returning
        value(es_result) type ycrm_order_ts_sl_problem
      raising
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc
        ycx_slpm_configuration_exc
        ycx_system_user_exc,

    get_texts
      importing
        ip_guid  type crmt_object_guid
      exporting
        et_texts type cl_ai_crm_gw_mymessage_mpc=>tt_text
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    get_attachments_list
      importing
        ip_guid                          type crmt_object_guid
      exporting
        value(et_attachments_list_short) type ict_crm_documents
        value(et_attachments_list)       type cl_ai_crm_gw_mymessage_mpc=>tt_attachment
      raising
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc
        ycx_system_user_exc,

    get_attachment
      importing
        ip_guid              type crmt_object_guid
        ip_loio              type string
        ip_phio              type string
      returning
        value(er_attachment) type aic_s_attachment_incdnt_odata
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    get_attachment_content
      importing
        ip_guid              type crmt_object_guid
        ip_loio              type string
        ip_phio              type string
      exporting
        value(er_stream)     type /iwbep/if_mgw_appl_types=>ty_s_media_resource
        value(er_attachment) type aic_s_attachment_incdnt_odata
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    create_attachment
      importing
        ip_guid       type crmt_object_guid
        ip_file_name  type string
        ip_mime_type  type string
        ip_content    type xstring
        ip_visibility type char1 optional
      raising
        ycx_slpm_configuration_exc
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    delete_attachment
      importing
        ip_guid type crmt_object_guid
        ip_loio type string
        ip_phio type string
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc
        ycx_assistant_utilities_exc,

    create_text
      importing
        ip_guid type crmt_object_guid
        ip_text type string
        ip_tdid type tdid
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    get_last_text
      importing
        ip_guid        type crmt_object_guid
      returning
        value(es_text) type cl_ai_crm_gw_mymessage_mpc=>ts_text
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    create_problem
      importing
        is_problem       type ycrm_order_ts_sl_problem
      returning
        value(rs_result) type ycrm_order_ts_sl_problem
      raising
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc
        ycx_slpm_data_manager_exc
        ycx_slpm_configuration_exc
        ycx_system_user_exc,

    get_all_priorities
      returning
        value(rt_priorities) type ycrm_order_tt_priorities
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    get_priorities_of_product
      importing
        ip_guid              type comt_product_guid
      returning
        value(rt_priorities) type ycrm_order_tt_priorities
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    update_problem
      importing
        ip_guid          type crmt_object_guid
        is_problem       type ycrm_order_ts_sl_problem
      returning
        value(rs_result) type ycrm_order_ts_sl_problem
      raising
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc
        ycx_slpm_data_manager_exc
        ycx_slpm_configuration_exc
        ycx_system_user_exc,

    get_list_of_possible_statuses
      importing
        ip_status          type j_estat
      returning
        value(rt_statuses) type ycrm_order_tt_statuses
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    get_all_statuses
      returning
        value(rt_statuses) type ycrm_order_tt_statuses
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    get_list_of_processors
      returning
        value(rt_processors) type yslpm_tt_users
      raising
        ycx_slpm_configuration_exc,

    get_list_of_companies
      returning
        value(rt_companies) type yslpm_tt_companies
      raising
        ycx_system_user_exc,

    get_frontend_configuration
      importing
        ip_application                   type char100
      returning
        value(rt_frontend_configuration) type yslpm_tt_frontend_config
      raising
        ycx_slpm_configuration_exc
        ycx_system_user_exc,

    get_problem_sla_irt_history
      importing
        ip_guid                   type crmt_object_guid
      returning
        value(rt_sla_irt_history) type yslpm_tt_irt_hist
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    get_problem_sla_mpt_history
      importing
        ip_guid                   type crmt_object_guid
      returning
        value(rt_sla_mpt_history) type yslpm_tt_mpt_hist
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    fill_cached_prb_calc_flds
      importing
        ip_guid    type crmt_object_guid
      changing
        cs_problem type ycrm_order_ts_sl_problem
      raising
        ycx_crm_order_api_exc
        ycx_slpm_configuration_exc,

    calc_non_stand_sla_status
      importing
        ip_seconds_in_processing type integer
        ip_created_at_user_tzone type comt_created_at_usr
      changing
        cs_problem               type ycrm_order_ts_sl_problem.




endinterface.
