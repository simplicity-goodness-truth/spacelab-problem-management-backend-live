interface yif_slpm_data_manager
  public .

  methods: get_problems_list
    importing
      it_filters            type /iwbep/t_mgw_select_option optional
      it_order              type /iwbep/t_mgw_sorting_order  optional
      ip_exclude_exp_fields type abap_bool optional
      ip_search_string      type string optional
    returning
      value(et_result)      type ycrm_order_tt_sl_problems
    raising
      ycx_crm_order_api_exc
      ycx_assistant_utilities_exc
      ycx_system_user_exc
      ycx_slpm_configuration_exc,

    get_problem
      importing
        ip_guid               type crmt_object_guid
        ip_exclude_exp_fields type abap_bool optional
      returning
        value(es_result)      type ycrm_order_ts_sl_problem
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
        ycx_assistant_utilities_exc
        ycx_slpm_configuration_exc,

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
        ip_text          type string optional
        ip_tdid          type tdid optional
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
        cs_problem               type ycrm_order_ts_sl_problem,

    get_list_of_support_teams
      returning
        value(rt_support_teams) type yslpm_tt_support_teams
      raising
        ycx_slpm_configuration_exc,

    get_frontend_constants
      returning
        value(rt_constants) type yslpm_tt_frontend_const,

    is_status_a_customer_action
      importing
        ip_status                 type j_estat
      returning
        value(rp_customer_action) type abap_bool,

    is_status_a_final_status
      importing
        ip_status              type j_estat
      returning
        value(rp_final_status) type abap_bool,

    get_final_status_codes
      returning
        value(rt_final_status_codes) type zcrm_order_tt_statuses,

    get_active_configuration
      returning
        value(ro_active_configuration) type ref to yif_slpm_configuration,

    is_problem_dispute_open
      importing
        ip_guid                  type crmt_object_guid
      returning
        value(rp_dispute_active) type abap_bool,

    open_problem_dispute
      importing
        ip_guid type crmt_object_guid
      raising
        ycx_slpm_configuration_exc
        ycx_slpm_data_manager_exc
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc
        ycx_system_user_exc,

    close_problem_dispute
      importing
        ip_guid type crmt_object_guid
      raising
        ycx_slpm_configuration_exc
        ycx_slpm_data_manager_exc
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc
        ycx_system_user_exc,

    get_problem_dispute_history
      importing
        ip_guid                   type crmt_object_guid
      returning
        value(rt_dispute_history) type yslpm_tt_dispute_hist
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    is_there_problem_dispute_hist
      importing
        ip_guid                       type crmt_object_guid
      returning
        value(rp_dispute_hist_exists) type abap_bool.

endinterface.
