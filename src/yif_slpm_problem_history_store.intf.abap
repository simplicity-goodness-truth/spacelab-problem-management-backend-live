interface yif_slpm_problem_history_store
  public .

  methods:

    get_problem_history_headers
      returning
        value(rt_yslpm_pr_his_hdr) type yslpm_tt_pr_his_hdr,

    get_problem_history_records
      returning
        value(rt_yslpm_pr_his_rec) type yslpm_tt_pr_his_rec,

    get_problem_history_hierarchy
      returning
        value(rt_yslpm_pr_his_hry) type yslpm_tt_pr_his_hry,

    arch_orphaned_history_records
      raising
        ycx_slpm_data_manager_exc
        ycx_crm_order_api_exc
        ycx_assistant_utilities_exc
        ycx_slpm_configuration_exc
        ycx_system_user_exc,

    delete_arch_history_records
      importing
        ip_password type string,

    get_problem_flow_stat
      returning
        value(rt_problem_flow_stat) type yslpm_tt_pr_flow_stat
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc,

    get_dist_stco_count_by_stco
      importing
        ip_status_code  type j_estat
      returning
        value(rp_count) type int4.

endinterface.
