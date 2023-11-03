class ycl_slpm_data_manager definition
  public
  final
  create public .
  public section.
    interfaces yif_slpm_data_manager.

    methods: constructor
      importing
        io_active_configuration type ref to yif_slpm_configuration
        io_system_user          type ref to yif_system_user optional
      raising
        ycx_slpm_configuration_exc
        ycx_system_user_exc
        ycx_crm_order_api_exc.

  protected section.
  private section.
    data:
      mo_slpm_problem_api            type ref to ycl_slpm_problem_api,
      mo_active_configuration        type ref to yif_slpm_configuration,
      mo_system_user                 type ref to yif_system_user,
      mo_log                         type ref to ycl_logger_to_app_log,
      mv_app_log_object              type balobj_d,
      mv_app_log_subobject           type balsubobj,
      mt_internal_pool               type yslpm_tt_users,
      mt_proc_pool_assigned_pos      type yorg_model_tt_positions,
      mo_slpm_cache_controller       type ref to yif_slpm_problem_cache,
      mt_cust_action_status_codes    type table of j_estat,
      mt_final_status_codes          type table of j_estat,
      mt_hold_irt_sla_status_codes   type table of j_estat,
      mt_hold_mpt_sla_status_codes   type table of j_estat,
      mt_reqconfenab_status_codes    type table of j_estat,
      mt_reqrepenab_status_codes     type table of j_estat,
      mt_requpdenab_status_codes     type table of j_estat,
      mt_reqwitenab_status_codes     type table of j_estat,
      mt_procedmodeenab_status_codes type table of j_estat,
      mt_procprichgenab_status_codes type table of j_estat,
      mt_procretwitenab_status_codes type table of j_estat.

    methods:

      add_problem_non_db_fields
        importing
          ip_exclude_exp_fields type abap_bool optional
        changing
          cs_problem            type ycrm_order_ts_sl_problem
        raising
          ycx_slpm_configuration_exc,

      fill_possible_problem_actions
        changing
          cs_problem type ycrm_order_ts_sl_problem,

      get_processors_pool_org_unit
        returning
          value(rp_processors_pool_org_unit) type pd_objid_r
        raising
          ycx_slpm_configuration_exc,

      get_distinct_comp_with_prod
        returning
          value(rt_companies) type crmt_bu_partner_t,

      is_requester_bp_in_intern_pool
        importing
          ip_requester_bp  type bu_partner
        returning
          value(rp_result) type bool
        raising
          ycx_slpm_configuration_exc,

      set_app_logger
        raising
          ycx_slpm_configuration_exc,

      get_stored_irt_perc
        importing
          ip_guid                   type crmt_object_guid
        returning
          value(rp_stored_irt_perc) type int4,

      get_stored_irt_timestamp
        importing
          ip_guid             type crmt_object_guid
        returning
          value(rp_timestamp) type timestamp,

      get_last_slpm_irt_hist
        importing
          ip_guid                 type crmt_object_guid
        returning
          value(rs_slpm_irt_hist) type yslpm_irt_hist,

      is_irt_history_available
        importing
          ip_guid             type crmt_object_guid
        returning
          value(rp_available) type bool,

      is_mpt_history_available
        importing
          ip_guid             type crmt_object_guid
        returning
          value(rp_available) type bool,

      get_problem_from_cache
        importing
          ip_guid           type crmt_object_guid
        returning
          value(rs_problem) type ycrm_order_ts_sl_problem
        raising
          ycx_slpm_configuration_exc,

      add_problem_to_cache
        importing
          is_problem type ycrm_order_ts_sl_problem
        raising
          ycx_slpm_configuration_exc,

      get_problem_through_cache
        importing
          ip_guid          type crmt_object_guid
        returning
          value(es_result) type ycrm_order_ts_sl_problem
        raising
          ycx_crm_order_api_exc
          ycx_assistant_utilities_exc
          ycx_slpm_configuration_exc
          ycx_system_user_exc,

      set_slpm_cache_controller
        raising
          ycx_slpm_configuration_exc,

      get_prob_guids_through_cache
        returning
          value(rt_problems_guids) type ycrm_order_tt_guids,

      filter_out_attachments
        importing
          ip_guid             type crmt_object_guid
          it_attachments_list type cl_ai_crm_gw_mymessage_mpc=>tt_attachment
        exporting
          et_attachments_list type cl_ai_crm_gw_mymessage_mpc=>tt_attachment
        raising
          ycx_system_user_exc,

      record_attachment_visibility
        importing
          ip_guid       type crmt_object_guid
          ip_loio       type sdok_docid
          ip_phio       type sdok_docid
          ip_visibility type char1,

      get_attachment_extra_vsbl
        importing
          ip_guid              type crmt_object_guid
          ip_loio              type sdok_docid
          ip_phio              type sdok_docid
        returning
          value(rp_visibility) type char1,

      set_cust_action_status_codes,

      set_final_status_codes,

      set_hold_irt_sla_status_codes,

      set_hold_mpt_sla_status_codes,

      set_reqconfenab_status_code,

      set_reqrepenab_status_codes ,

      set_requpdenab_status_codes,

      set_reqwitenab_status_codes,

      set_procedmodeena_status_codes,

      set_procprichgena_status_codes,

      set_procretwitena_status_codes,

      get_stored_mpt_perc
        importing
          ip_guid                   type crmt_object_guid
        returning
          value(rp_stored_mpt_perc) type int4,

      get_last_slpm_mpt_hist
        importing
          ip_guid                 type crmt_object_guid
        returning
          value(rs_slpm_mpt_hist) type yslpm_mpt_hist.

endclass.

class ycl_slpm_data_manager implementation.

  method set_app_logger.

    mv_app_log_object = mo_active_configuration->get_parameter_value( 'APP_LOG_OBJECT' ).
    mv_app_log_subobject = 'YDATAMANAGER'.

    mo_log = ycl_logger_to_app_log=>get_instance( ).
    mo_log->set_object_and_subobject(
          exporting
            ip_object    =   mv_app_log_object
            ip_subobject =   mv_app_log_subobject ).

  endmethod.

  method yif_slpm_data_manager~create_attachment.


    data: ls_loio type skwf_io,
          ls_phio type skwf_io.

    mo_slpm_problem_api->yif_custom_crm_order_create~create_attachment(
      exporting
          ip_content = ip_content
          ip_file_name = ip_file_name
          ip_guid = ip_guid
          ip_mime_type = ip_mime_type
      importing
          es_loio = ls_loio
          es_phio = ls_phio ).

    if ip_visibility is not initial.

      me->record_attachment_visibility(
         ip_guid = ip_guid
         ip_loio = ls_loio-objid
         ip_phio = ls_phio-objid
         ip_visibility = ip_visibility ).

    endif.

  endmethod.

  method yif_slpm_data_manager~create_problem.

    data:
      "lo_slpm_problem_api type ref to ycl_slpm_problem_api,
      lr_problem type ref to data,
      lv_guid    type crmt_object_guid.


    "  lo_slpm_problem_api = new ycl_slpm_problem_api(  ).

    get reference of is_problem into lr_problem.

    mo_slpm_problem_api->yif_custom_crm_order_create~create_with_std_and_cust_flds(
        exporting ir_entity = lr_problem
        importing
        ep_guid = lv_guid ).

    rs_result = me->yif_slpm_data_manager~get_problem( lv_guid ).

  endmethod.

  method yif_slpm_data_manager~create_text.


    mo_slpm_problem_api->yif_custom_crm_order_create~create_text(
        exporting
            ip_guid = ip_guid
            ip_tdid = ip_tdid
            ip_text = ip_text ).
  endmethod.


  method yif_slpm_data_manager~delete_attachment.

    mo_slpm_problem_api->yif_custom_crm_order_update~delete_attachment(
     exporting
            ip_guid = ip_guid
            ip_loio = ip_loio
            ip_phio = ip_phio ).

  endmethod.

  method yif_slpm_data_manager~get_all_priorities.

    rt_priorities = mo_slpm_problem_api->yif_custom_crm_order_read~get_all_priorities_list(  ).

  endmethod.

  method yif_slpm_data_manager~get_attachment.

    er_attachment = mo_slpm_problem_api->yif_custom_crm_order_read~get_attachment_by_keys(
        exporting
            ip_guid = ip_guid
            ip_loio = ip_loio
            ip_phio = ip_phio ).

  endmethod.

  method yif_slpm_data_manager~get_attachments_list.


    data lt_attachments_list type cl_ai_crm_gw_mymessage_mpc=>tt_attachment.

    mo_slpm_problem_api->yif_custom_crm_order_read~get_attachments_list_by_guid(
    exporting
     ip_guid = ip_guid
    importing
     et_attachments_list = lt_attachments_list
     et_attachments_list_short = et_attachments_list_short ).

    me->filter_out_attachments(
        exporting
            ip_guid = ip_guid
            it_attachments_list = lt_attachments_list
        importing
           et_attachments_list = et_attachments_list ).

  endmethod.

  method yif_slpm_data_manager~get_attachment_content.

    mo_slpm_problem_api->yif_custom_crm_order_read~get_attachment_content_by_keys(
      exporting
          ip_guid = ip_guid
          ip_loio = ip_loio
          ip_phio = ip_phio
          importing
          er_attachment = er_attachment
          er_stream = er_stream ).

  endmethod.

  method yif_slpm_data_manager~get_last_text.

    mo_slpm_problem_api->yif_custom_crm_order_read~get_last_text(
        exporting ip_guid = ip_guid ).

  endmethod.

  method yif_slpm_data_manager~get_priorities_of_product.

    data lo_service_product type ref to yif_crm_service_product.

    lo_service_product = new ycl_crm_service_product( ip_guid ).

    rt_priorities = lo_service_product->get_resp_profile_prio(  ).

  endmethod.

  method yif_slpm_data_manager~get_problem.

    data:
      "  lo_slpm_problem_api            type ref to ycl_slpm_problem_api,
      ls_sl_problem_standard_package type ycrm_order_ts,
      ls_sl_problem_custom_package   type ycrm_order_ts_sl_problem,
      lo_custom_fields               type ref to data,
      lt_sl_problem_custom_package   type standard table of ycrm_order_ts_sl_problem.

    field-symbols <ls_custom_fields> type any table.

    ls_sl_problem_standard_package = mo_slpm_problem_api->yif_custom_crm_order_read~get_standard_fields_by_guid( ip_guid ).

    " Getting additional fields package

    call method mo_slpm_problem_api->yif_custom_crm_order_read~get_custom_fields_by_guid
      exporting
        ip_guid   = ip_guid
      importing
        es_result = lo_custom_fields.

    if ( lo_custom_fields is bound ).

      assign lo_custom_fields->* to <ls_custom_fields>.

      lt_sl_problem_custom_package = <ls_custom_fields>.

      read table  lt_sl_problem_custom_package into ls_sl_problem_custom_package index 1.

    endif.

    " Filling output structure

    move-corresponding ls_sl_problem_custom_package to es_result.

    move-corresponding ls_sl_problem_standard_package to es_result.

    " Adding problem non-database fields

    add_problem_non_db_fields(
        exporting
            ip_exclude_exp_fields = ip_exclude_exp_fields
        changing
            cs_problem = es_result ).

  endmethod.

  method yif_slpm_data_manager~get_problems_list.

    data:

      "lo_slpm_problem_api type ref to ycl_slpm_problem_api,
      lt_crm_guids       type ycrm_order_tt_guids,
      ls_crm_order_ts    type  ycrm_order_ts,
      ls_result          like line of et_result,
      lv_include_record  type ac_bool,
      lr_entity          type ref to data,
      lo_sorted_table    type ref to data,
      lo_slpm_user       type ref to yif_slpm_user,
      lv_log_record_text type string,
      lv_product_id      type comt_product_id.

    field-symbols: <ls_sorted_table> type any table.

    if ( mo_active_configuration->get_parameter_value( 'USE_SNLRU_CACHE_FOR_PROBLEM_GUIDS_LIST' ) eq 'X').

      lt_crm_guids = get_prob_guids_through_cache(  ).

    else.

      lt_crm_guids = mo_slpm_problem_api->yif_custom_crm_order_read~get_guids_list(  ).

    endif.



    if lt_crm_guids is not initial.

      lo_slpm_user = new ycl_slpm_user( sy-uname ).

      loop at lt_crm_guids assigning field-symbol(<ls_crm_guid>).

        if ( mo_active_configuration->get_parameter_value( 'USE_SNLRU_CACHE' ) eq 'X').

          ls_result = me->get_problem_through_cache( <ls_crm_guid>-guid ).

          me->yif_slpm_data_manager~fill_cached_prb_calc_flds(
          exporting
              ip_guid = <ls_crm_guid>-guid
              changing
              cs_problem = ls_result ).

        else.

          ls_result = me->yif_slpm_data_manager~get_problem(
                   exporting
                     ip_guid = <ls_crm_guid>-guid
                     ip_exclude_exp_fields = ip_exclude_exp_fields

                     ).


        endif.

        " Filters processing

        if it_filters is not initial.

          lv_include_record = abap_true.

          get reference of ls_result into lr_entity.

          mo_slpm_problem_api->yif_custom_crm_order_organizer~is_order_matching_to_filters( exporting
                          ir_entity         = lr_entity
                          it_set_filters    = it_filters
                          changing
                            cp_include_record = lv_include_record
                      ).

          "   Executing filtering

          if lv_include_record eq abap_false.
            continue.
          endif.

        endif.

        " User can only see the companies for which he/she is authorized

        if ( lo_slpm_user->is_auth_to_view_company( ls_result-companybusinesspartner ) eq abap_false ).

          message e003(yslpm_data_manager) with sy-uname ls_result-companybusinesspartner into lv_log_record_text.

          mo_log->yif_logger~warn( lv_log_record_text  ).

          continue.

        endif.


        " User can only see the problems for which he/she is authorized

        lv_product_id = ls_result-productname.


        if ( lo_slpm_user->is_auth_to_view_product( lv_product_id ) eq abap_false ).

          message e006(yslpm_data_manager) with sy-uname lv_product_id into lv_log_record_text.


          mo_log->yif_logger~warn( lv_log_record_text  ).

          continue.

        endif.

        append ls_result to et_result.

      endloop.

      if it_order is not initial.

        get reference of et_result into lr_entity.

        mo_slpm_problem_api->yif_custom_crm_order_organizer~sort_orders(
          exporting
            ir_entity = lr_entity
            it_order  =     it_order
         receiving
            er_entity = lo_sorted_table
        ).

        assign lo_sorted_table->* to <ls_sorted_table>.

        et_result = <ls_sorted_table>.

      endif.

    endif.

  endmethod.

  method yif_slpm_data_manager~get_texts.

    mo_slpm_problem_api->yif_custom_crm_order_read~get_all_texts(
     exporting ip_guid = ip_guid
     importing et_texts = et_texts ).

  endmethod.

  method constructor.

    mo_active_configuration = io_active_configuration.
    mo_system_user = io_system_user.
    me->set_app_logger(  ).
    mo_slpm_problem_api = new ycl_slpm_problem_api( io_active_configuration ).
    me->set_slpm_cache_controller(  ).
    me->set_cust_action_status_codes(  ).
    me->set_final_status_codes( ).
    me->set_hold_irt_sla_status_codes(  ).
    me->set_hold_mpt_sla_status_codes(  ).
    me->set_reqconfenab_status_code(  ).
    me->set_reqrepenab_status_codes(  ).
    me->set_requpdenab_status_codes(  ).
    me->set_reqwitenab_status_codes(  ).
    me->set_procedmodeena_status_codes(  ).
    me->set_procprichgena_status_codes(  ).
    me->set_procretwitena_status_codes(  ).

  endmethod.


  method yif_slpm_data_manager~update_problem.

    data:
      lr_problem type ref to data,
      ls_problem type ycrm_order_ts_sl_problem.

    ls_problem = is_problem.

    get reference of ls_problem into lr_problem.

    mo_slpm_problem_api->yif_custom_crm_order_update~update_order(
         exporting
         ir_entity = lr_problem
         ip_guid = ip_guid ).

    rs_result = me->yif_slpm_data_manager~get_problem( ip_guid ).


  endmethod.


  method yif_slpm_data_manager~get_list_of_possible_statuses.

    data:
      lv_possible_status_list type char200,
      lt_possible_status_list type table of j_estat,
      lt_all_statuses         type ycrm_order_tt_statuses,
      ls_status               type ycrm_order_ts_status.

    lt_all_statuses = mo_slpm_problem_api->yif_custom_crm_order_read~get_all_statuses_list(  ).

    select single statuslist into lv_possible_status_list from yslpm_stat_flow
        where status eq ip_status.

    if lv_possible_status_list is not initial.

      split lv_possible_status_list at ';' into table lt_possible_status_list.

      " Adding current status into a list on a first place

      insert ip_status into lt_possible_status_list index 1.

    endif.

    try.

        loop at lt_possible_status_list assigning field-symbol(<ls_possible_status>).

          clear ls_status.

          ls_status = lt_all_statuses[ code = <ls_possible_status> ].

          append ls_status to rt_statuses.

        endloop.

      catch cx_sy_itab_line_not_found.

    endtry.



  endmethod.

  method add_problem_non_db_fields.

    data: lo_slpm_customer            type ref to yif_slpm_customer,
          lo_company                  type ref to yif_company,
          lv_duration_sec             type integer,
          lv_system_timezone          type timezone,
          lv_timestamp                type timestamp,
          lo_slpm_product             type ref to yif_slpm_product,
          lv_stored_irt_perc          type int4,
          lv_pending_irt_shift_exists type abap_bool,
          lv_stored_mpt_perc          type int4,
          lv_pending_mpt_shift_exists type abap_bool.

    me->fill_possible_problem_actions(
       changing
       cs_problem = cs_problem ).

    " Requester company name and system details

    if cs_problem-companybusinesspartner is not initial.

      lo_slpm_customer = new ycl_slpm_customer( cs_problem-companybusinesspartner ).

      lo_company ?= lo_slpm_customer.

      cs_problem-companyname = lo_company->get_company_name(  ).

      " Customer system details

      if cs_problem-sapsystemname is not initial.

        data lt_customer_systems type zslpm_tt_systems.

        try.

            lt_customer_systems = lo_slpm_customer->get_slpm_systems_of_customer(  ).

            cs_problem-sapsystemdescription = lt_customer_systems[ sapsystemname = cs_problem-sapsystemname ]-description.
            cs_problem-sapsystemrole = lt_customer_systems[ sapsystemname = cs_problem-sapsystemname ]-role.

          catch cx_sy_itab_line_not_found.

        endtry.

      endif.

    endif.

    " Created internally flag

    cs_problem-createdinternally = me->is_requester_bp_in_intern_pool( cs_problem-requestorbusinesspartner ).

    " SLAs on Hold flag

    cs_problem-irtslaonhold = cond abap_bool(
        when line_exists( mt_hold_irt_sla_status_codes[ table_line = cs_problem-status ] )  then abap_true
            else abap_false ).

    cs_problem-mptslaonhold = cond abap_bool(
        when line_exists( mt_hold_mpt_sla_status_codes[ table_line = cs_problem-status ] )  then abap_true
            else abap_false ).

    " Stored IRT percent

    if cs_problem-irtslaonhold eq abap_true.

      clear: lv_stored_irt_perc, lv_pending_irt_shift_exists.

      new ycl_slpm_sla_irt_hist( cs_problem-guid )->yif_slpm_sla_hist~get_sla_perc_of_pending_shift(
        exporting
            ip_status = cs_problem-status
        importing
            ep_pending_shift_sla_perc = lv_stored_irt_perc
            ep_pending_shift_exists = lv_pending_irt_shift_exists
      ).

      cs_problem-storedirtperc = cond int4(
             when lv_pending_irt_shift_exists eq abap_true then lv_stored_irt_perc
             else cs_problem-irt_perc
           ).


    endif.

    " Stored MPT percent

    if cs_problem-mptslaonhold eq abap_true.

      clear: lv_stored_mpt_perc, lv_pending_mpt_shift_exists.

      new ycl_slpm_sla_mpt_hist( cs_problem-guid )->yif_slpm_sla_hist~get_sla_perc_of_pending_shift(
        exporting
            ip_status = cs_problem-status
        importing
            ep_pending_shift_sla_perc = lv_stored_mpt_perc
            ep_pending_shift_exists = lv_pending_mpt_shift_exists
      ).

      cs_problem-storedmptperc = cond int4(
             when lv_pending_mpt_shift_exists eq abap_true then lv_stored_mpt_perc
             else cs_problem-mpt_perc
           ).

    endif.

    "Closed flag

    cs_problem-closed = cond abap_bool(
        when me->yif_slpm_data_manager~is_status_a_final_status( cs_problem-status ) then abap_true
            else abap_false
    ).


    " Show Priorities

    lo_slpm_product = new ycl_slpm_product( cs_problem-productguid ).

    cs_problem-showpriorities = lo_slpm_product->is_show_priority_set(  ).

    " IRT history available

    cs_problem-irthistoryavailable = cond abap_bool(
        when lv_pending_irt_shift_exists eq abap_true then abap_true
            else me->is_irt_history_available(  cs_problem-guid  )
    ).

    " MPT history available

    cs_problem-mpthistoryavailable = cond abap_bool(
    when lv_pending_mpt_shift_exists eq abap_true then abap_true
        else me->is_mpt_history_available(  cs_problem-guid  )
    ).

    " Default processing unit

    cs_problem-defaultprocessingorgunit = cond pd_objid_r(
        when lo_slpm_product->get_processing_org_unit( ) then
            lo_slpm_product->get_processing_org_unit( )
        else
            mo_active_configuration->get_parameter_value( 'PROCESSORS_POOL_ORG_UNIT_NUMBER' ) ).

    " Adding expensive fields filling

    if ip_exclude_exp_fields eq abap_false.

      " Total processing time

      cs_problem-totalproctimeminutes = 0.

      if ( cs_problem-closed eq abap_true ).

        " For statuses Withdrawn (E0010) and Confirmed (E0008) it is a difference between completion timestamp
        " and last change timestamp

        lv_duration_sec = zcl_assistant_utilities=>calc_duration_btw_timestamps(
          exporting
              ip_timestamp_1 = cs_problem-created_at
              ip_timestamp_2 = cs_problem-changedat ).

      else.

        " For statuses except Withdrawn (E0010) and Confirmed (E0008)  it is a difference between now and creation timestamp

        lv_system_timezone =  zcl_assistant_utilities=>get_system_timezone(  ).

        convert date sy-datum time sy-uzeit into time stamp lv_timestamp time zone lv_system_timezone.

        lv_duration_sec = zcl_assistant_utilities=>calc_duration_btw_timestamps(
          exporting
              ip_timestamp_1 = cs_problem-created_at
              ip_timestamp_2 = lv_timestamp ).

      endif.

      cs_problem-totalproctimeminutes = lv_duration_sec div 60.




      " Total response time in minutes

      if ( cs_problem-firstreactiontimestamp is not initial ).

        clear lv_duration_sec.

        " SLA IRT has been achieved

        lv_duration_sec = zcl_assistant_utilities=>calc_duration_btw_timestamps(
          exporting
              ip_timestamp_1 = cs_problem-created_at
              ip_timestamp_2 = cs_problem-firstreactiontimestamp ).


      else.

        " SLA IRT has been not achieved

        lv_system_timezone =  zcl_assistant_utilities=>get_system_timezone(  ).

        convert date sy-datum time sy-uzeit into time stamp lv_timestamp time zone lv_system_timezone.

        lv_duration_sec = zcl_assistant_utilities=>calc_duration_btw_timestamps(
          exporting
              ip_timestamp_1 = cs_problem-created_at
              ip_timestamp_2 = lv_timestamp ).

      endif.

      cs_problem-totalresptimeminutes = lv_duration_sec div 60.


    endif.

  endmethod.

  method fill_possible_problem_actions.

    " Requester confirmation: when is enabled

    cs_problem-requesterconfirmenabled  = cond abap_bool(
        when line_exists( mt_reqconfenab_status_codes[ table_line = cs_problem-status ] )  then abap_true
            else abap_false ).

    " Requester reply: when is enabled

    cs_problem-requesterreplyenabled = cond abap_bool(
        when line_exists( mt_reqrepenab_status_codes[ table_line = cs_problem-status ] )  then abap_true
            else abap_false ).

    " Requester update(extra data provision): when is enabled

    cs_problem-requesterupdateenabled = cond abap_bool(
            when line_exists( mt_requpdenab_status_codes[ table_line = cs_problem-status ] )  then abap_true
                else abap_false ).

    " Requester withdrawal: when is enabled

    cs_problem-requesterwithdrawenabled = cond abap_bool(
        when line_exists( mt_reqwitenab_status_codes[ table_line = cs_problem-status ] )  then abap_true
            else abap_false ).

    " Processor editing: when is enabled

    cs_problem-processoreditmodeenabled = cond abap_bool(
        when cs_problem-processorbusinesspartner = mo_system_user->get_businesspartner( ) then
            cond abap_bool(
                when line_exists( mt_procedmodeenab_status_codes[ table_line = cs_problem-status ] ) then abap_true
                    else abap_false )
        else abap_false ) .

    " ViaRight Requirement: take over should be possible in all statuses

    cs_problem-processortakeoverenabled = cond abap_bool(
        when cs_problem-processorbusinesspartner ne mo_system_user->get_businesspartner( ) then abap_true
            else abap_false ).

    " Processor priority change: when is enabled

    cs_problem-processorprioritychangeenabled = cond abap_bool(
        when cs_problem-processorbusinesspartner = mo_system_user->get_businesspartner( ) then
            cond abap_bool(
                when line_exists( mt_procprichgenab_status_codes[ table_line = cs_problem-status ] )  then abap_true
                    else abap_false )
        else abap_false ) .

    " Processor return from withdrawal: when is enabled

    cs_problem-processorreturnfromwithdrawal = cond abap_bool(
        when cs_problem-processorbusinesspartner = mo_system_user->get_businesspartner( ) then
            cond abap_bool(
                when line_exists( mt_procretwitenab_status_codes[ table_line = cs_problem-status ] )  then abap_true
                    else abap_false )
    else abap_false ) .


  endmethod.

  method yif_slpm_data_manager~get_list_of_processors.

    data:
      lv_proc_pool_org_unit        type pd_objid_r,
      lo_organizational_model      type ref to yif_organizational_model,
      ls_processor                 type yslpm_ts_user,
      lv_bp_in_processing          type bu_partner,
      lv_sup_team_level_in_org_str type int1,
      lv_sup_team_text_field_name  type name_komp.

    field-symbols:
        <fs_value> type any.

    if mt_proc_pool_assigned_pos is initial.

      lv_proc_pool_org_unit = me->get_processors_pool_org_unit(  ).

      if lv_proc_pool_org_unit is not initial.

        lo_organizational_model = new ycl_organizational_model( lv_proc_pool_org_unit ).

        mt_proc_pool_assigned_pos = lo_organizational_model->get_assig_pos_of_root_org_unit( abap_true ).

        sort mt_proc_pool_assigned_pos by businesspartner.

      endif.

    endif.


    if mt_proc_pool_assigned_pos is not initial.

      " Getting a level of support team from a configuration (default level is 2)

      lv_sup_team_level_in_org_str = mo_active_configuration->get_parameter_value( 'SUPPORT_TEAM_LEVEL_IN_ORG_STRUCTURE' ).

      if ( lv_sup_team_level_in_org_str is initial ) or ( lv_sup_team_level_in_org_str eq  0 ).

        lv_sup_team_level_in_org_str = 2.

      endif.

      lv_sup_team_text_field_name = |LEVEL| & |{ lv_sup_team_level_in_org_str }| & |OOBJIDTEXT|.

      loop at mt_proc_pool_assigned_pos assigning field-symbol(<ls_proc_pool_assigned_pos>).

        if ( lv_bp_in_processing is initial ).

          ls_processor-businesspartner = <ls_proc_pool_assigned_pos>-businesspartner.
          ls_processor-fullname = <ls_proc_pool_assigned_pos>-fullname.
          ls_processor-username = <ls_proc_pool_assigned_pos>-businesspartner.
          ls_processor-searchtag1 = <ls_proc_pool_assigned_pos>-stext.

          " Filling support team

          if <fs_value> is assigned.

            unassign <fs_value>.

          endif.

          assign component lv_sup_team_text_field_name of structure <ls_proc_pool_assigned_pos> to <fs_value>.

          if <fs_value> is assigned.

            ls_processor-searchtag2 = <fs_value>.

          endif.

          lv_bp_in_processing = <ls_proc_pool_assigned_pos>-businesspartner.

        elseif lv_bp_in_processing eq <ls_proc_pool_assigned_pos>-businesspartner.

          ls_processor-searchtag1 = |{ ls_processor-searchtag1 }| && |, | && |{ <ls_proc_pool_assigned_pos>-stext }|.

        else.

          append ls_processor to rt_processors.

          clear lv_bp_in_processing.

          ls_processor-businesspartner = <ls_proc_pool_assigned_pos>-businesspartner.
          ls_processor-fullname = <ls_proc_pool_assigned_pos>-fullname.
          ls_processor-username = <ls_proc_pool_assigned_pos>-businesspartner.
          ls_processor-searchtag1 = <ls_proc_pool_assigned_pos>-stext.

          " Filling support team

          if <fs_value> is assigned.

            unassign <fs_value>.

          endif.

          assign component lv_sup_team_text_field_name of structure <ls_proc_pool_assigned_pos> to <fs_value>.

          if <fs_value> is assigned.

            ls_processor-searchtag2 = <fs_value>.

          endif.

          lv_bp_in_processing = <ls_proc_pool_assigned_pos>-businesspartner.

        endif.

      endloop.

      append ls_processor to rt_processors.

    endif.


  endmethod.

  method get_processors_pool_org_unit.

    rp_processors_pool_org_unit = me->mo_active_configuration->get_parameter_value( 'PROCESSORS_POOL_ORG_UNIT_NUMBER' ).

  endmethod.

  method yif_slpm_data_manager~get_list_of_companies.

    data:
      lt_companies       type crmt_bu_partner_t,
      ls_company         type yslpm_ts_company,
      lo_company         type ref to yif_company,
      lo_slpm_user       type ref to yif_slpm_user,
      lv_log_record_text type string.

    " Get distinct companies from YSLPM_CUST_PROD

    lt_companies = me->get_distinct_comp_with_prod(  ).

    if lt_companies is not initial.

      lo_slpm_user = new ycl_slpm_user( sy-uname ).

      loop at lt_companies assigning field-symbol(<ls_company>).

        " User can only see the companies for which he/she is authorized

        if ( lo_slpm_user->is_auth_to_view_company( <ls_company> ) eq abap_false ).

          message e003(yslpm_data_manager) with sy-uname <ls_company> into lv_log_record_text.

          mo_log->yif_logger~warn( lv_log_record_text  ).

          continue.

        endif.

        ls_company-companybusinesspartner = <ls_company>.

        lo_company = new ycl_company( <ls_company> ).

        ls_company-companyname = lo_company->get_company_name(  ).

        append ls_company to rt_companies.

      endloop.

    endif.

  endmethod.

  method get_distinct_comp_with_prod.

    select customerbusinesspartner into table rt_companies from yslpm_cust_prod.

    sort rt_companies.

    delete adjacent duplicates from rt_companies.

  endmethod.

  method is_requester_bp_in_intern_pool.

    data: "lt_internal_pool type yslpm_tt_users,
          lo_requester_bp  type ref to yif_bp_master_data.


    if mt_internal_pool is initial.
      mt_internal_pool = me->yif_slpm_data_manager~get_list_of_processors(  ).
    endif.

    if mt_internal_pool is not initial.

      lo_requester_bp = new ycl_bp_master_data( ip_requester_bp ).

      if line_exists( mt_internal_pool[ businesspartner = lo_requester_bp->get_bp_number(  ) ] ).

        rp_result = abap_true.

      endif.

    endif.

  endmethod.

  method yif_slpm_data_manager~get_frontend_configuration.

    data lv_parameter_mask type char50.

    lv_parameter_mask = ip_application.

    lv_parameter_mask = switch char100( ip_application
              when 'yslpmmyprb'    then 'MYPROBLEMS'
              when 'yslpmcrprb'    then 'NEWPROBLEM'
              when 'yslpmprprb'    then 'PROCESSPROBLEM'
             ).

    if lv_parameter_mask is not initial.

      rt_frontend_configuration = mo_active_configuration->get_parameters_values_by_mask( lv_parameter_mask ).

    endif.

  endmethod.


  method get_last_slpm_irt_hist.

    select guid apptguid problemguid irttimestamp irttimezone irtperc
update_timestamp update_timezone
      from yslpm_irt_hist
      into corresponding fields of rs_slpm_irt_hist
     up to 1 rows
       where problemguid = ip_guid order by update_timestamp descending.

    endselect.

  endmethod.

  method get_stored_irt_timestamp.

    rp_timestamp = me->get_last_slpm_irt_hist( ip_guid )-irttimestamp.

  endmethod.

  method get_stored_irt_perc.

    rp_stored_irt_perc = me->get_last_slpm_irt_hist( ip_guid )-irtperc.

  endmethod.

  method yif_slpm_data_manager~get_all_statuses.

    rt_statuses = mo_slpm_problem_api->yif_custom_crm_order_read~get_all_statuses_list(  ).

  endmethod.

  method yif_slpm_data_manager~get_problem_sla_irt_history.

    data:
      lt_sla_irt_history type table of yslpm_irt_hist,
      ls_sla_irt_history type yslpm_ts_irt_hist,
      lt_statuses        type ycrm_order_tt_statuses,
      ls_status          type ycrm_order_ts_status,
      ls_priority        type ycrm_order_ts_priority,
      lt_priorities      type ycrm_order_tt_priorities.

    select guid apptguid problemguid irttimestamp irttimezone irtperc
        update_timestamp update_timezone statusin
        statusout priorityin priorityout username manualchange from yslpm_irt_hist into
        corresponding fields of table lt_sla_irt_history
        where problemguid = ip_guid.

    loop at lt_sla_irt_history assigning field-symbol(<ls_sla_irt_history>).

      move-corresponding <ls_sla_irt_history> to ls_sla_irt_history.

      try.

          lt_statuses = me->yif_slpm_data_manager~get_all_statuses(  ).
          lt_priorities = me->yif_slpm_data_manager~get_all_priorities( ).

          ls_sla_irt_history-statusintext = lt_statuses[ code = <ls_sla_irt_history>-statusin ]-text.
          ls_sla_irt_history-statusouttext = lt_statuses[ code = <ls_sla_irt_history>-statusout ]-text.

          ls_sla_irt_history-priorityintext = lt_priorities[ code = <ls_sla_irt_history>-priorityin ]-description.
          ls_sla_irt_history-priorityouttext = lt_priorities[ code = <ls_sla_irt_history>-priorityout ]-description.

          append ls_sla_irt_history to rt_sla_irt_history.

        catch cx_sy_itab_line_not_found.

      endtry.

    endloop.

  endmethod.

  method is_irt_history_available.

    select count( * )
      from yslpm_irt_hist up to 1 rows
      where problemguid = ip_guid.

    if sy-subrc eq 0.
      rp_available = abap_true.
    endif.

  endmethod.

  method is_mpt_history_available.

    select count( * )
    from yslpm_mpt_hist up to 1 rows
    where problemguid = ip_guid.

    if sy-subrc eq 0.
      rp_available = abap_true.
    endif.


  endmethod.

  method yif_slpm_data_manager~get_problem_sla_mpt_history.

    data:
      lt_sla_mpt_history type table of yslpm_mpt_hist,
      ls_sla_mpt_history type yslpm_ts_mpt_hist,
      lt_statuses        type ycrm_order_tt_statuses,
      ls_status          type ycrm_order_ts_status,
      ls_priority        type ycrm_order_ts_priority,
      lt_priorities      type ycrm_order_tt_priorities.

    select guid apptguid problemguid mpttimestamp mpttimezone mptperc
        update_timestamp update_timezone statusin
        statusout priorityin priorityout username manualchange from yslpm_mpt_hist into
        corresponding fields of table lt_sla_mpt_history
        where problemguid = ip_guid.

    loop at lt_sla_mpt_history assigning field-symbol(<ls_sla_mpt_history>).

      move-corresponding <ls_sla_mpt_history> to ls_sla_mpt_history.

      try.

          lt_statuses = me->yif_slpm_data_manager~get_all_statuses(  ).
          lt_priorities = me->yif_slpm_data_manager~get_all_priorities( ).

          ls_sla_mpt_history-statusintext = lt_statuses[ code = <ls_sla_mpt_history>-statusin ]-text.
          ls_sla_mpt_history-statusouttext = lt_statuses[ code = <ls_sla_mpt_history>-statusout ]-text.

          ls_sla_mpt_history-priorityintext = lt_priorities[ code = <ls_sla_mpt_history>-priorityin ]-description.
          ls_sla_mpt_history-priorityouttext = lt_priorities[ code = <ls_sla_mpt_history>-priorityout ]-description.

          append ls_sla_mpt_history to rt_sla_mpt_history.

        catch cx_sy_itab_line_not_found.

      endtry.

    endloop.

  endmethod.

  method get_problem_from_cache.

    rs_problem = mo_slpm_cache_controller->get_record( ip_guid ).

  endmethod.


  method get_problem_through_cache.

    es_result = me->get_problem_from_cache( ip_guid ).

    if es_result is initial.

      es_result = me->yif_slpm_data_manager~get_problem(
                 exporting
                   ip_guid = ip_guid
                   ).

      add_problem_to_cache( es_result ).

    endif.


  endmethod.


  method add_problem_to_cache.


    mo_slpm_cache_controller->add_record( is_problem ).

  endmethod.

  method set_slpm_cache_controller.

    if  mo_slpm_cache_controller is not bound.

      mo_slpm_cache_controller = new ycl_slpm_problem_snlru_cache( mo_active_configuration ).

    endif.


  endmethod.

  method yif_slpm_data_manager~fill_cached_prb_calc_flds.

    data: ls_sla_status            type ais_sla_status,
          lv_current_timestamp     type timestamp,
          lv_system_timezone       type timezone,
          lv_seconds_total_in_proc type integer,
          lv_created_at_user_tzone type comt_created_at_usr.

    " ===========  Total processing time  ===========

    if ( cs_problem-closed eq abap_false ).

      " For statuses except Withdrawn (E0010) and Confirmed (E0008)  it is a difference between now and creation timestamp

      lv_system_timezone =  ycl_assistant_utilities=>get_system_timezone(  ).

      convert date sy-datum time sy-uzeit into time stamp lv_current_timestamp time zone 'UTC'.

      " Converting CREATED_AT date to a user timezone according to CRM logic

      lv_created_at_user_tzone = ycl_assistant_utilities=>convert_timestamp_to_timezone(
        ip_timestamp = cs_problem-created_at
        ip_timezone = sy-zonlo ).

      lv_seconds_total_in_proc = ycl_assistant_utilities=>calc_duration_btw_timestamps(
        exporting
            ip_timestamp_1 = lv_created_at_user_tzone
            ip_timestamp_2 = lv_current_timestamp ).

    endif.

    cs_problem-totalproctimeminutes = lv_seconds_total_in_proc div 60.

    " ===========  SLA related fields  ===========

    if ( mo_active_configuration->get_parameter_value( 'USE_NON_STANDARD_SLA_CALC_IN_SNLRU_CACHE' ) eq 'X').

      me->yif_slpm_data_manager~calc_non_stand_sla_status(
         exporting
             ip_seconds_in_processing = lv_seconds_total_in_proc
             ip_created_at_user_tzone = lv_created_at_user_tzone
         changing
             cs_problem = cs_problem ).

    else.

      ls_sla_status = mo_slpm_problem_api->yif_custom_crm_order_read~get_sla_status_by_guid( ip_guid ).

      move-corresponding ls_sla_status to cs_problem.

    endif.

  endmethod.

  method yif_slpm_data_manager~calc_non_stand_sla_status.

    data:
      lv_seconds_for_irt type integer,
      lv_seconds_for_mpt type integer.

    " ==================== IRT block ======================

    " Live recalculation of cached IRT SLA is not required,
    " when first reaction has been provided and IRT SLAs are
    " stored already, and when irt sla is on hold - then cached data
    " will be relevant until a next update

    if ( cs_problem-firstreactiondate is initial ) and ( cs_problem-irtslaonhold is initial ).

      " Calculating seconds for IRT

      lv_seconds_for_irt = ycl_assistant_utilities=>calc_duration_btw_timestamps(
          exporting
              ip_timestamp_1 = ip_created_at_user_tzone
              ip_timestamp_2 = cs_problem-irt_timestamp ).


      " Calculating percentage for IRT

      cs_problem-irt_perc = ( ip_seconds_in_processing * 100 ) div lv_seconds_for_irt.

      " Calculating status for IRT

      cs_problem-irt_icon_bsp = cond #(
          when cs_problem-irt_perc > 100 then 'OVERDUE'
              else 'NOTDUE'
        ).

      " Additional 999 percentage according to standard

      if cs_problem-irt_perc > 500.

        cs_problem-irt_perc = 999.

      endif.

    endif.

    " ==================== MPT block ======================

    " Live recalculation of cached MPT SLA is not required,
    " when problem has beenclosed, and when mpt sla is on hold - then cached data
    " will be relevant until a next update

    if ( ( cs_problem-status ne 'E0008' ) and ( cs_problem-status ne 'E0010' ) ) and
    ( cs_problem-mptslaonhold is initial ) .

      " Calculating seconds for MPT

      lv_seconds_for_mpt = ycl_assistant_utilities=>calc_duration_btw_timestamps(
          exporting
              ip_timestamp_1 = ip_created_at_user_tzone
              ip_timestamp_2 = cs_problem-mpt_timestamp ).

      " Calculating percentage for MPT

      cs_problem-mpt_perc = ( ip_seconds_in_processing * 100 ) div lv_seconds_for_mpt.

      " Calculating status for MPT

      cs_problem-mpt_icon_bsp = cond #(
          when cs_problem-mpt_perc > 100 then 'OVERDUE'
              else 'NOTDUE'
        ).

      " Additional 999 percentage according to standard

      if cs_problem-mpt_perc > 500.

        cs_problem-mpt_perc = 999.

      endif.

    endif.

  endmethod.

  method get_prob_guids_through_cache.

    if mo_slpm_cache_controller is bound.

      rt_problems_guids = mo_slpm_cache_controller->get_all_problems_guids(  ).

      if rt_problems_guids is initial.

        rt_problems_guids = mo_slpm_problem_api->yif_custom_crm_order_read~get_guids_list(  ).

        mo_slpm_cache_controller->set_all_problems_guids( rt_problems_guids ).

      endif.

    endif.

  endmethod.

  method filter_out_attachments.

    data: lv_visibility       type char1,
          lo_slpm_user        type ref to yif_slpm_user,
          wa_attachments_list like line of et_attachments_list.

    lo_slpm_user = new ycl_slpm_user( sy-uname ).

    loop at it_attachments_list assigning field-symbol(<ls_attachment>).

      move-corresponding <ls_attachment> to wa_attachments_list.

      lv_visibility = me->get_attachment_extra_vsbl(
          exporting
              ip_guid = ip_guid
              ip_loio = <ls_attachment>-loio_id
              ip_phio = <ls_attachment>-phio_id ).

      if ( lv_visibility eq 'I' ).

        if ( lo_slpm_user->is_auth_for_internal_att(  ) eq abap_true ).

          " For internal visibility we will use field EnabledEdit, as we did not use it anyhow so far

          wa_attachments_list-enable_edit = abap_true.

          append wa_attachments_list to et_attachments_list.

        endif.

      else.

        append wa_attachments_list to et_attachments_list.

      endif.

    endloop.

  endmethod.

  method record_attachment_visibility.

    data wa_yslpm_att_vsbl type yslpm_att_vsbl.

    wa_yslpm_att_vsbl-guid = ip_guid.
    wa_yslpm_att_vsbl-loio_id = ip_loio.
    wa_yslpm_att_vsbl-phio_id = ip_phio.
    wa_yslpm_att_vsbl-visibility = ip_visibility.

    insert yslpm_att_vsbl from wa_yslpm_att_vsbl.

  endmethod.

  method get_attachment_extra_vsbl.

    select single visibility into rp_visibility
      from yslpm_att_vsbl
          where guid = ip_guid and
          loio_id = ip_loio and
          phio_id = ip_phio.

  endmethod.


  method yif_slpm_data_manager~get_list_of_support_teams.

    data:
      lt_support_team_org_units    type table of pd_objid_r,
      wa_support_team              type yslpm_ts_support_team,
      lo_organizational_model      type ref to yif_organizational_model,
      lv_org_unit_short_name       type short_d,
      lv_org_unit_text             type stext,
      lv_sup_team_level_in_org_str type int1,
      lt_proc_pool_sub_units       type crmt_orgman_swhactor_tab,
      lt_proc_pool_sub_units_struc type yif_organizational_model=>ty_sub_units_struc.


*    select processingorgunit into table lt_support_team_org_units
*        from yslpm_prod_attr.
*
*    sort lt_support_team_org_units.
*
*    delete adjacent duplicates from lt_support_team_org_units.
*
*    lo_organizational_model = new ycl_organizational_model( ).
*
*    loop at lt_support_team_org_units assigning field-symbol(<ls_support_team_org_unit>).
*
*      wa_support_team-orgunitnumber = <ls_support_team_org_unit>.
*
*      wa_support_team-businesspartner = lo_organizational_model->get_bp_of_org_unit( <ls_support_team_org_unit> ).
*
*      lo_organizational_model->get_org_unit_code_and_text(
*             exporting
*                 ip_org_unit = <ls_support_team_org_unit>
*                 ip_otype = 'O'
*             importing
*                 ep_org_unit_code = lv_org_unit_short_name
*                 ep_org_unit_text = lv_org_unit_text ).
*
*      wa_support_team-name = lv_org_unit_text.
*
*      append wa_support_team to rt_support_teams.
*
*    endloop.

    " Getting a level of support team from a configuration (default level is 2)

    lv_sup_team_level_in_org_str = mo_active_configuration->get_parameter_value( 'SUPPORT_TEAM_LEVEL_IN_ORG_STRUCTURE' ).

    if ( lv_sup_team_level_in_org_str is initial ) or ( lv_sup_team_level_in_org_str eq  0 ).

      lv_sup_team_level_in_org_str = 2.

    endif.

    lo_organizational_model = new ycl_organizational_model(  me->get_processors_pool_org_unit( ) ).

    lo_organizational_model->get_subunits_of_root_org_unit(
        importing
            et_sub_units = lt_proc_pool_sub_units
            et_sub_units_struc = lt_proc_pool_sub_units_struc ).

    loop at lt_proc_pool_sub_units_struc assigning field-symbol(<ls_proc_pool_sub_units_struc>)
        where level eq lv_sup_team_level_in_org_str.

      append <ls_proc_pool_sub_units_struc>-objid to lt_support_team_org_units.

    endloop.

    loop at lt_support_team_org_units assigning field-symbol(<ls_support_team_org_unit>).

      wa_support_team-orgunitnumber = <ls_support_team_org_unit>.

      wa_support_team-businesspartner = lo_organizational_model->get_bp_of_org_unit( <ls_support_team_org_unit> ).

      lo_organizational_model->get_org_unit_code_and_text(
             exporting
                 ip_org_unit = <ls_support_team_org_unit>
                 ip_otype = 'O'
             importing
                 ep_org_unit_code = lv_org_unit_short_name
                 ep_org_unit_text = lv_org_unit_text ).

      wa_support_team-name = lv_org_unit_text.

      append wa_support_team to rt_support_teams.

    endloop.

  endmethod.

  method yif_slpm_data_manager~get_frontend_constants.

    rt_constants = value #(

        ( class = 'statusNames' parameter = 'new' value = 'E0001')
        ( class = 'statusNames' parameter = 'approved' value = 'E0015')
        ( class = 'statusNames' parameter = 'inProcess' value = 'E0002')
        ( class = 'statusNames' parameter = 'customerAction' value = 'E0003')
        ( class = 'statusNames' parameter = 'solutionProvided' value = 'E0005')
        ( class = 'statusNames' parameter = 'confirmed' value = 'E0008')
        ( class = 'statusNames' parameter = 'withdrawn' value = 'E0010')
        ( class = 'statusNames' parameter = 'onApproval' value = 'E0016')
        ( class = 'statusNames' parameter = 'informationRequested' value = 'E0017')
        ( class = 'textTypes' parameter = 'reply' value = 'SU01')
        ( class = 'textTypes' parameter = 'description' value = 'SU99')
        ( class = 'textTypes' parameter = 'reproductionSteps' value = 'SURS')
        ( class = 'textTypes' parameter = 'internalNote' value = 'SU04')
        ( class = 'textTypes' parameter = 'solution' value = 'SUSO')
        ( class = 'textTypes' parameter = 'businessConsequences' value = 'SUBI')
        ( class = 'textTypes' parameter = 'additionalInformation' value = 'SU30')
        ( class = 'textTypesForStatuses' parameter = 'approved' value = 'SU01')
        ( class = 'textTypesForStatuses' parameter = 'customerAction' value = 'SU01')
        ( class = 'textTypesForStatuses' parameter = 'solutionProvided' value = 'SUSO')
        ( class = 'textTypesForStatuses' parameter = 'informationRequested' value = 'SU01')
        ( class = 'textTypesOfProblemCreation' parameter = 'textTypes' value = 'description')
        ( class = 'textTypesOfProblemCreation' parameter = 'textTypes' value = 'reproductionSteps')
        ( class = 'textTypesOfProblemCreation' parameter = 'textTypes' value = 'businessConsequences')
        ( class = 'statusesWithMandatoryTextComments' parameter = 'statuses' value = 'customerAction')
        ( class = 'statusesWithMandatoryTextComments' parameter = 'statuses' value = 'solutionProvided')
        ( class = 'statusesWithMandatoryTextComments' parameter = 'statuses' value = 'informationRequested')
        ( class = 'statusesWithMandatoryTextComments' parameter = 'statuses' value = 'withdrawn')
        ( class = 'statusesWithPossibleProccessorChange' parameter = 'statuses' value = 'new')
        ( class = 'statusesWithPossibleProccessorChange' parameter = 'statuses' value = 'approved')
        ( class = 'statusesWithPossibleProccessorChange' parameter = 'statuses' value = 'inProcess')
        ( class = 'statusesWithPossibleProccessorChange' parameter = 'statuses' value = 'onApproval')
        ( class = 'mandatoryInputFields' parameter = 'mandatoryInputFields' value = 'tableGeneralDataItemInputName')
        ( class = 'mandatoryInputFields' parameter = 'mandatoryInputFields' value = 'tableGeneralDataItemInputDescription')
        ( class = 'mandatoryInputFields' parameter = 'mandatoryInputFields' value = 'tableGeneralDataItemSelectSystem')
        ( class = 'inputFieldsWithMinCharsValidation' parameter = 'inputFieldsWithMinCharsValidation' value = 'tableGeneralDataItemInputName')
        ( class = 'inputFieldsWithMinCharsValidation' parameter = 'inputFieldsWithMinCharsValidation' value = 'tableGeneralDataItemInputDescription')
        ( class = 'emailAddressInputFields' parameter = 'emailAddressInputFields' value = 'tableGeneralDataItemInputContactPersonEmail')

    ).

  endmethod.

  method set_cust_action_status_codes.

    mt_cust_action_status_codes = value #(
        ( 'E0017' )
        ( 'E0003' )
        ( 'E0005' )
    ).

  endmethod.

  method yif_slpm_data_manager~is_status_a_customer_action.


    if line_exists( mt_cust_action_status_codes[ table_line = ip_status ] ).

      rp_customer_action = abap_true.

    endif.


  endmethod.

  method yif_slpm_data_manager~is_status_a_final_status.

    if line_exists( mt_final_status_codes[ table_line = ip_status ] ).

      rp_final_status = abap_true.

    endif.

  endmethod.

  method set_final_status_codes.

    mt_final_status_codes = value #(

    ( 'E0010' )
    ( 'E0008' )

    ).

  endmethod.

  method yif_slpm_data_manager~get_final_status_codes.

    loop at mt_final_status_codes assigning field-symbol(<fs_final_status>).

      append <fs_final_status> to rt_final_status_codes.

    endloop.

  endmethod.

  method set_hold_irt_sla_status_codes.

    mt_hold_irt_sla_status_codes = value #(

      ( 'E0017' )

    ).

  endmethod.


  method set_hold_mpt_sla_status_codes.

    mt_hold_mpt_sla_status_codes = value #(

      ( 'E0017' )
      ( 'E0003' )
      ( 'E0005' )

    ).

  endmethod.

  method set_reqconfenab_status_code.

    mt_reqconfenab_status_codes = value #(

    ( 'E0005' )

    ).

  endmethod.

  method set_reqrepenab_status_codes.

    mt_reqrepenab_status_codes = value #(

     ( 'E0003' )
     ( 'E0005' )
     ( 'E0017' )

    ).

  endmethod.

  method set_requpdenab_status_codes.

    mt_requpdenab_status_codes = value #(

        ( 'E0001' )
        ( 'E0002' )
        ( 'E0016' )

    ).

  endmethod.

  method set_reqwitenab_status_codes.

    mt_reqwitenab_status_codes = value #(

        ( 'E0001' )
        ( 'E0002' )

    ).

  endmethod.

  method set_procedmodeena_status_codes.

    mt_procedmodeenab_status_codes = value #(

        ( 'E0001' )
        ( 'E0002' )
        ( 'E0003' )
        ( 'E0015' )
        ( 'E0016' )
        ( 'E0005' )

    ).

  endmethod.

  method set_procprichgena_status_codes.

    mt_procprichgenab_status_codes = value #(

        ( 'E0001' )
        ( 'E0016' )

     ).

  endmethod.

  method set_procretwitena_status_codes.

    mt_procretwitenab_status_codes = value #(

        ( 'E0010' )

    ).

  endmethod.

  method yif_slpm_data_manager~get_active_configuration.

    ro_active_configuration = mo_active_configuration.

  endmethod.

  method get_last_slpm_mpt_hist.

    select guid apptguid problemguid mpttimestamp mpttimezone mptperc update_timestamp update_timezone
      from yslpm_mpt_hist
      into corresponding fields of rs_slpm_mpt_hist
     up to 1 rows
       where problemguid = ip_guid order by update_timestamp descending.

    endselect.

  endmethod.

  method get_stored_mpt_perc.

    rp_stored_mpt_perc = me->get_last_slpm_mpt_hist( ip_guid )-mptperc.

  endmethod.

endclass.
