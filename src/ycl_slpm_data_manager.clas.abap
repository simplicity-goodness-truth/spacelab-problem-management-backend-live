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
      mo_slpm_problem_api       type ref to ycl_slpm_problem_api,
      mo_active_configuration   type ref to yif_slpm_configuration,
      mo_system_user            type ref to yif_system_user,
      mo_log                    type ref to ycl_logger_to_app_log,
      mv_app_log_object         type balobj_d,
      mv_app_log_subobject      type balsubobj,
      mt_internal_pool          type yslpm_tt_users,
      mt_proc_pool_assigned_pos type yorg_model_tt_positions.

    methods:

      add_problem_non_db_fields
        changing cs_problem type ycrm_order_ts_sl_problem
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
          value(rp_available) type bool.


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

    mo_slpm_problem_api->yif_custom_crm_order_create~create_attachment(
      exporting
          ip_content = ip_content
          ip_file_name = ip_file_name
          ip_guid = ip_guid
          ip_mime_type = ip_mime_type ).

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

    mo_slpm_problem_api->yif_custom_crm_order_read~get_attachments_list_by_guid(
    exporting
     ip_guid = ip_guid
    importing
     et_attachments_list = et_attachments_list
     et_attachments_list_short = et_attachments_list_short ).

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
*
*    lo_slpm_problem_api = new ycl_slpm_problem_api(  ).

    lt_crm_guids = mo_slpm_problem_api->yif_custom_crm_order_read~get_guids_list(  ).

    if lt_crm_guids is not initial.

      lo_slpm_user = new ycl_slpm_user( sy-uname ).

      loop at lt_crm_guids assigning field-symbol(<ls_crm_guid>).

        ls_result = me->yif_slpm_data_manager~get_problem(
                 exporting
                   ip_guid = <ls_crm_guid>-guid
                   ).

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

  endmethod.


  method yif_slpm_data_manager~update_problem.

    data:
      lr_problem          type ref to data.

    get reference of is_problem into lr_problem.

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

    data: lo_slpm_customer type ref to yif_slpm_customer,
          lo_company       type ref to yif_company.

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

        data lt_customer_systems type yslpm_tt_systems.

        try.

            lt_customer_systems = lo_slpm_customer->get_slpm_systems_of_customer(  ).

            cs_problem-sapsystemdescription = lt_customer_systems[ sapsystemname = cs_problem-sapsystemname ]-description.
            cs_problem-sapsystemrole = lt_customer_systems[ sapsystemname = cs_problem-sapsystemname ]-role.

          catch cx_sy_itab_line_not_found.

        endtry.

      endif.

      " IRT history available

      cs_problem-irthistoryavailable = me->is_irt_history_available( cs_problem-guid  ).

      " MPT history available

      cs_problem-mpthistoryavailable = me->is_mpt_history_available( cs_problem-guid  ).

      " Stored IRT percent

      cs_problem-storedirtperc = me->get_stored_irt_perc( cs_problem-guid  ).

    endif.

    " Created internally flag

    cs_problem-createdinternally = me->is_requester_bp_in_intern_pool( cs_problem-requestorbusinesspartner ).

    " SLAs on Hold flag

    "if ( cs_problem-irt_icon_bsp ne 'OVERDUE' ) and  ( cs_problem-mpt_icon_bsp ne 'OVERDUE' ).

    cs_problem-irtslaonhold = switch char4(  cs_problem-status
                  when 'E0017'    then abap_true
                      else abap_false ).

    cs_problem-mptslaonhold = switch char4(  cs_problem-status
                    when 'E0017'    then abap_true
                    when 'E0003'    then abap_true
                    when 'E0005'    then abap_true
                    else abap_false ).


    "endif.

  endmethod.

  method fill_possible_problem_actions.

    " Requester confirmation: when is enabled

    cs_problem-requesterconfirmenabled = switch #( cs_problem-status
                  when 'E0005'    then abap_true
                      else abap_false ).

    " Requester reply: when is enabled
    cs_problem-requesterreplyenabled = switch #( cs_problem-status
              when 'E0003'    then abap_true
              when 'E0005'    then abap_true
              when 'E0017'    then abap_true
                  else abap_false ).

    " Requester update(extra data provision): when is enabled
    cs_problem-requesterupdateenabled = switch #( cs_problem-status
              when 'E0001'    then abap_true
              when 'E0002'    then abap_true
                  else abap_false ).

    " Requester withdrawal: when is enabled
    cs_problem-requesterwithdrawenabled = switch #( cs_problem-status
              when 'E0001'    then abap_true
              when 'E0002'    then abap_true
                  else abap_false ).

    " Processor editing: when is enabled
    cs_problem-processoreditmodeenabled = cond bu_partner(
        when cs_problem-processorbusinesspartner = mo_system_user->get_businesspartner( ) then
            switch char5( cs_problem-status
              when 'E0001'    then abap_true
              when 'E0002'    then abap_true
              when 'E0015'    then abap_true
              when 'E0016'    then abap_true
                  else abap_false )
        else abap_false ) .

    " Processor take over: when is enabled

    cs_problem-processortakeoverenabled = cond bu_partner( when ( cs_problem-processorbusinesspartner  is initial )
        or ( cs_problem-processorbusinesspartner eq '0' )
        then switch char5( cs_problem-status

              when 'E0001'    then abap_true
              when 'E0002'    then abap_true
              when 'E0015'    then abap_true
                  else abap_false )

        else abap_false ).

    " Processor priority change: when is enabled

    cs_problem-processorprioritychangeenabled = cond bu_partner(
            when cs_problem-processorbusinesspartner = mo_system_user->get_businesspartner( ) then
                switch char5( cs_problem-status
                  when 'E0001'    then abap_true
                  when 'E0016'    then abap_true
                      else abap_false )
            else abap_false ) .

    " Processor return from withdrawal: when is enabled

    cs_problem-processorreturnfromwithdrawal = cond bu_partner(
           when cs_problem-processorbusinesspartner = mo_system_user->get_businesspartner( ) then
                switch char5( cs_problem-status
                    when 'E0010'    then abap_true
                    else abap_false )
            else abap_false ) .

  endmethod.

  method yif_slpm_data_manager~get_list_of_processors.

    data:
      lv_proc_pool_org_unit   type pd_objid_r,
      lo_organizational_model type ref to yif_organizational_model,
      ls_processor            type yslpm_ts_user,
      lv_bp_in_processing     type bu_partner.



    if mt_proc_pool_assigned_pos is initial.

      lv_proc_pool_org_unit = me->get_processors_pool_org_unit(  ).

      if lv_proc_pool_org_unit is not initial.

        lo_organizational_model = new ycl_organizational_model( lv_proc_pool_org_unit ).

        mt_proc_pool_assigned_pos = lo_organizational_model->get_assigned_pos_of_org_unit(  ).

        sort mt_proc_pool_assigned_pos by businesspartner.

      endif.

    endif.


    if mt_proc_pool_assigned_pos is not initial.

      loop at mt_proc_pool_assigned_pos assigning field-symbol(<ls_proc_pool_assigned_pos>).

        if ( lv_bp_in_processing is initial ).

          ls_processor-businesspartner = <ls_proc_pool_assigned_pos>-businesspartner.
          ls_processor-fullname = <ls_proc_pool_assigned_pos>-fullname.
          ls_processor-username = <ls_proc_pool_assigned_pos>-businesspartner.
          ls_processor-searchtag1 = <ls_proc_pool_assigned_pos>-stext.

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
        statusout priorityin priorityout from yslpm_irt_hist into
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
        statusout priorityin priorityout from yslpm_mpt_hist into
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

endclass.
