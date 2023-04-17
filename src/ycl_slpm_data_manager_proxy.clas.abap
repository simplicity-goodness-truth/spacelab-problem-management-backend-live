class YCL_SLPM_DATA_MANAGER_PROXY definition
  public
  final
  create public .
  public section.

    interfaces yif_slpm_data_manager.

    methods constructor
      raising ycx_slpm_data_manager_exc
              ycx_slpm_configuration_exc
              ycx_system_user_exc
              ycx_crm_order_api_exc.

  protected section.
  private section.
    data:
      mo_slpm_data_provider   type ref to yif_slpm_data_manager,
      mo_active_configuration type ref to yif_slpm_configuration,
      mo_system_user          type ref to yif_system_user,
      mo_slpm_user            type ref to yif_slpm_user,
      mo_log                  type ref to ycl_logger_to_app_log,
      mv_app_log_object       type balobj_d,
      mv_app_log_subobject    type balsubobj.

    methods:


      notify_on_problem_change
        importing
          is_problem_new_state type ycrm_order_ts_sl_problem
          is_problem_old_state type ycrm_order_ts_sl_problem
        raising
          ycx_assistant_utilities_exc
          ycx_slpm_configuration_exc,

      set_app_logger
        raising
          ycx_slpm_configuration_exc,

      post_update_external_actions
        importing
          is_problem_old_state type ycrm_order_ts_sl_problem
          is_problem_new_state type ycrm_order_ts_sl_problem
        raising
          ycx_slpm_configuration_exc,

      store_irt_sla
        importing
          ip_guid        type crmt_object_guid
          ip_irt_perc    type int4
          ip_statusin    type char5
          ip_statusout   type char5
          ip_priorityin  type crmt_priority
          ip_priorityout type crmt_priority
        raising
          ycx_slpm_configuration_exc
          ycx_crm_order_api_exc
          ycx_system_user_exc,

      store_mpt_sla
        importing
          ip_guid        type crmt_object_guid
          ip_mpt_perc    type int4
          ip_statusin    type char5
          ip_statusout   type char5
          ip_priorityin  type crmt_priority
          ip_priorityout type crmt_priority
        raising
          ycx_slpm_configuration_exc
          ycx_crm_order_api_exc
          ycx_system_user_exc,

      recalc_irt_sla
        importing
          ip_guid               type crmt_object_guid
          ip_avail_profile_name type srv_serwi
          ip_irt_perc           type int4 optional
          ip_statusin           type char5
          ip_statusout          type char5
          ip_priorityin         type crmt_priority
          ip_priorityout        type crmt_priority
        raising
          ycx_slpm_configuration_exc
          ycx_crm_order_api_exc,

      adjust_scapptseg_irt
        importing
          ip_guid type crmt_object_guid.



endclass.

class YCL_SLPM_DATA_MANAGER_PROXY implementation.

  method set_app_logger.

    mv_app_log_object = mo_active_configuration->get_parameter_value( 'APP_LOG_OBJECT' ).
    mv_app_log_subobject = 'YDATAMANAGER'.

    mo_log = ycl_logger_to_app_log=>get_instance( ).
    mo_log->set_object_and_subobject(
          exporting
            ip_object    =   mv_app_log_object
            ip_subobject =   mv_app_log_subobject ).

  endmethod.

  method constructor.

    mo_slpm_user = new ycl_slpm_user( sy-uname ).

    mo_system_user ?= mo_slpm_user.

    if mo_slpm_user->is_auth_to_read_problems(  ) eq abap_true.

      mo_active_configuration = new ycl_slpm_configuration(  ).

      mo_slpm_data_provider = new ycl_slpm_data_manager(
        io_active_configuration = mo_active_configuration
        io_system_user = me->mo_system_user ).

    else.

      " User has no authorizations to read problems

      raise exception type ycx_slpm_data_manager_exc
        exporting
          textid         = ycx_slpm_data_manager_exc=>not_authorized_for_read
          ip_system_user = sy-uname.

    endif.

    me->set_app_logger(  ).

  endmethod.


  method yif_slpm_data_manager~create_attachment.

    if mo_slpm_data_provider is bound.

      mo_slpm_data_provider->create_attachment(
      exporting
          ip_content = ip_content
          ip_file_name = ip_file_name
          ip_guid = ip_guid
          ip_mime_type = ip_mime_type ).

    endif.

  endmethod.


  method notify_on_problem_change.

    data lo_slpm_prob_change_notifier type ref to yif_crm_order_change_notifier.

    lo_slpm_prob_change_notifier = new ycl_slpm_prob_change_notifier(
            io_active_configuration = mo_active_configuration
            is_problem_new_state = is_problem_new_state
            is_problem_old_state = is_problem_old_state ).

    lo_slpm_prob_change_notifier->notify(  ).

  endmethod.


  method yif_slpm_data_manager~create_problem.


    data: ls_problem_newstate          type ycrm_order_ts_sl_problem,
          lo_slpm_prob_change_notifier type ref to yif_crm_order_change_notifier,
          lo_slpm_user                 type ref to yif_slpm_user,
          lv_log_record_text           type string,
          lv_product_id                type comt_product_id.


    " User has no authorizations to create problems

    if mo_slpm_user->is_auth_to_create_problems(  ) eq abap_false.

      raise exception type ycx_slpm_data_manager_exc
        exporting
          textid         = ycx_slpm_data_manager_exc=>not_authorized_for_create
          ip_system_user = sy-uname.

    endif.


    if mo_slpm_data_provider is bound.

      " Check authorizations of a user to create a problem against a company

      lo_slpm_user = new ycl_slpm_user( sy-uname ).

      if ( lo_slpm_user->is_auth_to_crea_company( is_problem-companybusinesspartner ) eq abap_false ).

        message e004(yslpm_data_manager) with sy-uname is_problem-companybusinesspartner into lv_log_record_text.

        mo_log->yif_logger~err( lv_log_record_text ).

        raise exception type ycx_slpm_data_manager_exc
          exporting
            textid         = ycx_slpm_data_manager_exc=>no_auth_for_creat_for_company
            ip_system_user = sy-uname
            ip_company_bp  = is_problem-companybusinesspartner.

      endif.

      " Check authorizations of a user to create a problem against a product

      lv_product_id = is_problem-productname.

      if ( lo_slpm_user->is_auth_to_crea_product( lv_product_id ) eq abap_false ).

        message e006(yslpm_data_manager) with sy-uname lv_product_id into lv_log_record_text.

        mo_log->yif_logger~err( lv_log_record_text ).

        raise exception type ycx_slpm_data_manager_exc
          exporting
            textid         = ycx_slpm_data_manager_exc=>no_auth_for_creat_for_prod
            ip_system_user = sy-uname
            ip_product_id  = lv_product_id.

      endif.

      " Notification on a problem change

      try.

          rs_result = mo_slpm_data_provider->create_problem( exporting is_problem = is_problem ).

          me->notify_on_problem_change(
              exporting
              is_problem_new_state = rs_result
              is_problem_old_state = is_problem ).

        catch  ycx_crm_order_api_exc ycx_assistant_utilities_exc into data(lcx_process_exception).

          raise exception type ycx_slpm_data_manager_exc
            exporting
              textid = ycx_slpm_data_manager_exc=>internal_error
              ip_error_message = lcx_process_exception->get_text( ).

      endtry.

    endif.

  endmethod.




  method yif_slpm_data_manager~update_problem.

    data: ls_problem_old_state type ycrm_order_ts_sl_problem.

    " User has no authorizations to update problems

    if mo_slpm_user->is_auth_to_update_problems(  ) eq abap_false.

      raise exception type ycx_slpm_data_manager_exc
        exporting
          textid         = ycx_slpm_data_manager_exc=>not_authorized_for_update
          ip_system_user = sy-uname.

    endif.


    if mo_slpm_data_provider is bound.

      try.

          ls_problem_old_state = mo_slpm_data_provider->get_problem(
              exporting
                ip_guid = ip_guid ).

          rs_result = mo_slpm_data_provider->update_problem(
            exporting
                ip_guid = ip_guid
                is_problem = is_problem ).

          me->post_update_external_actions(
               exporting
               is_problem_new_state = rs_result
               is_problem_old_state = ls_problem_old_state ).


          me->notify_on_problem_change(
                     exporting
                     is_problem_new_state = rs_result
                     is_problem_old_state = ls_problem_old_state ).


        catch ycx_crm_order_api_exc into data(lcx_process_exception).

          raise exception type ycx_slpm_data_manager_exc
            exporting
              textid           = ycx_slpm_data_manager_exc=>internal_error
              ip_error_message = lcx_process_exception->get_text( ).
      endtry.

    endif.

  endmethod.

  method yif_slpm_data_manager~create_text.

    if mo_slpm_data_provider is bound.

      mo_slpm_data_provider->create_text(
             exporting
                 ip_guid = ip_guid
                 ip_tdid = ip_tdid
                 ip_text = ip_text ).


    endif.

  endmethod.


  method yif_slpm_data_manager~delete_attachment.

    mo_slpm_data_provider->delete_attachment(
         exporting
             ip_guid = ip_guid
             ip_loio = ip_loio
             ip_phio = ip_phio ).

  endmethod.


  method yif_slpm_data_manager~get_all_priorities.

    if mo_slpm_data_provider is bound.
      rt_priorities = mo_slpm_data_provider->get_all_priorities(  ).
    endif.

  endmethod.


  method yif_slpm_data_manager~get_attachment.

    if mo_slpm_data_provider is bound.

      er_attachment = mo_slpm_data_provider->get_attachment(
      exporting
      ip_guid = ip_guid
      ip_loio = ip_loio ip_phio = ip_phio ).

    endif.

  endmethod.


  method yif_slpm_data_manager~get_attachments_list.

    mo_slpm_data_provider->get_attachments_list(
      exporting
       ip_guid = ip_guid
      importing
       et_attachments_list = et_attachments_list
       et_attachments_list_short = et_attachments_list_short ).

  endmethod.

  method yif_slpm_data_manager~get_attachment_content.

    if mo_slpm_data_provider is bound.

      mo_slpm_data_provider->get_attachment_content(
       exporting
           ip_guid = ip_guid
           ip_loio = ip_loio
           ip_phio = ip_phio
         importing
         er_attachment = er_attachment
         er_stream = er_stream ).

    endif.

  endmethod.


  method yif_slpm_data_manager~get_last_text.

    if mo_slpm_data_provider is bound.

      mo_slpm_data_provider->get_last_text( exporting ip_guid = ip_guid ).

    endif.
  endmethod.


  method yif_slpm_data_manager~get_priorities_of_product.

    if mo_slpm_data_provider is bound.
      rt_priorities = mo_slpm_data_provider->get_priorities_of_product(
ip_guid ).
    endif.

  endmethod.


  method yif_slpm_data_manager~get_problem.

    if mo_slpm_data_provider is bound.

      es_result = mo_slpm_data_provider->get_problem( ip_guid ).

    endif.

  endmethod.

  method yif_slpm_data_manager~get_problems_list.

    if mo_slpm_data_provider is bound.

      et_result = mo_slpm_data_provider->get_problems_list(
      exporting
        it_filters = it_filters
        it_order = it_order ).

    endif.

  endmethod.

  method yif_slpm_data_manager~get_texts.

    mo_slpm_data_provider->get_texts(
     exporting ip_guid = ip_guid
     importing et_texts = et_texts ).

  endmethod.

  method yif_slpm_data_manager~get_list_of_possible_statuses.

    if mo_slpm_data_provider is bound.

      rt_statuses = mo_slpm_data_provider->get_list_of_possible_statuses( ip_status ).

    endif.

  endmethod.

  method yif_slpm_data_manager~get_list_of_processors.

    if mo_slpm_data_provider is bound.

      rt_processors = mo_slpm_data_provider->get_list_of_processors(  ).

    endif.

  endmethod.

  method yif_slpm_data_manager~get_list_of_companies.

    if mo_slpm_data_provider is bound.

      rt_companies = mo_slpm_data_provider->get_list_of_companies(  ).

    endif.

  endmethod.

  method yif_slpm_data_manager~get_frontend_configuration.

    if mo_slpm_data_provider is bound.

      rt_frontend_configuration = mo_slpm_data_provider->get_frontend_configuration( ip_application ).

    endif.


  endmethod.

  method recalc_irt_sla.

    data:lv_difference_in_seconds      type integer,
         lv_timestamp_of_status_switch type timestamp,
         lv_irt_update_timestamp       type timestamp,
         lv_irt_update_timezone        type timezone,
         lv_old_irt_timestamp          type timestamp,
         lv_old_irt_timezone           type timezone,
         lv_new_irt_timestamp          type timestamp,
         lv_new_irt_timezone           type timezone,
         lv_appt_guid                  type sc_aptguid,
         lo_serv_profile_date_calc     type ref to
yif_serv_profile_date_calc,
         lv_avail_profile_name         type char258,
         lv_time                       type sy-uzeit,
         lv_date                       type sy-datum,
         lv_system_timezone            type timezone,
         ls_yslpm_irt_hist             type yslpm_irt_hist.

    " Taking a timestamp when we switched back from 'Information Requested

    get time stamp field lv_timestamp_of_status_switch.

    " Taking last stored IRT SLA

    select  update_timestamp update_timezone irttimestamp irttimezone apptguid
        from yslpm_irt_hist
        into (lv_irt_update_timestamp, lv_irt_update_timezone,
        lv_old_irt_timestamp, lv_old_irt_timezone, lv_appt_guid)
       up to 1 rows
         where problemguid = ip_guid order by update_timestamp descending.

    endselect.

    if sy-subrc eq 0.

      " Calculating difference between movement from 'On Approval' to 'Information Requested' and backwards

      lv_difference_in_seconds = ycl_assistant_utilities=>calc_duration_btw_timestamps(
       exporting
           ip_timestamp_1 = lv_irt_update_timestamp
           ip_timestamp_2 = lv_timestamp_of_status_switch ).

      " Calculating new value for IRT and storing it

      lv_avail_profile_name = ip_avail_profile_name.

      lo_serv_profile_date_calc = new ycl_serv_profile_date_calc( lv_avail_profile_name  ).

      ycl_assistant_utilities=>get_date_time_from_timestamp(
        exporting
            ip_timestamp = lv_old_irt_timestamp
            importing
            ep_date = lv_date
            ep_time = lv_time ).

      lo_serv_profile_date_calc->add_seconds_to_date(
        exporting
            ip_added_seconds_total = lv_difference_in_seconds
            ip_date_from = lv_date
            ip_time_from = lv_time
        importing
            ep_sla_date = lv_date
            ep_sla_time = lv_time ).

      lv_system_timezone =  ycl_assistant_utilities=>get_system_timezone(  ).

      convert date lv_date time lv_time into time stamp
lv_new_irt_timestamp time zone lv_system_timezone.

      update scapptseg set
          tst_from =  lv_new_irt_timestamp
          tst_to = lv_new_irt_timestamp
      where
          appt_guid = lv_appt_guid.

      " Storing for further internal usage

      ls_yslpm_irt_hist-irttimestamp = lv_new_irt_timestamp.
      ls_yslpm_irt_hist-irttimezone = lv_system_timezone.
      ls_yslpm_irt_hist-guid = ycl_assistant_utilities=>generate_x16_guid(  ).
      ls_yslpm_irt_hist-apptguid = lv_appt_guid.
      ls_yslpm_irt_hist-problemguid = ip_guid.
      get time stamp field ls_yslpm_irt_hist-update_timestamp.
      ls_yslpm_irt_hist-irtperc = ip_irt_perc.
      ls_yslpm_irt_hist-update_timezone = ycl_assistant_utilities=>get_system_timezone( ).
      ls_yslpm_irt_hist-statusin = ip_statusin.
      ls_yslpm_irt_hist-statusout = ip_statusout.
      ls_yslpm_irt_hist-priorityin = ip_priorityin.
      ls_yslpm_irt_hist-priorityout = ip_priorityout.

      insert yslpm_irt_hist from ls_yslpm_irt_hist.

    endif.

  endmethod.


  method store_irt_sla.


    data:
      lo_slmp_problem_api       type ref to ycl_slpm_problem_api,
      lt_appointments           type crmt_appointment_wrkt,
      ls_srv_rfirst_appointment type crmt_appointment_wrk,
      ls_yslpm_irt_hist         type yslpm_irt_hist.

    lo_slmp_problem_api       = new ycl_slpm_problem_api( mo_active_configuration ).

    lt_appointments = lo_slmp_problem_api->yif_custom_crm_order_read~get_all_appointments_by_guid( ip_guid ).

    try.

        ls_srv_rfirst_appointment = lt_appointments[ appt_type = 'SRV_RFIRST' ].

      catch cx_sy_itab_line_not_found.

    endtry.

    " Storing old IRT SLA

    select single tst_from zone_from into ( ls_yslpm_irt_hist-irttimestamp, ls_yslpm_irt_hist-irttimezone )
     from scapptseg
     where appt_guid = ls_srv_rfirst_appointment-appt_guid.

    if sy-subrc eq 0.

      ls_yslpm_irt_hist-guid = ycl_assistant_utilities=>generate_x16_guid(  ).
      ls_yslpm_irt_hist-apptguid = ls_srv_rfirst_appointment-appt_guid.
      ls_yslpm_irt_hist-problemguid = ip_guid.
      get time stamp field ls_yslpm_irt_hist-update_timestamp.
      ls_yslpm_irt_hist-irtperc = ip_irt_perc.
      ls_yslpm_irt_hist-update_timezone = ycl_assistant_utilities=>get_system_timezone( ).
      ls_yslpm_irt_hist-statusin = ip_statusin.
      ls_yslpm_irt_hist-statusout = ip_statusout.
      ls_yslpm_irt_hist-priorityin = ip_priorityin.
      ls_yslpm_irt_hist-priorityout = ip_priorityout.

      insert yslpm_irt_hist from ls_yslpm_irt_hist.

    endif.

  endmethod.


  method store_mpt_sla.


    data:
      lo_slmp_problem_api       type ref to ycl_slpm_problem_api,
      lt_appointments           type crmt_appointment_wrkt,
      ls_srv_rready_appointment type crmt_appointment_wrk,
      ls_yslpm_mpt_hist         type yslpm_mpt_hist.

    lo_slmp_problem_api       = new ycl_slpm_problem_api(
mo_active_configuration ).

    lt_appointments = lo_slmp_problem_api->yif_custom_crm_order_read~get_all_appointments_by_guid( ip_guid ).

    try.

        ls_srv_rready_appointment = lt_appointments[ appt_type = 'SRV_RREADY' ].

      catch cx_sy_itab_line_not_found.

    endtry.

    " Storing old IRT SLA

    select single tst_from zone_from into ( ls_yslpm_mpt_hist-mpttimestamp, ls_yslpm_mpt_hist-mpttimezone )
     from scapptseg
     where appt_guid = ls_srv_rready_appointment-appt_guid.

    if sy-subrc eq 0.

      ls_yslpm_mpt_hist-guid = ycl_assistant_utilities=>generate_x16_guid(  ).
      ls_yslpm_mpt_hist-apptguid = ls_srv_rready_appointment-appt_guid.
      ls_yslpm_mpt_hist-problemguid = ip_guid.
      get time stamp field ls_yslpm_mpt_hist-update_timestamp.
      ls_yslpm_mpt_hist-mptperc = ip_mpt_perc.
      ls_yslpm_mpt_hist-update_timezone = ycl_assistant_utilities=>get_system_timezone( ).
      ls_yslpm_mpt_hist-statusin = ip_statusin.
      ls_yslpm_mpt_hist-statusout = ip_statusout.
      ls_yslpm_mpt_hist-priorityin = ip_priorityin.
      ls_yslpm_mpt_hist-priorityout = ip_priorityout.

      insert yslpm_mpt_hist from ls_yslpm_mpt_hist.

    endif.

  endmethod.


  method adjust_scapptseg_irt.

    data:
      lv_irt_update_timestamp    type timestamp,
      lv_irt_update_timezone     type timezone,
      lv_stored_irt_timestamp    type timestamp,
      lv_stored_irt_timezone     type timezone,
      lv_appt_guid               type sc_aptguid,
      lv_scapptseg_irt_timestamp type timestamp,
      lv_scapptseg_irt_timezone  type timezone.

    " Taking last stored IRT SLA

    select update_timestamp update_timezone irttimestamp irttimezone apptguid
      from yslpm_irt_hist
      into (lv_irt_update_timestamp, lv_irt_update_timezone,
        lv_stored_irt_timestamp, lv_stored_irt_timezone, lv_appt_guid)
      up to 1 rows
      where problemguid = ip_guid order by update_timestamp descending.

      if sy-subrc eq 0.

        select single tst_from zone_from into ( lv_scapptseg_irt_timestamp, lv_scapptseg_irt_timezone )
            from scapptseg
            where appt_guid = lv_appt_guid.

        if lv_stored_irt_timestamp > lv_scapptseg_irt_timestamp.

          update scapptseg set
              tst_from =  lv_stored_irt_timestamp
              tst_to = lv_stored_irt_timestamp
          where
              appt_guid = lv_appt_guid.

        endif.

      endif.

    endselect.

  endmethod.

  method post_update_external_actions.

    types: begin of ty_methods_list,
             method_name type string,
             parameters  type abap_parmbind_tab,
           end of ty_methods_list.

    data: lv_method_name     type string,
          lv_log_record_text type string,
          lt_method_params   type abap_parmbind_tab,
          lt_common_params   type abap_parmbind_tab,
          lt_specific_params type abap_parmbind_tab,
          lo_slpm_product    type ref to yif_crm_service_product,
          lv_avail_profile   type srv_serwi,
          lt_methods_list    type table of  ty_methods_list,
          ls_method          type  ty_methods_list.


    lt_common_params = value #(
                 ( name = 'IP_GUID' value = ref #( is_problem_new_state-guid ) kind = cl_abap_objectdescr=>exporting )
                 ( name = 'IP_STATUSIN' value = ref #( is_problem_old_state-status ) kind = cl_abap_objectdescr=>exporting )
                 ( name = 'IP_STATUSOUT' value = ref #( is_problem_new_state-status ) kind = cl_abap_objectdescr=>exporting )
                 ( name = 'IP_PRIORITYIN' value = ref #( is_problem_old_state-priority ) kind = cl_abap_objectdescr=>exporting )
                 ( name = 'IP_PRIORITYOUT' value = ref #( is_problem_new_state-priority ) kind = cl_abap_objectdescr=>exporting )
             ).

    " Funny thing!!!
    " In our code we have to update scapptseg table to write shifted SLAs,
    " because we cannot set appointments through CRM order API (it just doesn't save it :-( )
    " However later somehow after each switch from 'In process' to 'Customer Action' OR from
    " 'On approval' to 'Information requested' all changed records in scapptseg table
    " ARE REVERTED BACK again to initial state!!! Don't know how and why it happens somewhere
    " deep in CRM ITSM...
    "
    " Finally after each save we have to compare recent scapptseg table SLA value and
    " those, which we stored in our custom tables. If scapptseg records were reverted,
    " then we have to re-write it once again....

    if  is_problem_old_state-status ne is_problem_new_state-status.
      me->adjust_scapptseg_irt( is_problem_new_state-guid ).
    endif.


    " Storing SLA if priority has been changed

    if ( is_problem_old_state-priority ne is_problem_new_state-priority ).

      lv_method_name = |STORE_IRT_SLA|.

      ls_method-method_name = lv_method_name.

      clear lt_method_params.
      lt_method_params = corresponding #( lt_common_params ).
      lt_specific_params = value #(

            ( name = 'IP_IRT_PERC' value = ref #( is_problem_new_state-irt_perc ) kind = cl_abap_objectdescr=>exporting ) ).

      insert lines of lt_specific_params into table lt_method_params.

      ls_method-parameters = lt_method_params.

      append ls_method to lt_methods_list.

    endif.

    " Storing MPT SLA when switch to 'Customer Action' and 'Solution Proposed' are done

    if ( is_problem_old_state-status = 'E0002' and is_problem_new_state-status = 'E0003' ) or
        ( is_problem_old_state-status = 'E0002' and is_problem_new_state-status = 'E0005' ).

      lv_method_name = |STORE_MPT_SLA|.

      ls_method-method_name = lv_method_name.

      clear lt_method_params.
      lt_method_params = corresponding #( lt_common_params ).

      lt_specific_params = value #(
         ( name = 'IP_MPT_PERC' value = ref #( is_problem_new_state-mpt_perc ) kind = cl_abap_objectdescr=>exporting )
        ).

      insert lines of lt_specific_params into table lt_method_params.

      ls_method-parameters = lt_method_params.

      append ls_method to lt_methods_list.

    endif.

    " When status is changed from 'Information Requested' to 'In approval'
    " we need to store IRT and MPT SLA timestamps

    if ( is_problem_old_state-status = 'E0016' and is_problem_new_state-status = 'E0017' ).

      if ( mo_active_configuration->get_parameter_value( 'SHIFT_IRT_ON_INFORMATION_REQUESTED_STAT' ) eq 'X').

        " Storing of IRT SLA happens only if IRT SLA is not overdue
        if ( is_problem_new_state-irt_icon_bsp eq 'NOTDUE').

          lv_method_name = |STORE_IRT_SLA|.
          ls_method-method_name = lv_method_name.

          clear lt_method_params.
          lt_method_params = corresponding #( lt_common_params ).

          lt_specific_params = value #(
                     ( name = 'IP_IRT_PERC' value = ref #( is_problem_new_state-irt_perc ) kind = cl_abap_objectdescr=>exporting )
                 ).

          insert lines of lt_specific_params into table lt_method_params.

          ls_method-parameters = lt_method_params.
          append ls_method to lt_methods_list.

        endif.

        lv_method_name = |STORE_MPT_SLA|.

        ls_method-method_name = lv_method_name.

        clear lt_method_params.
        lt_method_params = corresponding #( lt_common_params ).

        lt_specific_params = value #(
             ( name = 'IP_MPT_PERC' value = ref #( is_problem_new_state-mpt_perc ) kind = cl_abap_objectdescr=>exporting )
         ).

        insert lines of lt_specific_params into table lt_method_params.

        ls_method-parameters = lt_method_params.
        append ls_method to lt_methods_list.

      endif.

    endif.

    if ( is_problem_old_state-status = 'E0017' and is_problem_new_state-status = 'E0016' ).

      " Recalculation will be done only if IRT is not overdue

      if ( mo_active_configuration->get_parameter_value( 'SHIFT_IRT_ON_INFORMATION_REQUESTED_STAT' ) eq 'X') and
        ( is_problem_new_state-irt_icon_bsp eq 'NOTDUE').

        lv_method_name = |RECALC_IRT_SLA|.

        ls_method-method_name = lv_method_name.

        " Getting a name of an availability profile

        lo_slpm_product = new ycl_crm_service_product( is_problem_new_state-productguid ).

        lv_avail_profile = lo_slpm_product->get_availability_profile_name(  ).

        clear lt_method_params.
        lt_method_params = corresponding #( lt_common_params ).

        lt_specific_params = value #(
             ( name = 'IP_AVAIL_PROFILE_NAME' value = ref #( lv_avail_profile ) kind = cl_abap_objectdescr=>exporting )
             ( name = 'IP_IRT_PERC' value = ref #( is_problem_new_state-irt_perc ) kind = cl_abap_objectdescr=>exporting )
           ).

        insert lines of lt_specific_params into table lt_method_params.

        ls_method-parameters = lt_method_params.

        append ls_method to lt_methods_list.

      endif.

    endif.

    loop at lt_methods_list assigning field-symbol(<ls_method>).

      try.

          call method me->(<ls_method>-method_name)
            parameter-table
            <ls_method>-parameters.

        catch cx_sy_dyn_call_error into data(lcx_process_exception).

          lv_log_record_text = lcx_process_exception->get_text(  ) .
          mo_log->yif_logger~err( lv_log_record_text  ).

      endtry.

    endloop.

  endmethod.

  method yif_slpm_data_manager~get_all_statuses.

    if mo_slpm_data_provider is bound.

      rt_statuses = mo_slpm_data_provider->get_all_statuses(  ).

    endif.

  endmethod.



  method yif_slpm_data_manager~get_problem_sla_irt_history.

    if mo_slpm_data_provider is bound.

      rt_sla_irt_history = mo_slpm_data_provider->get_problem_sla_irt_history( ip_guid ).

    endif.

  endmethod.

  method yif_slpm_data_manager~get_problem_sla_mpt_history.

    if mo_slpm_data_provider is bound.

      rt_sla_mpt_history = mo_slpm_data_provider->get_problem_sla_mpt_history( ip_guid ).

    endif.

  endmethod.


endclass.