class ycl_slpm_prob_change_notifier definition
  public

  create public .

  public section.
    interfaces:

      yif_crm_order_change_notifier,

      yif_slpm_problem_observer .

    methods constructor
      importing
        io_active_configuration type ref to yif_slpm_configuration
        is_problem_old_state    type ycrm_order_ts_sl_problem
        is_problem_new_state    type ycrm_order_ts_sl_problem
      raising
        ycx_slpm_configuration_exc
        ycx_assistant_utilities_exc.

    methods: get_problem_new_state
      returning
        value(rs_problem_new_state) type ycrm_order_ts_sl_problem,
      get_problem_old_state
        returning
          value(rs_problem_old_state) type ycrm_order_ts_sl_problem.

  protected section.
  private section.
    data:
      ms_slpm_emails_for_statuses type yslpm_stat_email,
      ms_problem_old_state        type ycrm_order_ts_sl_problem,
      ms_problem_new_state        type ycrm_order_ts_sl_problem,
      mt_variables_values         type yst_text_tt_variables_values,
      mo_active_configuration     type ref to yif_slpm_configuration,
      mo_slpm_product             type ref to yif_slpm_product.

    class-data: mv_sender_email_address type ymessenger_address,
                mo_log                  type ref to ycl_logger_to_app_log,
                mv_app_log_object       type balobj_d,
                mv_app_log_subobject    type balsubobj.

    methods: notify_by_email,
      get_stat_dependant_email_rules,

      dispatch_emails,

      notify_by_process_role
        importing
          ip_email_rule             type char64
          ip_receiver_email_address type ymessenger_address
        raising
          ycx_assistant_utilities_exc,

      get_email_rule
        importing
          ip_email_rule          type char64
        exporting
          ep_email_internal_flag type char1
          ep_email_subj_text     type tdobname
          ep_email_body_text     type tdobname,

      get_compiled_text
        importing
          ip_use_tags             type abap_bool optional
          ip_text_name            type tdobname
        returning
          value(rp_compiled_text) type string,

      send_email
        importing
          ip_receiver_email_address type ymessenger_address
          ip_email_body             type string
          ip_email_subject          type so_obj_des,

      fill_variables_values
        importing
          ip_email_internal_flag type char1 optional
        raising
          ycx_assistant_utilities_exc,

      get_email_addresses
        importing
          ip_process_role           type char64
        returning
          value(rt_email_addresses) type ymessenger_tt_addresses,

      fill_problem_details_html
        importing
          ip_email_internal_flag    type char1 optional
        returning
          value(rp_problem_details) type string
        raising
          ycx_assistant_utilities_exc,

      get_config_parameter_value
        importing
          ip_param        type char50
        returning
          value(rp_value) type text200
        raising
          ycx_slpm_configuration_exc,

      set_app_logger
        raising
          ycx_slpm_configuration_exc,

      get_support_team_addresses
        returning value(rt_addresses) type ymessenger_tt_addresses,

      get_prod_org_unit_for_updates
        returning value(rp_org_unit) type pd_objid_r,

      get_signature_for_product
        returning
          value(rp_signature) type lxechar1024,

      set_slpm_product
        importing
          ip_product_guid type crmt_product_guid_db.

    class-methods set_sender_email_address
      importing
        ip_sender_email_address type ymessenger_address
      raising
        ycx_slpm_configuration_exc.

endclass.

class ycl_slpm_prob_change_notifier implementation.

  method get_problem_new_state.
    rs_problem_new_state = me->ms_problem_new_state.
  endmethod.

  method get_problem_old_state.
    rs_problem_old_state = me->ms_problem_old_state.
  endmethod.

  method constructor.

    ms_problem_new_state = is_problem_new_state.
    ms_problem_old_state = is_problem_old_state.

    mo_active_configuration = io_active_configuration.

    me->set_app_logger(  ).

    me->ms_problem_new_state = me->get_problem_new_state(  ).
    me->ms_problem_old_state = me->get_problem_old_state(  ).

    "  me->fill_variables_values( ).

    me->set_sender_email_address( get_config_parameter_value( 'email_sender_address' ) ).

    me->set_slpm_product( ms_problem_new_state-productguid ).

  endmethod.

  method set_app_logger.

    mv_app_log_object = mo_active_configuration->get_parameter_value( 'APP_LOG_OBJECT' ).
    mv_app_log_subobject = 'YNOTIFICATIONS'.

    mo_log = ycl_logger_to_app_log=>get_instance( ).
    mo_log->set_object_and_subobject(
          exporting
            ip_object    =   mv_app_log_object
            ip_subobject =   mv_app_log_subobject ).

  endmethod.

  method get_config_parameter_value.

    rp_value = mo_active_configuration->get_parameter_value( ip_param ).

  endmethod.

  method yif_crm_order_change_notifier~notify.

    me->notify_by_email(  ).

  endmethod.

  method get_stat_dependant_email_rules.

    " Get a email rule, corresponding to a current status change

    select single
        statusin statusout emailrulereq emailrulesup emailrulepro emailruleobs
            into corresponding fields of ms_slpm_emails_for_statuses
                 from yslpm_stat_email as a
                    where statusin = ms_problem_old_state-status and
                        statusout eq ms_problem_new_state-status.
  endmethod.

  method get_email_rule.

    select single internal sttextsubj sttextbody
        into ( ep_email_internal_flag, ep_email_subj_text, ep_email_body_text )
        from yslpm_email_rule
        where rulename = ip_email_rule.

  endmethod.

  method dispatch_emails.

    types: begin of ty_process_roles,
             acronym type char3,
             name    type char64,
           end of ty_process_roles.


    data: lo_structure_ref            type ref to data,
          lr_entity                   type ref to data,
          lt_process_roles            type table of ty_process_roles,
          lv_field_name               type string,
          lv_method_name              type string,
          lv_email_rule               type char64,
          ptab                        type abap_parmbind_tab,
          lt_receiver_email_addresses type ymessenger_tt_addresses,
          lv_log_record_text          type string.

    field-symbols: <fs_structure> type any,
                   <fs_value>     type any.


    " Fill possible roles acronyms table
    " REQ - requester
    " SUP - support team
    " PRO - processor
    " OBS - observer

    lt_process_roles = value #(
        ( acronym = 'REQ' name = 'REQUESTER' )
        ( acronym = 'SUP' name = 'SUPPORTTEAM' )
        ( acronym = 'PRO' name = 'PROCESSOR' )
        ( acronym = 'OBS' name = 'OBSERVER' ) ).

    get reference of ms_slpm_emails_for_statuses into lr_entity.

    if ( lr_entity is bound ).

      assign lr_entity->* to <fs_structure>.

    endif. " if ( ir_entity is bound )

    loop at lt_process_roles assigning field-symbol(<ls_process_role>).

      lv_field_name = |EMAILRULE| && |{ <ls_process_role>-acronym }|.

      assign component lv_field_name of structure <fs_structure> to <fs_value>.

      if <fs_value> is not initial.

        " Execute corresponding method

        "lv_method_name = |NOTIFY_| && |{ <ls_process_role>-name }|.
        lv_method_name = |NOTIFY_BY_PROCESS_ROLE|.

        lv_email_rule = <fs_value>.

        lt_receiver_email_addresses = me->get_email_addresses( <ls_process_role>-name ).

        loop at lt_receiver_email_addresses assigning field-symbol(<ls_receiver_email_address>).

          ptab = value #(
              ( name = 'IP_EMAIL_RULE' value = ref #( lv_email_rule ) kind = cl_abap_objectdescr=>exporting )
              ( name = 'IP_RECEIVER_EMAIL_ADDRESS' value = ref #( <ls_receiver_email_address> ) kind = cl_abap_objectdescr=>exporting )
          ).

          try.

              call method me->(lv_method_name)
                parameter-table
                ptab.

            catch cx_sy_dyn_call_error into data(lcx_process_exception).

              lv_log_record_text = lcx_process_exception->get_text(  ) .
              mo_log->yif_logger~err( lv_log_record_text  ).

              return.
          endtry.

        endloop.

      endif.

    endloop.

  endmethod.

  method fill_variables_values.

    mt_variables_values = value yst_text_tt_variables_values(
      ( variable = '&REQUESTERNAME' value = ms_problem_new_state-requestorfullname )
      ( variable = '&PROBMLEMID' value = ms_problem_new_state-objectid opentag = '<STRONG>' closetag = '</STRONG>' )
      ( variable = '&PROCESSORNAME' value = ms_problem_new_state-processorfullname )
      ( variable = '&STATUSOLD' value = ms_problem_old_state-statustext opentag = '<STRONG>' closetag = '</STRONG>' )
      ( variable = '&STATUSNEW' value = ms_problem_new_state-statustext opentag = '<STRONG>' closetag = '</STRONG>' )
      ( variable = '&FIELDS' value = me->fill_problem_details_html( ip_email_internal_flag  ) )
      ( variable = '&SIGNATURE' value = me->get_signature_for_product( ) )
    ).

  endmethod.

  method notify_by_process_role.

    data: lv_email_subj_text_name type tdobname,
          lv_email_body_text_name type tdobname,
          lv_email_internal_flag  type char1,
          lv_email_body           type string,
          lv_email_subject        type so_obj_des.

    if ip_receiver_email_address is not initial.

      me->get_email_rule(
         exporting
             ip_email_rule = ip_email_rule
             importing
             ep_email_internal_flag = lv_email_internal_flag
             ep_email_body_text = lv_email_body_text_name
             ep_email_subj_text = lv_email_subj_text_name ).

      me->fill_variables_values( lv_email_internal_flag ).

      lv_email_subject = me->get_compiled_text(
          exporting
              ip_text_name = lv_email_subj_text_name ).

      lv_email_body = me->get_compiled_text(
          exporting
              ip_use_tags = abap_true
              ip_text_name = lv_email_body_text_name ).

      me->send_email(
          exporting
              ip_receiver_email_address = ip_receiver_email_address
              ip_email_body = lv_email_body
              ip_email_subject = lv_email_subject ).

    endif.

  endmethod.

  method get_compiled_text.

    data  lo_standard_text  type ref to yif_standard_text.

    lo_standard_text = new ycl_standart_text(
    ip_text_language = 'R'
    ip_text_name = ip_text_name ).

    rp_compiled_text = lo_standard_text->get_compiled_text_by_name(
        exporting
            ip_use_tags = ip_use_tags
            it_variables_values =  mt_variables_values ).

  endmethod.

  method send_email.

    data: lo_email       type ref to yif_messenger,
          lt_recepients  type ymessenger_tt_addresses,
          ls_recepient   like line of lt_recepients,
          lt_attachments type ymessenger_tt_attachments,
          ls_attachment  like line of lt_attachments.

    lo_email = new ycl_email_messenger(  ).

    lo_email->set_message_subject( ip_email_subject ).

    lo_email->set_message_body(
        exporting
            ip_message_body_text = ip_email_body
            ip_message_body_type = 'HTM'
    ).

    ls_recepient = ip_receiver_email_address.
    append ls_recepient to lt_recepients.

    lo_email->set_message_recepients( lt_recepients ).
    lo_email->set_message_sender( mv_sender_email_address ).

    lo_email->send_message(  ).

  endmethod.

  method get_email_addresses.

    data:
      lo_bp_address_book type ref to yif_contacts_book,
      lv_email_address   type ymessenger_address.

    case ip_process_role.

      when 'REQUESTER'.

        lo_bp_address_book = new ycl_bp_master_data( ms_problem_new_state-requestorbusinesspartner ).
        lv_email_address = lo_bp_address_book->get_email_address(  ).

        append lv_email_address to rt_email_addresses.

      when 'PROCESSOR'.

        lo_bp_address_book = new ycl_bp_master_data( ms_problem_new_state-processorbusinesspartner ).
        lv_email_address = lo_bp_address_book->get_email_address(  ).

        append lv_email_address to rt_email_addresses.

      when 'OBSERVER'.

        if ( ms_problem_new_state-notifybycontactemail eq abap_true ).

          lv_email_address = ms_problem_new_state-contactemail.
          append lv_email_address to rt_email_addresses.

        endif.

      when 'SUPPORTTEAM'.

        rt_email_addresses = me->get_support_team_addresses( ).

    endcase.

*    " All participants except Observer have Business Partners
*    " For requester, processor, support team we use emails from BP
*    " For observer we use raw email from problem entity
*
*    if not ip_process_role eq 'OBSERVER'.
*
*      lo_bp_address_book = new zcl_bp_master_data( switch #( ip_process_role
*
*          when 'REQUESTER'    then ms_problem_new_state-requestorbusinesspartner
*          when 'PROCESSOR'    then ms_problem_new_state-processorbusinesspartner
*          when 'SUPPORTTEAM'  then ms_problem_new_state-supportteambusinesspartner ) ).
*
*      rp_email_address = lo_bp_address_book->get_email_address(  ).
*
*    else.
*
*      if ( ms_problem_new_state-notifybycontactemail eq abap_true ).
*        rp_email_address = ms_problem_new_state-contactemail.
*      endif.
*
*    endif.


  endmethod.

  method notify_by_email.

    me->get_stat_dependant_email_rules(  ).

    me->dispatch_emails(  ).

  endmethod.

  method set_sender_email_address.

    mv_sender_email_address = ip_sender_email_address.

  endmethod.

  method fill_problem_details_html.

     types: begin of ty_fields_to_fill,
             name        type name_komp,
             translation type char64,
             internal    type char1,
           end of ty_fields_to_fill.

    types: begin of ty_fields_to_replace,
             input_name  type name_komp,
             output_name type name_komp,
           end of ty_fields_to_replace.


    data: lt_fields_to_fill       type table of ty_fields_to_fill,
          lt_fields_to_replace    type table of ty_fields_to_replace,
          lo_structure_ref        type ref to data,
          lr_entity               type ref to data,
          lo_descr_ref            type ref to cl_abap_typedescr,
          lv_text_token           type string,
          lv_structure_field_name type name_komp.



    field-symbols: <fs_structure> type any,
                   <fs_value>     type any.

    lt_fields_to_fill = value #(

        ( name = 'DESCRIPTION' translation = 'Название/тема' internal = '' )
        ( name = 'OBJECTID' translation = 'Номер' internal = '' )
        ( name = 'PRIORITYTEXT' translation = 'Приоритет' internal = '' )
        ( name = 'PRODUCTTEXT' translation = 'Тип сервиса' internal = '' )
        ( name = 'REQUESTORFULLNAME' translation = 'Автор' internal = '' )
        ( name = 'CREATED_AT' translation = 'Дата создания (временная зона системы)' internal = '' )
        ( name = 'PROCESSORFULLNAME' translation = 'Обработчик' internal = 'X' )
        ( name = 'IRT_TIMESTAMP' translation = 'Время реакции' internal = 'X' )
        ( name = 'MPT_TIMESTAMP' translation = 'Максимальное время обработки' internal = 'X' )
        ( name = 'COMPANYNAME' translation = 'Компания автора' internal = 'X' )
    ).

    lt_fields_to_replace = value #(

       ( input_name = 'IRT_TIMESTAMP' output_name = 'IRT_TIMESTAMP_UTC' )
       ( input_name = 'MPT_TIMESTAMP' output_name = 'MPT_TIMESTAMP_UTC' )

       ).

    get reference of ms_problem_new_state into lr_entity.

    if ( lr_entity is bound ).

      assign lr_entity->* to <fs_structure>.

    endif. " if ( ir_entity is bound )


    loop at lt_fields_to_fill assigning field-symbol(<ls_field>).

      if <fs_value> is assigned.
        unassign <fs_value>.
      endif.

      clear: lv_text_token,
             lv_structure_field_name.

      try.

          lv_structure_field_name = lt_fields_to_replace[ input_name = <ls_field>-name ]-output_name.

        catch cx_sy_itab_line_not_found.

          lv_structure_field_name = <ls_field>-name.

      endtry.

      " assign component <ls_field>-name of structure <fs_structure> to <fs_value>.

      assign component lv_structure_field_name of structure <fs_structure> to <fs_value>.

      if ( <fs_value> is assigned ) and ( <fs_value> is not initial ).

        if ( <ls_field>-internal = 'X' ).

          if ip_email_internal_flag ne 'X'.

            continue.

          endif.

        endif.

        lo_descr_ref = cl_abap_typedescr=>describe_by_data( <fs_value> ).

        lv_text_token = <fs_value>.

        " Conversion of a timestamp data types:
        " yyyymmddhhmmss -> dd.mm.yyyy hh:mm:ss

        if ( lo_descr_ref->absolute_name eq '\TYPE=COMT_CREATED_AT_USR' ) or
            ( lo_descr_ref->absolute_name eq '\TYPE=CRMT_DATE_TIMESTAMP_FROM' ) .

          lv_text_token = ycl_assistant_utilities=>format_timestamp( <fs_value> ).

        endif.

        rp_problem_details = |{ rp_problem_details }| &&
        |{ <ls_field>-translation }| && |:| && | | && |{ lv_text_token }| &&
        |<br/>|.

      endif.

    endloop.

  endmethod.

  method yif_slpm_problem_observer~problem_created.

    me->yif_crm_order_change_notifier~notify(  ).

  endmethod.

  method yif_slpm_problem_observer~problem_updated.

    me->yif_crm_order_change_notifier~notify(  ).

  endmethod.

  method get_support_team_addresses.

    data: lv_team_to_inform         type pd_objid_r,
          lo_organizational_model   type ref to yif_organizational_model,
          lt_proc_pool_assigned_pos type yorg_model_tt_positions,
          wa_address                type ymessenger_address,
          lo_bp_address_book        type ref to yif_contacts_book.

    lv_team_to_inform = me->get_prod_org_unit_for_updates( ).

    if lv_team_to_inform is not initial.

      lo_organizational_model = new ycl_organizational_model( lv_team_to_inform ).

      lt_proc_pool_assigned_pos = lo_organizational_model->get_assig_pos_of_root_org_unit(  ).

      loop at lt_proc_pool_assigned_pos assigning field-symbol(<ls_proc_pool_assigned_pos>).

        lo_bp_address_book = new ycl_bp_master_data( <ls_proc_pool_assigned_pos>-businesspartner ).

        wa_address = lo_bp_address_book->get_email_address(  ).

        append wa_address to rt_addresses.

      endloop.

      sort rt_addresses.
      delete adjacent duplicates from rt_addresses.

    endif.

  endmethod.


  method get_prod_org_unit_for_updates.

*    data lo_slpm_product type ref to yif_slpm_product.
*
*    lo_slpm_product = new ycl_slpm_product( ms_problem_new_state-productguid ).
*
*    rp_org_unit = lo_slpm_product->get_org_unit_for_updates( ).

    if mo_slpm_product is not initial.

      rp_org_unit = mo_slpm_product->get_org_unit_for_updates( ).

    endif.

  endmethod.

  method get_signature_for_product.

    if mo_slpm_product is not initial.

      rp_signature = mo_slpm_product->get_signature_for_updates(  ).

    endif.

  endmethod.

  method set_slpm_product.

    mo_slpm_product = new ycl_slpm_product( ip_product_guid ).

  endmethod.

endclass.
