class ycl_custom_crm_order_sla_escal definition
  public
  final
  create public .

  public section.

    interfaces yif_custom_crm_order_sla_escal.

  protected section.
  private section.

    data:
      mt_escalation_setup           type table of ycrmo_sla_escal,
      mt_process_types_in_scope     type table of crmt_process_type,
      mt_esc_setup_for_process_type type table of ycrmo_sla_escal,
      mt_variables_values           type yst_text_tt_variables_values,
      ms_crm_order                  type ycrm_order_ts,
      mv_current_sla_value          type int4,
      mo_slpm_product               type ref to yif_slpm_product.

    class-data: mv_sender_email_address type ymessenger_address.

    methods:

      set_sla_escalation_setup,

      set_process_types_in_scope,

      process_escalations_from_setup
        raising
          ycx_crm_order_api_exc
          ycx_assistant_utilities_exc,

      process_single_process_type
        importing
          ip_process_type type crmt_process_type
        raising
          ycx_crm_order_api_exc
          ycx_assistant_utilities_exc,

      process_escalated_orders
        importing
          ip_process_type type crmt_process_type

        raising
          ycx_crm_order_api_exc
          ycx_assistant_utilities_exc,

      process_sla_data
        importing
          ip_guid           type crmt_object_guid
          ip_sla_type       type char3
          io_crm_order_read type ref to yif_custom_crm_order_read
          ip_process_type   type crmt_process_type optional
        raising
          ycx_crm_order_api_exc
          ycx_assistant_utilities_exc,

      compose_email_subject_and_body
        importing
          ip_emailsttextsubj type tdobname
          ip_emailsttextbody type tdobname
        exporting
          ep_subject         type so_obj_des
          ep_body            type string,

      fill_variables_values,

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

      get_email_addresses
        importing
          ip_process_role           type char64
        returning
          value(rt_email_addresses) type ymessenger_tt_addresses,

      add_escal_log_record
        importing
          ip_sla_type        type char3
          ip_process_type    type crmt_process_type
          ip_emailreceivers  type char1024
          ip_email_sent      type char1
          ip_percentage_low  type int4
          ip_percentage_high type int4,

      is_order_valid_for_mail_post
        importing
          is_escal_setup  type ycrmo_sla_escal
        returning
          value(rp_valid) type abap_bool,

      is_mail_post_possible
        importing
          is_escal_setup      type ycrmo_sla_escal
        returning
          value(rp_possibile) type abap_bool,


      get_order_sent_esc_mail_count
        returning
          value(rp_count) type int4,

      get_support_team_addresses
        returning value(rt_addresses) type zmessenger_tt_addresses,

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

class ycl_custom_crm_order_sla_escal implementation.

  method set_sender_email_address.

    mv_sender_email_address = ip_sender_email_address.

  endmethod.

  method yif_custom_crm_order_sla_escal~process_escalations.

    me->set_sla_escalation_setup(  ).

    me->set_process_types_in_scope( ).

    me->process_escalations_from_setup(  ).

  endmethod.

  method set_sla_escalation_setup.

    select
        mandt processtype slatype
        perclow perchigh sendemail emailsttextsubj
        emailsttextbody emailreceivers emailsender
        statusestoskip emailinworkinghours emailnotificationscount
            from ycrmo_sla_escal
                into table mt_escalation_setup.

  endmethod.

  method process_escalations_from_setup.

    loop at mt_process_types_in_scope assigning field-symbol(<ls_process_types_in_scope>).

      me->process_single_process_type( <ls_process_types_in_scope> ).

    endloop.

  endmethod.

  method process_single_process_type.

    me->process_escalated_orders(
        ip_process_type = ip_process_type
        ).

  endmethod.

  method process_escalated_orders.

    data:
      lo_crm_order_init       type ref to yif_custom_crm_order_init,
      lo_crm_order_read       type ref to yif_custom_crm_order_read,
      lt_guids                type ycrm_order_tt_guids,
      ls_crm_order_sla_status type ais_sla_status.

    " Preparing KPIs for process type

    clear mt_esc_setup_for_process_type.

    loop at mt_escalation_setup assigning field-symbol(<ls_setup_for_process_type>)
        where processtype = ip_process_type.

      append <ls_setup_for_process_type> to mt_esc_setup_for_process_type.

    endloop.

    " Looping through CRM orders

    lo_crm_order_init = new ycl_custom_crm_order_api(  ).

    lo_crm_order_init->set_process_type( ip_process_type ).

    lo_crm_order_read ?= lo_crm_order_init.

    lt_guids = lo_crm_order_read->get_guids_list(  ).

    loop at lt_guids assigning field-symbol(<ls_guid>).

      clear ls_crm_order_sla_status.

      ls_crm_order_sla_status = lo_crm_order_read->get_sla_status_by_guid( <ls_guid>-guid ).

      " Processing of IRT

      clear mv_current_sla_value.
      mv_current_sla_value = ls_crm_order_sla_status-irt_perc.

      me->process_sla_data(
        ip_guid = <ls_guid>-guid
        ip_sla_type = 'IRT'
        io_crm_order_read = lo_crm_order_read ).

      " Processing of MPT

      clear mv_current_sla_value.
      mv_current_sla_value = ls_crm_order_sla_status-mpt_perc.

      me->process_sla_data(
        ip_guid = <ls_guid>-guid
        ip_sla_type = 'MPT'
        io_crm_order_read = lo_crm_order_read ).

    endloop.

  endmethod.

  method set_process_types_in_scope.

    loop at mt_escalation_setup assigning field-symbol(<ls_escalation_setup_record>).

      append <ls_escalation_setup_record>-processtype to mt_process_types_in_scope.

    endloop.

    sort mt_process_types_in_scope.

    delete adjacent duplicates from mt_process_types_in_scope.

  endmethod.

  method process_sla_data.

    data:
      lv_percentage_low           type int4,
      lv_percentage_high          type int4,
      lt_email_receivers          type table of string,
      lv_emailsubj                type so_obj_des,
      lv_emailbody                type string,
      lv_is_email_address         type abap_bool,
      lv_receiver_email_address   type ymessenger_address,
      lv_process_role             type char64,
      lt_esc_setup_for_sla_type   type standard table of ycrmo_sla_escal,
      lt_statuses_to_skip         type table of j_estat,
      lv_sender_address           type string,
      lv_email_has_been_sent      type abap_bool,
      lt_receiver_email_addresses type ymessenger_tt_addresses.

    try.


        " Get all records for configured SLA type

        loop at mt_esc_setup_for_process_type assigning field-symbol(<ls_esc_setup_for_proc_type>)
            where    slatype = ip_sla_type.

          append <ls_esc_setup_for_proc_type> to lt_esc_setup_for_sla_type.

        endloop.

        loop at lt_esc_setup_for_sla_type assigning field-symbol(<ls_esc_setup_for_sla_type>).

          " Getting statuses to skip

          split <ls_esc_setup_for_sla_type>-statusestoskip at ';' into table lt_statuses_to_skip.

*          lv_percentage_low = mt_esc_setup_for_process_type[ slatype = ip_sla_type ]-perclow.
*          lv_percentage_high = mt_esc_setup_for_process_type[ slatype = ip_sla_type ]-perchigh.

          lv_percentage_low = <ls_esc_setup_for_sla_type>-perclow.
          lv_percentage_high = <ls_esc_setup_for_sla_type>-perchigh.

          if ( mv_current_sla_value ge lv_percentage_low ) and
            ( mv_current_sla_value le lv_percentage_high ).

            " Getting CRM order details

            clear ms_crm_order.

            ms_crm_order = io_crm_order_read->get_standard_fields_by_guid( ip_guid ).

            " Setting product

            me->set_slpm_product( ms_crm_order-productguid ).

            " Check if we need to skip the status

            if ( ms_crm_order-status is not initial ) and
                line_exists( lt_statuses_to_skip[ table_line = ms_crm_order-status ] ).

              return.

            endif.

            " Email posting process

            clear lv_email_has_been_sent.

            " Check if the order is valid for a mail posting

            if me->is_mail_post_possible( is_escal_setup = <ls_esc_setup_for_sla_type> ) eq abap_true.

              if ( ( <ls_esc_setup_for_sla_type>-sendemail eq 'X' ) and
              ( me->is_order_valid_for_mail_post( is_escal_setup = <ls_esc_setup_for_sla_type> ) eq abap_true ) ).

                " Setting sender

                clear lv_sender_address.

                lv_sender_address = <ls_esc_setup_for_sla_type>-emailsender.

                me->set_sender_email_address( lv_sender_address ).

                " Preparing body and subject

                compose_email_subject_and_body(
                    exporting
                        ip_emailsttextbody = <ls_esc_setup_for_sla_type>-emailsttextbody
                        ip_emailsttextsubj = <ls_esc_setup_for_sla_type>-emailsttextsubj
                    importing
                        ep_body = lv_emailbody
                        ep_subject = lv_emailsubj ).


                " Preparing receivers

                split <ls_esc_setup_for_sla_type>-emailreceivers at ';' into table lt_email_receivers.

                loop at lt_email_receivers assigning field-symbol(<ls_receiver>).

                  " Checking if it is a raw email or a token

                  clear: lv_is_email_address, lv_receiver_email_address.

                  lv_is_email_address = zcl_assistant_utilities=>is_valid_email_address( <ls_receiver> ).

                  if <ls_receiver> is initial.

                    continue.

                  endif.

                  if lv_is_email_address eq abap_true.

                    "lv_receiver_email_address = <ls_receiver>.

                    append <ls_receiver> to lt_receiver_email_addresses.

                  else.

                    " Getting email address from Business Partner data

                    lv_process_role = <ls_receiver>.

                    "lv_receiver_email_address = me->get_email_addresses( lv_process_role ).

                    lt_receiver_email_addresses = me->get_email_addresses( lv_process_role ).

                  endif.

                  " Sending email

                  if  ( lv_emailbody is not initial ) and
                          ( lv_emailsubj is not initial ).

                    loop at lt_receiver_email_addresses assigning field-symbol(<ls_receiver_email_address>).

                      if ( <ls_receiver_email_address> is not initial ).

                        me->send_email(
                          exporting
                              ip_receiver_email_address = <ls_receiver_email_address>
                              ip_email_body = lv_emailbody
                              ip_email_subject = lv_emailsubj ).
                      endif.

                    endloop.

                  endif.
                endloop.

                lv_email_has_been_sent = abap_true.

              endif.

            endif.

            " Adding escalation log record

            me->add_escal_log_record(
                exporting
                 ip_process_type = ip_process_type
                 ip_sla_type = ip_sla_type
                 ip_emailreceivers = <ls_esc_setup_for_sla_type>-emailreceivers
                 ip_email_sent = lv_email_has_been_sent
                 ip_percentage_high = lv_percentage_high
                 ip_percentage_low = lv_percentage_low
                 ).

          endif.

        endloop.

      catch cx_sy_itab_line_not_found.

    endtry.


  endmethod.

  method compose_email_subject_and_body.

    me->fill_variables_values( ).

    if ip_emailsttextsubj is not initial.

      ep_subject = me->get_compiled_text(
      exporting
          ip_text_name = ip_emailsttextsubj ).

    endif.

    if ip_emailsttextbody is not initial.

      ep_body = me->get_compiled_text(
          exporting
              ip_use_tags = abap_true
              ip_text_name = ip_emailsttextbody ).

    endif.

  endmethod.


  method fill_variables_values.

    mt_variables_values = value yst_text_tt_variables_values(
      ( variable = '&PROBMLEMID' value = ms_crm_order-objectid opentag = '<STRONG>' closetag = '</STRONG>' )
      ( variable = '&PERCENTAGE' value = mv_current_sla_value opentag = '<STRONG>' closetag = '</STRONG>' )
      ( variable = '&SIGNATURE' value = me->get_signature_for_product( ) )
     ).

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

        lo_bp_address_book = new ycl_bp_master_data( ms_crm_order-requestorbusinesspartner ).
        lv_email_address = lo_bp_address_book->get_email_address(  ).

        append lv_email_address to rt_email_addresses.

      when 'PROCESSOR'.

        lo_bp_address_book = new ycl_bp_master_data( ms_crm_order-processorbusinesspartner ).
        lv_email_address = lo_bp_address_book->get_email_address(  ).

        append lv_email_address to rt_email_addresses.

      when 'SUPPORTTEAM'.

        rt_email_addresses = me->get_support_team_addresses( ).

    endcase.


  endmethod.

  method add_escal_log_record.

    data wa_ycrmo_sla_esclog type ycrmo_sla_esclog.

    get time stamp field wa_ycrmo_sla_esclog-detectiontimestamp.


    wa_ycrmo_sla_esclog-processtype = ip_process_type.
    wa_ycrmo_sla_esclog-guid = ms_crm_order-guid.
    wa_ycrmo_sla_esclog-slatype = ip_sla_type.
    wa_ycrmo_sla_esclog-object_id = ms_crm_order-objectid.
    wa_ycrmo_sla_esclog-perc = mv_current_sla_value.
    wa_ycrmo_sla_esclog-status = ms_crm_order-status.
    wa_ycrmo_sla_esclog-emailreceivers = ip_emailreceivers.
    wa_ycrmo_sla_esclog-emailsent = ip_email_sent.
    wa_ycrmo_sla_esclog-perchigh = ip_percentage_high.
    wa_ycrmo_sla_esclog-perclow = ip_percentage_low.

    insert ycrmo_sla_esclog from wa_ycrmo_sla_esclog.


  endmethod.

  method is_order_valid_for_mail_post.

    rp_valid = abap_true.

    if ( is_escal_setup-emailnotificationscount > 0 ) and
        ( me->get_order_sent_esc_mail_count( ) ge is_escal_setup-emailnotificationscount ).

      rp_valid = abap_false.

    endif.

  endmethod.

  method get_order_sent_esc_mail_count.

    select count( * ) from ycrmo_sla_esclog
        into rp_count
            where guid eq ms_crm_order-guid and
            emailsent eq abap_true.

  endmethod.

  method is_mail_post_possible.

    data:
      lo_slpm_product  type ref to yif_crm_service_product,
      lv_avail_profile type char258, " srv_serwi,
      lo_serv_profile  type ref to yif_serv_profile.

    rp_possibile = abap_true.

    " Check for posting only in work time

    if is_escal_setup-emailinworkinghours eq abap_true.

      " Getting availability profile

      lo_slpm_product = new ycl_crm_service_product( ms_crm_order-productguid ).

      lv_avail_profile = lo_slpm_product->get_availability_profile_name(  ).

      if lv_avail_profile is not initial.

        lo_serv_profile = new ycl_serv_profile( lv_avail_profile ).

        if lo_serv_profile->is_now_an_availability_time( ) eq abap_false.

          rp_possibile = abap_false.

        endif.

      endif.

    endif.

  endmethod.

  method yif_custom_crm_order_sla_escal~clear_escal_log.

    delete from ycrmo_sla_esclog.

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

      lt_proc_pool_assigned_pos = lo_organizational_model->get_assigned_pos_of_org_unit(  ).

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

*    data lo_slpm_product type ref to zif_slpm_product.
*
*    lo_slpm_product = new zcl_slpm_product( ms_crm_order-productguid ).
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
