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
      mv_current_sla_value          type int4.

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

      get_email_address
        importing
          ip_process_role         type char64
        returning
          value(rp_email_address) type ymessenger_address

        .

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
        statusestoskip
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
      lv_percentage_low         type int4,
      lv_percentage_high        type int4,
      lt_email_receivers        type table of string,
      lv_emailsubj              type so_obj_des,
      lv_emailbody              type string,
      lv_is_email_address       type abap_bool,
      lv_receiver_email_address type ymessenger_address,
      lv_process_role           type char64,
      lt_esc_setup_for_sla_type type standard table of ycrmo_sla_escal,
      lt_statuses_to_skip       type table of j_estat,
      lv_sender_address         type string.

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

            " Check if we need to skip the status

            if ( ms_crm_order-status is not initial ) and
                line_exists( lt_statuses_to_skip[ table_line = ms_crm_order-status ] ).

              return.

            endif.

            " Email posting process

            if <ls_esc_setup_for_sla_type>-sendemail eq 'X'.

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

                lv_is_email_address = ycl_assistant_utilities=>is_valid_email_address( <ls_receiver> ).

                if <ls_receiver> is initial.

                  continue.

                endif.

                if lv_is_email_address eq abap_true.

                  lv_receiver_email_address = <ls_receiver>.

                else.

                  " Getting email address from Business Partner data

                  lv_process_role = <ls_receiver>.

                  lv_receiver_email_address = me->get_email_address( lv_process_role ).

                endif.

                " Sending email

                if ( lv_receiver_email_address is not initial ) and
                  ( lv_emailbody is not initial ) and
                      ( lv_emailsubj is not initial ).

                  me->send_email(
                    exporting
                        ip_receiver_email_address = lv_receiver_email_address
                        ip_email_body = lv_emailbody
                        ip_email_subject = lv_emailsubj ).
                endif.

              endloop.

            endif.

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

  method get_email_address.

    data lo_bp_address_book type ref to yif_contacts_book.


    lo_bp_address_book = new ycl_bp_master_data( switch #( ip_process_role

        when 'REQUESTER'    then ms_crm_order-requestorbusinesspartner
        when 'PROCESSOR'    then ms_crm_order-processorbusinesspartner
        when 'SUPPORTTEAM'  then ms_crm_order-supportteambusinesspartner ) ).

    rp_email_address = lo_bp_address_book->get_email_address(  ).


  endmethod.

endclass.
