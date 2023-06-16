class ycl_custom_crm_order_api_proxy definition
  public
  create public .

  public section.

    interfaces:

      yif_custom_crm_order_create,
      yif_custom_crm_order_init,
      yif_custom_crm_order_read,
      yif_custom_crm_order_update,
      yif_custom_crm_order_organizer.

    methods constructor
      importing
        ip_process_type type crmt_process_type
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc.

  protected section.
  private section.

    data:
      mv_system_user                type ref to yif_system_user,
      mv_crm_user                   type ref to yif_crm_user,
      mv_process_type               type crmt_process_type,
      mo_custom_crm_order_init      type ref to yif_custom_crm_order_init,
      mo_custom_crm_order_read      type ref to yif_custom_crm_order_read,
      mo_custom_crm_order_create    type ref to yif_custom_crm_order_create,
      mo_custom_crm_order_update    type ref to yif_custom_crm_order_update,
      mo_custom_crm_order_organizer type ref to yif_custom_crm_order_organizer.

    methods:
      set_mo_custom_crm_order_create
        raising
          ycx_crm_order_api_exc,

      set_mo_custom_crm_order_update
        raising
          ycx_crm_order_api_exc.


endclass.

class ycl_custom_crm_order_api_proxy implementation.


  method constructor.

    mv_process_type = ip_process_type.

    mv_crm_user = new ycl_crm_user( sy-uname ).

    if ( mv_crm_user->is_auth_to_read_on_proc_type( mv_process_type ) eq abap_true ).

      create object mo_custom_crm_order_init type ycl_custom_crm_order_api.

      " If user is authorized for read operation, we create additional reference for create and update
      " Create and update authorizations will be checked in corresponding methods

      mo_custom_crm_order_read ?= mo_custom_crm_order_init.
      mo_custom_crm_order_organizer ?= mo_custom_crm_order_init.

    else.

      raise exception type ycx_crm_order_api_exc
        exporting
          textid  = ycx_crm_order_api_exc=>not_authorized_for_read
          ip_user = sy-uname.

    endif.

  endmethod.


  method yif_custom_crm_order_create~create_attachment.

    me->set_mo_custom_crm_order_create( ).

    if ( mo_custom_crm_order_create is bound ).

      mo_custom_crm_order_create->create_attachment(
        exporting
            ip_content = ip_content
            ip_file_name = ip_file_name
            ip_guid = ip_guid
            ip_mime_type = ip_mime_type ).

    endif.

  endmethod.


  method yif_custom_crm_order_create~create_text.


    " Potentially a user could have only UPDATE authorizations without CREATE, but
    " during an update he/she could require to create text. In this case an exception would be
    " raised as there are no CREATE authorizations. That's why we will check UPDATE authorizations
    " first and then CREATE authorizations check will follow, if there are no UPDATE rights

    me->set_mo_custom_crm_order_update(  ).

    if  ( mo_custom_crm_order_update is bound ).

      mo_custom_crm_order_update->create_text(
        exporting
            ip_guid = ip_guid
            ip_tdid = ip_tdid
            ip_text = ip_text ).


    else.

      me->set_mo_custom_crm_order_create( ).

      if ( mo_custom_crm_order_create is bound ).

        mo_custom_crm_order_create->create_text(
          exporting
              ip_guid = ip_guid
              ip_tdid = ip_tdid
              ip_text = ip_text ).

      endif.

    endif.

  endmethod.

  method yif_custom_crm_order_create~create_with_std_and_cust_flds.

    me->set_mo_custom_crm_order_create( ).

    if  ( mo_custom_crm_order_create is bound ).

      mo_custom_crm_order_create->create_with_std_and_cust_flds(
       exporting
        ir_entity = ir_entity
       importing
        ep_guid = ep_guid ).

    endif.

  endmethod.


  method yif_custom_crm_order_init~set_crm_category1.

    if ( mo_custom_crm_order_init is bound ).

      mo_custom_crm_order_init->set_crm_category1( ip_crm_category1 ).

    endif.

  endmethod.


  method yif_custom_crm_order_init~set_crm_category2.

    if ( mo_custom_crm_order_init is bound ).

      mo_custom_crm_order_init->set_crm_category2( ip_crm_category2 ).
    endif.

  endmethod.


  method yif_custom_crm_order_init~set_crm_category3.

    if ( mo_custom_crm_order_init is bound ).

      mo_custom_crm_order_init->set_crm_category3( ip_crm_category3 ).

    endif.

  endmethod.


  method yif_custom_crm_order_init~set_crm_category4.

    if ( mo_custom_crm_order_init is bound ).

      mo_custom_crm_order_init->set_crm_category4( ip_crm_category4 ).

    endif.

  endmethod.


  method yif_custom_crm_order_init~set_crm_cat_schema.

    if ( mo_custom_crm_order_init is bound ).

      mo_custom_crm_order_init->set_crm_cat_schema( ip_crm_cat_schema ).

    endif.

  endmethod.


  method yif_custom_crm_order_init~set_custom_fields_db_table.

    if ( mo_custom_crm_order_init is bound ).

      mo_custom_crm_order_init->set_custom_fields_db_table( ip_custom_fields_db_table ).

    endif.

  endmethod.


  method yif_custom_crm_order_init~set_db_struct_fields_map.

    if ( mo_custom_crm_order_init is bound ).

      mo_custom_crm_order_init->set_db_struct_fields_map( it_db_struct_fields_map ).

    endif.

  endmethod.


  method yif_custom_crm_order_init~set_process_type.

    if ( mo_custom_crm_order_init is bound ).

      mo_custom_crm_order_init->set_process_type( ip_process_type ).

    endif.

  endmethod.


  method yif_custom_crm_order_init~set_sold_to_party.

    if ( mo_custom_crm_order_init is bound ).

      mo_custom_crm_order_init->set_sold_to_party( ip_sold_to_party ).

    endif.

  endmethod.


  method yif_custom_crm_order_init~set_status_profile.

    if ( mo_custom_crm_order_init is bound ).

      mo_custom_crm_order_init->set_status_profile( ip_status_profile ).

    endif.

  endmethod.


  method yif_custom_crm_order_init~set_structure_name.

    if ( mo_custom_crm_order_init is bound ).

      mo_custom_crm_order_init->set_structure_name( ip_structure_name ).

    endif.

  endmethod.


  method yif_custom_crm_order_organizer~is_order_matching_to_filters.

    if ( mo_custom_crm_order_read is bound ).

      mo_custom_crm_order_organizer->is_order_matching_to_filters(
       exporting
           it_set_filters = it_set_filters
           ir_entity = ir_entity
       changing
            cp_include_record = cp_include_record ).

    endif.

  endmethod.


  method yif_custom_crm_order_organizer~sort_orders.

    if ( mo_custom_crm_order_read is bound ).

      er_entity = mo_custom_crm_order_organizer->sort_orders(
        exporting
            ir_entity = ir_entity
            it_order = it_order ).

    endif.


  endmethod.


  method yif_custom_crm_order_read~get_all_priorities_list.

    if ( mo_custom_crm_order_read is bound ).

      et_result = mo_custom_crm_order_read->get_all_priorities_list(  ).

    endif.

  endmethod.


  method yif_custom_crm_order_read~get_all_statuses_list.


    if ( mo_custom_crm_order_read is bound ).

      et_statuses = mo_custom_crm_order_read->get_all_statuses_list(  ).

    endif.

  endmethod.


  method yif_custom_crm_order_read~get_all_texts.

    mo_custom_crm_order_read->get_all_texts(
    exporting ip_guid = ip_guid
    importing et_texts = et_texts ).

  endmethod.


  method yif_custom_crm_order_read~get_attachments_list_by_guid.

    if ( mo_custom_crm_order_read is bound ).

      mo_custom_crm_order_read->get_attachments_list_by_guid(
        exporting
            ip_guid = ip_guid
        importing
            et_attachments_list_short = et_attachments_list_short
            et_attachments_list = et_attachments_list ).

    endif.

  endmethod.


  method yif_custom_crm_order_read~get_attachment_by_keys.

    if ( mo_custom_crm_order_read is bound ).

      er_attachment = mo_custom_crm_order_read->get_attachment_by_keys(
          exporting
              ip_guid = ip_guid
              ip_loio = ip_loio
              ip_phio = ip_phio ).

    endif.

  endmethod.


  method yif_custom_crm_order_read~get_attachment_content_by_keys.

    if ( mo_custom_crm_order_read is bound ).
      mo_custom_crm_order_read->get_attachment_content_by_keys(
        exporting
        ip_guid = ip_guid
        ip_loio = ip_loio
        ip_phio = ip_phio
        importing
        er_attachment = er_attachment
        er_stream = er_stream ).

    endif.

  endmethod.


  method yif_custom_crm_order_read~get_custom_fields_by_guid.

    if ( mo_custom_crm_order_read is bound ).

      call method mo_custom_crm_order_read->get_custom_fields_by_guid
        exporting
          ip_guid   = ip_guid
        importing
          es_result = es_result.

    endif.

  endmethod.


  method yif_custom_crm_order_read~get_guids_list.

    if ( mo_custom_crm_order_read is bound ).

      et_result = mo_custom_crm_order_read->get_guids_list(  ).

    endif.

  endmethod.


  method yif_custom_crm_order_read~get_last_text.

    if ( mo_custom_crm_order_read is bound ).

      es_text = mo_custom_crm_order_read->get_last_text( exporting ip_guid = ip_guid ).

    endif.

  endmethod.


  method yif_custom_crm_order_read~get_standard_fields_by_guid.

    if ( mo_custom_crm_order_read is bound ).

      es_result = mo_custom_crm_order_read->get_standard_fields_by_guid( ip_guid ).

    endif.

  endmethod.


  method yif_custom_crm_order_update~delete_attachment.

    me->set_mo_custom_crm_order_update(  ).

    if ( mo_custom_crm_order_update is bound ).

      mo_custom_crm_order_update->delete_attachment(
        exporting
             ip_guid = ip_guid
             ip_loio = ip_loio
             ip_phio = ip_phio ).

    endif.

  endmethod.

  method yif_custom_crm_order_update~update_order.

    me->set_mo_custom_crm_order_update(  ).

    if  ( mo_custom_crm_order_update is bound ).

      mo_custom_crm_order_update->update_order(
       exporting
        ir_entity = ir_entity
        ip_guid = ip_guid ).

    endif.

  endmethod.

  method yif_custom_crm_order_read~get_all_appointments_by_guid.

    if ( mo_custom_crm_order_read is bound ).

      rt_appointments = mo_custom_crm_order_read->get_all_appointments_by_guid( ip_guid ).

    endif.

  endmethod.

  method set_mo_custom_crm_order_create.

    if ( mo_custom_crm_order_create is not bound ) and
        ( mv_crm_user->is_auth_to_create_on_proc_type( mv_process_type ) eq abap_true ).

      mo_custom_crm_order_create ?= mo_custom_crm_order_init.

    else.

      raise exception type ycx_crm_order_api_exc
        exporting
          textid  = ycx_crm_order_api_exc=>not_authorized_for_create
          ip_user = sy-uname.

    endif.

  endmethod.

  method set_mo_custom_crm_order_update.


    if ( mo_custom_crm_order_update is not bound ) and
    ( mv_crm_user->is_auth_to_update_on_proc_type( mv_process_type ) eq abap_true ).

      mo_custom_crm_order_update ?= mo_custom_crm_order_init.

    else.

      raise exception type ycx_crm_order_api_exc
        exporting
          textid  = ycx_crm_order_api_exc=>not_authorized_for_update
          ip_user = sy-uname.

    endif.


  endmethod.

  method yif_custom_crm_order_update~create_text.


    me->set_mo_custom_crm_order_update( ).

    if ( mo_custom_crm_order_update is bound ).

      mo_custom_crm_order_update->create_text(
        exporting
            ip_guid = ip_guid
            ip_tdid = ip_tdid
            ip_text = ip_text ).

    endif.

  endmethod.


  method yif_custom_crm_order_read~get_sla_status_by_guid.

    if ( mo_custom_crm_order_read is bound ).

      rs_sla_status = mo_custom_crm_order_read->get_sla_status_by_guid( ip_guid ).

    endif.

  endmethod.

endclass.
