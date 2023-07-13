class ycl_custom_crm_order_api definition
  public
  create public .

  public section.

    interfaces:

      yif_custom_crm_order_read,
      yif_custom_crm_order_create,
      yif_custom_crm_order_update,
      yif_custom_crm_order_init,
      yif_custom_crm_order_organizer.

    methods: is_order_matching_to_filters
      importing
        ir_entity                type ref to data
        it_set_filters           type /iwbep/t_mgw_select_option
      changing
        value(cp_include_record) type ac_bool,
      sort_orders
        importing
          ir_entity        type ref to data
          it_order         type /iwbep/t_mgw_sorting_order
        returning
          value(er_entity) type ref to data .

  protected section.
  private section.
    data: mv_status_profile         type crm_j_stsma,
          mt_db_struct_fields_map   type ycrm_order_tt_cust_fields_map,
          mv_custom_fields_db_table type tabname16,
          mv_process_type           type crmt_process_type,
          mv_structure_name         type strukname,
          mv_sold_to_party          type crmt_partner_no,
          mv_crm_category1          type crm_erms_cat_as_id,
          mv_crm_category2          type crm_erms_cat_as_id,
          mv_crm_category3          type crm_erms_cat_as_id,
          mv_crm_category4          type crm_erms_cat_as_id,
          mv_crm_cat_schema         type crm_erms_cat_as_id.

    methods: update_custom_orderadm_h_flds
      importing
        it_custom_fields_map type ycrm_order_tt_cust_fields_map
        ip_guid              type crmt_object_guid
      returning
        value(ep_rc)         type sy-subrc ,
      assign_customer_h_fields
        importing
          it_custom_fields_map type ycrm_order_tt_cust_fields_map
        returning
          value(es_customer_h) type crmt_customer_h_com ,
      get_texts
        importing
          ip_guid         type crmt_object_guid
        exporting
          value(et_texts) type cl_ai_crm_gw_mymessage_mpc=>tt_text
        raising
          ycx_crm_order_api_exc ,
      get_text_details_odata
        importing
          is_text           type comt_text_textdata
          it_text_cust      type comt_text_cust_struc1_tab
          is_orderadm_h_wrk type crmt_orderadm_h_wrk
        returning
          value(rs_entity)  type cl_ai_crm_gw_mymessage_mpc=>ts_text ,
      get_category_and_aspect_guids
        importing
          ip_asp_id   type crm_erms_cat_as_id
          ip_cat_id   type crm_erms_cat_ca_id
        exporting
          ep_asp_guid type crm_erms_cat_guid
          ep_cat_guid type crm_erms_cat_guid,
      get_category_tree
        importing
          ip_asp_guid             type crm_erms_cat_guid
          ip_cat_guid             type crm_erms_cat_guid
        returning
          value(rt_category_tree) type bsp_wd_dropdown_table.

endclass.

class ycl_custom_crm_order_api implementation.

  method yif_custom_crm_order_read~get_guids_list.

    select guid
      from crmd_orderadm_h
      into table et_result
      where process_type = me->mv_process_type.

  endmethod.


  method yif_custom_crm_order_read~get_all_statuses_list.

    select estat txt30 from tj30t into table et_statuses
    where stsma = mv_status_profile and spras = sy-langu.

  endmethod.

  method yif_custom_crm_order_read~get_all_priorities_list.

    data: ls_priority     type ycrm_order_ts_priority,
          lv_prioritytext type sc_text.

    data(lt_priorities) = cl_ai_crm_gw_utilities=>get_priorities( ).

    loop at lt_priorities assigning field-symbol(<ls_priority>).

      clear:
        ls_priority,
        lv_prioritytext.

      ls_priority-code = <ls_priority>-key.

      lv_prioritytext = substring_after( val = <ls_priority>-value sub =
':' ).
      condense lv_prioritytext.

      ls_priority-description = lv_prioritytext.

      append ls_priority to et_result.

    endloop. " loop at lt_priorities assigning field-symbol(<ls_priority>)


  endmethod.

  method yif_custom_crm_order_read~get_standard_fields_by_guid.

    data:
      lo_cl_ags_crm_1o_api     type ref to cl_ags_crm_1o_api,
      lo_categorization_schema type ref to
yif_crm_categorization_schema,
      lv_error_text            type bapi_msg,
      lv_user_status           type crm_j_status,
      lt_status                type standard table of tj30t,
      lt_partner               type crmt_partner_external_wrkt,
      lv_bp_num                type bu_partner,
      lv_processor_uname       type crmt_erms_agent_name,
      lv_requestor_id          type uname,
      lv_requestor_dep         type ad_dprtmnt,
      ls_orderadm_h            type crmt_orderadm_h_wrk,
      lv_prioritytext          type sc_text,
      ls_sla_status            type ais_sla_status,
      lt_product_data          type crmt_orderadm_i_wrkt,
      ls_product_data          type crmt_orderadm_i_wrk,
      ls_subject               type  crmt_subject_wrk,
      lt_subjects              type crmt_subject_wrkt,
      lt_appointments          type crmt_appointment_wrkt,
      ls_appointment           type crmt_appointment_wrk.

    " Getting request instance

    call method cl_ags_crm_1o_api=>get_instance
      exporting
        iv_header_guid                = ip_guid
        iv_process_mode               = 'C'
        iv_process_type               = mv_process_type
      importing
        eo_instance                   = lo_cl_ags_crm_1o_api
      exceptions
        invalid_parameter_combination = 1
        error_occurred                = 2
        others                        = 3.

    if sy-subrc <> 0.

      raise exception type ycx_crm_order_api_exc
        exporting
          textid  = ycx_crm_order_api_exc=>cant_get_document_by_guid
          ip_guid = ip_guid.

    endif. "  if sy-subrc <> 0

    " Assigning GUID

    es_result-guid = ip_guid.

    " Partners block

    lo_cl_ags_crm_1o_api->get_partners( importing et_partner = lt_partner ).

    if lt_partner is not initial.

      loop at lt_partner assigning field-symbol(<ls_partner>).

        clear lv_bp_num.

        lv_bp_num = <ls_partner>-partner_no.

        case <ls_partner>-ref_partner_fct.

            " Processor

          when 'SLFN0004'.

            es_result-processorbusinesspartner = lv_bp_num.

            " Get a Process Business Partner name

            es_result-processorfullname = new ycl_bp_master_data( lv_bp_num )->yif_contacts_book~get_full_name(  ).

            " Requester

          when 'SLFN0002'.

            es_result-requestorbusinesspartner = lv_bp_num.

            " Get a Requester Business Partner name

            es_result-requestorfullname = new ycl_bp_master_data( lv_bp_num )->yif_contacts_book~get_full_name(  ).

            " Support Team

          when 'SLFN0003'.

            es_result-supportteambusinesspartner = lv_bp_num.

        endcase. " case ls_partner-ref_partner_fct

      endloop. " loop at et_partner into ls_partner

    endif. " if lt_partner is not initial

    " Searching for request status

    lo_cl_ags_crm_1o_api->get_status( importing ev_user_status = lv_user_status ).

    es_result-status = lv_user_status.

    " Receiving status text

    select txt30 from tj30t into corresponding fields of table lt_status
       where estat = lv_user_status and
       stsma = mv_status_profile and
       spras = sy-langu.

    loop at lt_status assigning field-symbol(<ls_status>).

      es_result-statustext = <ls_status>-txt30.

    endloop.

    " Priority

    lo_cl_ags_crm_1o_api->get_priority( importing ev_priority = es_result-priority ).

    clear lv_prioritytext.

    select single txt_long into lv_prioritytext from scpriot where priority = es_result-priority and langu = sy-langu.

    lv_prioritytext = substring_after( val = lv_prioritytext sub = ':' ).
    condense lv_prioritytext.

    es_result-prioritytext = lv_prioritytext.

    " Getting ORDERADM_H record

    lo_cl_ags_crm_1o_api->get_orderadm_h(
      importing
        es_orderadm_h        = ls_orderadm_h ).


    if ls_orderadm_h is not initial.

      " Description / Short Text

      es_result-description = ls_orderadm_h-description.

      " Object ID

      es_result-objectid = ls_orderadm_h-object_id.

      " Posting date

      es_result-postingdate = ls_orderadm_h-posting_date.


      " Changed At

      es_result-changedat = ls_orderadm_h-changed_at.

      " Created At

      es_result-created_at = ls_orderadm_h-created_at.

    endif. " if ls_orderadm_h is not initial

    " SLA status

    ls_sla_status = lo_cl_ags_crm_1o_api->get_sla_status(  ).

    move-corresponding ls_sla_status to es_result.

    " Product

    "lo_cl_ags_crm_1o_api->get_items( importing et_orderadm_i = lt_product_data ).
    lo_cl_ags_crm_1o_api->get_service_products( importing et_orderadm_i = lt_product_data ).

    if lt_product_data is not initial.
      read table lt_product_data into ls_product_data index 1.

      es_result-productname = ls_product_data-ordered_prod.
      es_result-producttext = ls_product_data-description.
      es_result-productguid = ls_product_data-product.

    endif.

    " Category

*    lo_cl_ags_crm_1o_api->get_subject(
*        importing
*            es_subject = ls_subject
*            et_subject = lt_subjects ).
*
*    if ls_subject is not initial.
*
*      es_result-catschemaid = ls_subject-asp_id.
*      es_result-catschemanodeid = ls_subject-cat_id.
*
*      " Getting text descriptions
*
*      lo_categorization_schema = new ycl_crm_categorization_schema(ip_asp_id = ls_subject-asp_id ).
*
*      es_result-catschemaname = lo_categorization_schema->get_asp_label(  ).
*      es_result-catschemaname = lo_categorization_schema->get_hierarchy_cat_label( ls_subject-cat_id ).
*
*    endif.


    " First response timestamp, time and date (duration 'SMIN_RESPF')

    lo_cl_ags_crm_1o_api->get_appointments(
     importing
         et_appointment = lt_appointments ).

    try.

        ls_appointment = lt_appointments[ appt_type = 'SMIN_RESPF' ].

        if ls_appointment is not initial.

          es_result-firstreactiondate = ls_appointment-date_from.
          es_result-firstreactiontime = ls_appointment-time_from.
          es_result-firstreactiontimestamp = ls_appointment-timestamp_from.
          es_result-firstreactiontimezone = ls_appointment-timezone_from.

        endif.

      catch cx_sy_itab_line_not_found.

    endtry.


  endmethod.

  method yif_custom_crm_order_read~get_custom_fields_by_guid.

    data:
      lv_db_fields_list             type string,
      ls_crmd_customer_h            type  crmd_customer_h,
      lt_crmd_customer_h            type table of crmd_customer_h,
      ls_crmd_orderadm_h            type  crmd_orderadm_h,
      lt_crmd_orderadm_h            type table of crmd_orderadm_h,
      lv_error_text                 type string,
      lcx_sy_dynamic_osql_semantics type ref to cx_sy_dynamic_osql_semantics,
      table_ref                     type ref to data,
      wa_ref                        type ref to data.


    field-symbols: <ls_db_struct_fields_map> like line of
mt_db_struct_fields_map,
                   <lv_value>                type any,
                   <lv_value_db>             type any,
                   <fs_table>                type standard table,
                   <fs_table_wa>             type any.


    " Preparing list of db fields

    loop at mt_db_struct_fields_map assigning <ls_db_struct_fields_map>.

      if lv_db_fields_list is initial.

        lv_db_fields_list = <ls_db_struct_fields_map>-db_field.

      else.

        concatenate lv_db_fields_list <ls_db_struct_fields_map>-db_field
into lv_db_fields_list separated by space.

      endif. " if lv_db_fields_list is initial

    endloop. " loop at mt_db_struct_fields_map assigning field-symbol(<ls_db_struct_fields_map>)

    if lv_db_fields_list is initial.

      return.

    endif. " if lv_db_fields_list is initial

    try.

        if ( mv_custom_fields_db_table eq 'CRMD_CUSTOMER_H' ).

          select single (lv_db_fields_list)
            into corresponding fields of ls_crmd_customer_h
             from (mv_custom_fields_db_table) where guid = ip_guid.

        elseif ( mv_custom_fields_db_table eq 'CRMD_ORDERADM_H' ).

          select single (lv_db_fields_list)
            into corresponding fields of ls_crmd_orderadm_h
             from (mv_custom_fields_db_table) where guid = ip_guid.


        endif. " if ( mv_custom_fields_db_table eq 'CRMD_CUSTOMER_H' )

        if sy-subrc <> 0.

          return.

        endif.

        append ls_crmd_customer_h to lt_crmd_customer_h.

      catch cx_sy_dynamic_osql_semantics into lcx_sy_dynamic_osql_semantics.
        lv_error_text = lcx_sy_dynamic_osql_semantics->get_text( ).

    endtry.


    " Preparing output structure dynamically

    create data table_ref type standard table of (mv_structure_name).
    assign table_ref->* to <fs_table>.
    create data wa_ref like line of <fs_table>.
    assign wa_ref->* to <fs_table_wa>.


    if <ls_db_struct_fields_map>  is assigned.
      unassign <ls_db_struct_fields_map> .
    endif.


    " Filling the output structure

    loop at mt_db_struct_fields_map assigning <ls_db_struct_fields_map>.


      if ( mv_custom_fields_db_table eq 'CRMD_CUSTOMER_H' ).
        assign component <ls_db_struct_fields_map>-db_field of structure ls_crmd_customer_h to <lv_value_db>.
      elseif ( mv_custom_fields_db_table eq 'CRMD_ORDERADM_H' ).
        assign component <ls_db_struct_fields_map>-db_field of structure ls_crmd_orderadm_h to <lv_value_db>.
      endif.

      if <lv_value_db> is assigned.

        assign component <ls_db_struct_fields_map>-struct_field of structure <fs_table_wa> to <lv_value>.
        <lv_value> = <lv_value_db>.

      endif. " if <lv_value_db> is assigned

    endloop. " loop at lt_db_struct_fields_map assigning field-symbol (<ls_db_struct_fields_map>)

    append <fs_table_wa> to <fs_table>.

    get reference of <fs_table> into es_result.

  endmethod.

  method assign_customer_h_fields.

    data: ls_customer_h type crmt_customer_h_com.

    field-symbols:
      <fs_field> type any,
      <fs_value> type any.


    loop at it_custom_fields_map assigning field-symbol(<ls_custom_field_map>).

      if  <fs_field> is assigned.
        unassign  <fs_field>.
      endif.

      assign component <ls_custom_field_map>-db_field of structure ls_customer_h to <fs_field>.

      if <fs_value> is assigned.
        unassign <fs_value>.
      endif.

      assign <ls_custom_field_map>-value->* to <fs_value>.
      <fs_field> = <fs_value>.

    endloop. " loop at it_custom_fields_map assigning field-symbol(<ls_custom_field_map>)

    es_customer_h = ls_customer_h.

  endmethod.

  method update_custom_orderadm_h_flds.

    data: lv_fields_list type string,
          lt_update_set  type standard table of char72,
          wa_update_set  like line of lt_update_set.

    field-symbols: <fs_value> type any.

    loop at it_custom_fields_map assigning field-symbol(<ls_custom_field_map>).

      clear wa_update_set.

      if <fs_value> is assigned.
        unassign <fs_value>.
      endif.

      assign <ls_custom_field_map>-value->* to <fs_value>.

      describe field <fs_value> type data(lv_field_type).

      if lv_field_type eq 'C'.

        concatenate <ls_custom_field_map>-db_field '=' '''' into wa_update_set separated by space.
        concatenate wa_update_set <fs_value> '''' into wa_update_set.

      else.

        concatenate <ls_custom_field_map>-db_field '=' <fs_value> into wa_update_set separated by space.

      endif.


      append wa_update_set to lt_update_set.

    endloop. " loop at it_custom_fields_map assigning field-symbol(<ls_custom_field_map>)

    update  crmd_orderadm_h set (lt_update_set)
          where
           guid = ip_guid.

    ep_rc = sy-subrc.

  endmethod.

  method yif_custom_crm_order_create~create_with_std_and_cust_flds.

    constants:
       lc_crm_create    type crmt_mode value 'A'.

    data:
      lo_cl_ags_crm_1o_api    type ref to cl_ags_crm_1o_api,
      lo_cl_ags_crm_1o_api_sd type ref to cl_ags_crm_1o_api_sd,
      lv_user_partner_in      type bu_partner,
      lv_user_partner_out     type crmt_partner_no,
      lt_text                 type crmt_text_comt,
      ls_text                 type crmt_text_com,
      ls_text_line            type tline,
      lt_text_lines           type comt_text_lines_t,
      lv_guid                 type crmt_object_guid,
      lo_log_handle           type balloghndl,
      lv_user_id              type uname,
      lv_short_text           type crmt_process_description,
      lo_structure_ref        type ref to data,
      lv_note                 type string,
      lv_priority             type crmt_priority,
      lv_rc                   type sy-subrc,
      lt_custom_fields_map    type ycrm_order_tt_cust_fields_map,
      ls_custom_fields_map    like line of lt_custom_fields_map,
      ls_crmd_customer_h      type  crmd_customer_h,
      ls_crmd_orderadm_h      type  crmd_orderadm_h,
      ls_customer_h           type crmt_customer_h_com,
      lv_posting_date         type dats,
      ls_subject              type crmt_subject_com,
      lv_attr_name            type seocpdname,
      lv_iterator             type int1 value 4.


    field-symbols: <fs_entity>     type any,
                   <fs_structure>  type any,
                   <fs_value>      type any,
                   <field>         type any,
                   <lv_attr_value> type char40.

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Starting an incident API creation sequence
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if mv_process_type is initial.
      raise exception type ycx_crm_order_api_exc
        exporting
          textid = ycx_crm_order_api_exc=>process_type_not_set.
    endif. " if ip_process_type is initial

    cl_ags_crm_1o_api=>get_instance(
    exporting
      iv_process_mode = lc_crm_create
      iv_process_type = mv_process_type
    importing
      eo_instance = lo_cl_ags_crm_1o_api
      ).

    if lo_cl_ags_crm_1o_api is not bound.
      raise exception type ycx_crm_order_api_exc
        exporting
          textid = ycx_crm_order_api_exc=>standard_crm_api_internal_err.
    endif. " if lo_cl_ags_crm_1o_api is not bound

    lo_cl_ags_crm_1o_api_sd ?= lo_cl_ags_crm_1o_api.




    lo_cl_ags_crm_1o_api->get_service_products( importing et_orderadm_i = data(lt_product_data) ).



    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting new GUID
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    lv_guid = lo_cl_ags_crm_1o_api->get_guid( ).

    if lv_guid is initial.

      raise exception type ycx_crm_order_api_exc
        exporting
          textid = ycx_crm_order_api_exc=>cant_get_guid_of_new_instance.
    endif. " if lv_guid is initial

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting a reporter from sy-uname
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    lv_user_id = sy-uname.

    condense lv_user_id.

    call function 'CRM_ERMS_FIND_BP_FOR_USER'
      exporting
        iv_user_id = lv_user_id
      importing
        ev_bupa_no = lv_user_partner_in.

    lv_user_partner_out = lv_user_partner_in.

    if lv_user_partner_out is not initial.

      lo_cl_ags_crm_1o_api_sd->set_reporter( iv_partner_number = lv_user_partner_out ).


    endif. " if lv_user_partner_out is not INITIAL


    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting a sold-to-party (if required)
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if ( mv_sold_to_party is not initial ).

      lo_cl_ags_crm_1o_api_sd->set_soldto( iv_partner_number = mv_sold_to_party ).

    endif.

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Decode incoming abstract entity
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    create data lo_structure_ref type standard table of (mv_structure_name).
    assign lo_structure_ref->* to <fs_structure>.

    if ( ir_entity is bound ).

      assign ir_entity->* to <fs_structure>.

    endif. " if ( ir_entity is bound )

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting a processor
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'PROCESSORBUSINESSPARTNER' of structure <fs_structure> to <fs_value>.

    if <fs_value> is not initial.

      lv_user_partner_in = <fs_value>.
      lv_user_partner_out = lv_user_partner_in.

      lo_cl_ags_crm_1o_api_sd->set_processor( iv_partner_number =
lv_user_partner_out ).

    endif.

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting Description
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    " if ip_description is not initial.


    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'DESCRIPTION' of structure <fs_structure> to <fs_value>.

    if <fs_value> is not initial.

      lv_short_text = <fs_value>.

      lo_cl_ags_crm_1o_api->set_short_text( exporting iv_short_text = lv_short_text ).

    endif. " if er_entity-description is not INITIAL

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting Text
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'NOTE' of structure <fs_structure> to <fs_value>.

    if <fs_value> is not initial.

      lv_note = <fs_value>.

      ls_text-ref_guid = lv_guid.
      ls_text-tdid = 'SU99'.
      ls_text-tdstyle = 'SYSTEM'.        .
      ls_text-mode = 'I'.
      ls_text_line-tdline = lv_note.

      append ls_text_line to lt_text_lines.

      ls_text-lines = lt_text_lines.
      append ls_text to lt_text.

      lo_cl_ags_crm_1o_api->set_texts(
         exporting
           it_text           =  lt_text
         exceptions
           error_occurred    = 1
           document_locked   = 2
           no_change_allowed = 3
           no_authority      = 4
           others            = 5
       ).

      if sy-subrc <> 0.

        raise exception type ycx_crm_order_api_exc
          exporting
            textid = ycx_crm_order_api_exc=>standard_crm_api_internal_err.

      endif. "  if sy-subrc <> 0

    endif. " if er_entity-note is not INITIAL

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting Priority
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'PRIORITY' of structure <fs_structure> to <fs_value>.

    if <fs_value> is not initial.

      lv_priority = <fs_value>.

      lo_cl_ags_crm_1o_api->set_priority( exporting iv_priority = lv_priority ).

    endif. " if er_entity-priority is not initial

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting categorization from parameters
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if me->mv_crm_cat_schema is not initial.

      do 4 times.

        clear ls_subject.

        lv_attr_name = |MV_CRM_CATEGORY| && |{ lv_iterator }|.

        if <lv_attr_value> is assigned.
          unassign <lv_attr_value>.
        endif.

        assign  me->(lv_attr_name) to <lv_attr_value>.

        if  <lv_attr_value> is not initial.

          ls_subject-asp_id = me->mv_crm_cat_schema.
          ls_subject-cat_id = <lv_attr_value>.
          ls_subject-katalog_type = 'D'.
          ls_subject-ref_guid = lv_guid.
          ls_subject-ref_kind = 'A'.
          ls_subject-profile_type = 'A'.
          ls_subject-mode = 'A'.

          lo_cl_ags_crm_1o_api->set_subject(
            exporting
              is_subject        = ls_subject
            changing
              cv_log_handle     = lo_log_handle
            exceptions
              error_occurred    = 1
              document_locked   = 2
              no_change_allowed = 3
              no_authority      = 4
              others            = 5
          ).

          exit.

        endif. " if  <lv_attr_value> is assigned

        lv_iterator = lv_iterator - 1.

      enddo. " do 4 times

    endif. " if me->yif_custom_crm_order_api~mv_crm_cat_schema is not initial





    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Custom fields in case we use CRMD_CUSTOMER_H
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    " Preparing a list of custom fields which are not initial


    loop at mt_db_struct_fields_map assigning field-symbol(<ls_db_struct_fields_map>).

      if <fs_value> is assigned.
        unassign <fs_value>.
      endif.

      assign component <ls_db_struct_fields_map>-struct_field of structure <fs_structure> to <fs_value>.

      if <fs_value> is not initial.

        ls_custom_fields_map-db_field = <ls_db_struct_fields_map>-db_field.

        assign ls_custom_fields_map-value->* to <field>.

        if <field> is assigned.
          unassign <field>.
        endif.

        if ( mv_custom_fields_db_table eq 'CRMD_CUSTOMER_H' ).

          assign component ls_custom_fields_map-db_field of structure ls_crmd_customer_h to <field>.

        elseif ( mv_custom_fields_db_table eq 'CRMD_ORDERADM_H' ).

          assign component ls_custom_fields_map-db_field of structure ls_crmd_orderadm_h to <field>.

        endif. " if ( mv_custom_fields_db_table eq 'CRMD_CUSTOMER_H' )


        <field> = <fs_value>.

        get reference of <field> into ls_custom_fields_map-value.

        append ls_custom_fields_map to lt_custom_fields_map.


      endif. " if <fs_value> is not initial


    endloop. " loop at mt_db_struct_fields_map assigning field-symbol(<ls_db_struct_fields_map>)

    if ( mv_custom_fields_db_table eq 'CRMD_CUSTOMER_H' ).

      clear ls_customer_h.

      ls_customer_h = me->assign_customer_h_fields(
          exporting
            it_custom_fields_map = lt_custom_fields_map ).

      if ls_customer_h is not initial.

        lo_cl_ags_crm_1o_api->set_customer_h( exporting is_customer_h = ls_customer_h ).

      endif.

    endif. " if ( mv_custom_fields_db_table eq 'CRMD_CUSTOMER_H' )

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Saving document
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    lo_cl_ags_crm_1o_api->save(
      changing
        cv_log_handle = lo_log_handle
      exceptions
        others = 1 ).

    if ( sy-subrc <> 0 ).


      raise exception type ycx_crm_order_api_exc
        exporting
          textid = ycx_crm_order_api_exc=>error_saving_document.


    endif. " if ( sy-subrc <> 0 )


    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting Posting Date
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'POSTINGDATE' of structure <fs_structure> to <fs_value>.

    if <fs_value> is not initial.

      lv_posting_date = <fs_value>.

      update crmd_orderadm_h set posting_date = lv_posting_date where guid = lv_guid.


    endif. " if er_entity-priority is not initial


    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Product
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'PRODUCTGUID' of structure <fs_structure> to <fs_value>.

    if <fs_value> is not initial.

      data lo_crm_product type ref to yif_crm_product.

      data:

        lv_product_id   type   comt_product_id,
        lv_product_name type   comt_prshtextx.

      lo_crm_product = new ycl_crm_product( <fs_value> ).

      if lo_crm_product is bound.

        lv_product_id = lo_crm_product->get_id(  ).
        lv_product_name = lo_crm_product->get_name(  ).

        " Replacing a default product with the one we received from a frontend

        if ( lv_product_id is not initial )
        and ( lv_product_name is not initial ).

          update crmd_orderadm_i set
              product = <fs_value>
              ordered_prod = lv_product_id
              description = lv_product_name
              description_uc = lv_product_name
          where header = lv_guid.

        endif.

      endif.

    endif.


    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Custom fields in case we use CRMD_ORDERADM_H
    "   We must update CRMD_CUSTOMER_H after we saved an order
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^


    if ( mv_custom_fields_db_table eq 'CRMD_ORDERADM_H' ).

      select count( * ) up to 1 rows from crmd_orderadm_h
        where guid =  lv_guid.

      if sy-subrc = 0.

        lv_rc = update_custom_orderadm_h_flds(
            exporting
              ip_guid = lv_guid
              it_custom_fields_map = lt_custom_fields_map ).

        if  lv_rc <> 0.

          raise exception type ycx_crm_order_api_exc
            exporting
              textid = ycx_crm_order_api_exc=>error_saving_cust_flds_of_doc.


        endif. " if  lv_rc <> 0

      endif. " if sy-subrc = 0

    endif. " if ( mv_custom_fields_db_table eq 'CRMD_ORDERADM_H' )

    if lv_guid is not initial.

      ep_guid = lv_guid.

    endif.



  endmethod.

  method yif_custom_crm_order_read~get_attachments_list_by_guid.

    data: lt_urls   type ict_crm_urls,
          ls_entity type cl_ai_crm_gw_mymessage_mpc=>ts_attachment.

    call function 'ICT_READ_ATTACHMENTS'
      exporting
        i_crm_id     = ip_guid
        i_ids_only   = abap_true
      importing
        e_t_document = et_attachments_list_short
        e_t_url      = lt_urls.

    loop at et_attachments_list_short assigning field-symbol(<docs>).
      ls_entity = cl_ai_crm_gw_attachment=>get_attachment_details_odata( is_abap_doc = <docs> iv_guid = ip_guid ).

      " Additional check for formatted date (standard code of cl_ai_crm_gw_attachment is skipping date formatting)

      if ls_entity-upload_date_formatted is initial.

        ls_entity-upload_date_formatted = ycl_assistant_utilities=>format_timestamp( ls_entity-upload_date ).

      endif.

      append ls_entity to et_attachments_list.

    endloop.


  endmethod.

  method is_order_matching_to_filters.

    data: lt_filter_sel_opt type /iwbep/t_cod_select_options,
          lo_structure_ref  type ref to data.

    field-symbols: <fs_structure> type any,
                   <fs_value>     type any.

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Decode incoming abstract entity
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    create data lo_structure_ref type standard table of (mv_structure_name).
    assign lo_structure_ref->* to <fs_structure>.

    if ( ir_entity is bound ).

      assign ir_entity->* to <fs_structure>.

    endif. " if ( ir_entity is bound )

    loop at it_set_filters assigning field-symbol(<ls_set_filters>).

      clear lt_filter_sel_opt.
      lt_filter_sel_opt = <ls_set_filters>-select_options.

      assign component <ls_set_filters>-property of structure <fs_structure> to <fs_value>.

      if ( sy-subrc eq 0 ).

        if <fs_value> not in lt_filter_sel_opt.

          cp_include_record = abap_false.
          exit.

        endif.

      endif. " if ( sy-subrc eq 0 ) and ( <fs_value> is not initial )



    endloop. " loop at it_set_filters ASSIGNING FIELD-SYMBOL(<ls_set_filters>)


  endmethod.

  method sort_orders.

    data: lo_table_ref type ref to data,
          lt_order_tab type abap_sortorder_tab,
          ls_order_tab type abap_sortorder.

    field-symbols:
      <fs_table> type any table,
      <fs_value> type any.

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Decode incoming abstract entity
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    create data lo_table_ref type standard table of (mv_structure_name).
    assign lo_table_ref->* to <fs_table>.

    if ( ir_entity is bound ).

      assign ir_entity->* to <fs_table>.

    endif. " if ( ir_entity is bound )

    loop at it_order assigning field-symbol(<ls_order>).

      if <ls_order>-order = 'desc'.
        ls_order_tab-descending = abap_true.
      else.
        ls_order_tab-descending = abap_false.
      endif.


      search <ls_order>-property for 'Date' .
      if sy-subrc <> 0.
        ls_order_tab-astext = abap_true.
      endif.

      ls_order_tab-name = <ls_order>-property.

      translate ls_order_tab-name to upper case.

      append ls_order_tab to lt_order_tab.

      clear:
        ls_order_tab.

    endloop. " loop at it_order assigning field-symbol(<ls_order>)


    try.
        sort <fs_table> by (lt_order_tab).
      catch cx_sy_dyn_table_ill_comp_val.
    endtry.

    get reference of <fs_table> into er_entity.

  endmethod.

  method yif_custom_crm_order_update~update_order.

    constants:
      lc_crm_update   type crmt_mode value 'B'.

    data:
      lo_cl_ags_crm_1o_api    type ref to cl_ags_crm_1o_api,
      lo_cl_ags_crm_1o_api_sd type ref to cl_ags_crm_1o_api_sd,
      lv_user_partner_in      type bu_partner,
      lv_user_partner_out     type crmt_partner_no,
      lt_text                 type crmt_text_comt,
      ls_text                 type crmt_text_com,
      ls_text_line            type tline,
      lt_text_lines           type comt_text_lines_t,
      lo_log_handle           type balloghndl,
      lv_user_id              type uname,
      lv_short_text           type crmt_process_description,
      lo_structure_ref        type ref to data,
      lv_note                 type string,
      lv_priority             type crmt_priority,
      lv_rc                   type sy-subrc,
      lt_custom_fields_map    type ycrm_order_tt_cust_fields_map,
      ls_custom_fields_map    like line of lt_custom_fields_map,
      ls_crmd_customer_h      type  crmd_customer_h,
      ls_crmd_orderadm_h      type  crmd_orderadm_h,
      ls_customer_h           type crmt_customer_h_com,
      lt_partners             type comt_partner_comt,
      ls_partners             type comt_partner_com,
      lt_status               type crmt_status_comt,
      ls_status               type crmt_status_com,
      ev_log_handle           type balloghndl,
      lv_supportteam          type crmt_partner_no.

    field-symbols: <fs_entity>    type any,
                   <fs_structure> type any,
                   <fs_value>     type any,
                   <field>        type any.

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Starting an incident API update sequence
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if mv_process_type is initial.
      raise exception type ycx_crm_order_api_exc
        exporting
          textid = ycx_crm_order_api_exc=>process_type_not_set.
    endif. " if ip_process_type is initial

    cl_ags_crm_1o_api=>get_instance(
    exporting
      iv_header_guid  = ip_guid
      iv_process_mode = lc_crm_update
      iv_process_type = mv_process_type
    importing
      eo_instance = lo_cl_ags_crm_1o_api
      ).

    if lo_cl_ags_crm_1o_api is not bound.
      raise exception type ycx_crm_order_api_exc
        exporting
          textid = ycx_crm_order_api_exc=>standard_crm_api_internal_err.
    endif. " if lo_cl_ags_crm_1o_api is not bound

    lo_cl_ags_crm_1o_api_sd ?= lo_cl_ags_crm_1o_api.

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Decode incoming abstract entity
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    create data lo_structure_ref type standard table of (mv_structure_name).
    assign lo_structure_ref->* to <fs_structure>.

    if ( ir_entity is bound ).

      assign ir_entity->* to <fs_structure>.

    endif. " if ( ir_entity is bound )

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting Description
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'DESCRIPTION' of structure <fs_structure> to
<fs_value>.

    if <fs_value> is not initial.

      lv_short_text = <fs_value>.

      lo_cl_ags_crm_1o_api->set_short_text( exporting iv_short_text =
lv_short_text ).

    endif. " if er_entity-description is not INITIAL

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting Text
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'NOTE' of structure <fs_structure> to <fs_value>.

    if <fs_value> is not initial.

      lv_note = <fs_value>.

      ls_text-ref_guid = ip_guid.
      ls_text-tdid = 'SU01'.
      ls_text-tdstyle = 'SYSTEM'.        .
      ls_text-mode = 'I'.
      ls_text_line-tdline = lv_note.

      append ls_text_line to lt_text_lines.

      ls_text-lines = lt_text_lines.
      append ls_text to lt_text.

      lo_cl_ags_crm_1o_api->set_texts(
         exporting
           it_text           =  lt_text
         exceptions
           error_occurred    = 1
           document_locked   = 2
           no_change_allowed = 3
           no_authority      = 4
           others            = 5
       ).

      if sy-subrc <> 0.

        raise exception type ycx_crm_order_api_exc
          exporting
            textid = ycx_crm_order_api_exc=>standard_crm_api_internal_err.

      endif. "  if sy-subrc <> 0

    endif. " if er_entity-note is not INITIAL

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting Priority
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'PRIORITY' of structure <fs_structure> to
<fs_value>.

    if <fs_value> is not initial.

      lv_priority = <fs_value>.

      lo_cl_ags_crm_1o_api->set_priority( exporting iv_priority = lv_priority ).

    endif. " if er_entity-priority is not initial


    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting Processor
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'PROCESSORBUSINESSPARTNER' of structure <fs_structure> to <fs_value>.

    if <fs_value> is not initial.

      lv_user_partner_in = <fs_value>.
      lv_user_partner_out = lv_user_partner_in.

    endif. " if er_entity-priority is not initial

    lo_cl_ags_crm_1o_api_sd->set_processor( iv_partner_number = lv_user_partner_out ).


    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting Status
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'STATUS' of structure <fs_structure> to <fs_value>.

    if <fs_value> is not initial.

      ls_status-status = <fs_value>.

      if ls_status-status is not initial.

        ls_status-ref_guid = ip_guid.
        ls_status-ref_kind = 'A'.
        ls_status-user_stat_proc = mv_status_profile.
        ls_status-activate = 'X'.

        append ls_status to lt_status.

        lo_cl_ags_crm_1o_api->set_status(
          exporting
            it_status             = lt_status
          changing
            cv_log_handle         = ev_log_handle
          exceptions
            document_locked       = 1
            error_occurred        = 2
            no_authority          = 3
            no_change_allowed     = 4
            others                = 5
        ).

      endif. " if ls_status-status is not initial

    endif. " if <fs_value> is not initial


    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Setting Support Team
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if <fs_value> is assigned.
      unassign <fs_value>.
    endif.

    assign component 'SUPPORTTEAMBUSINESSPARTNER' of structure <fs_structure> to <fs_value>.

    if <fs_value> is not initial.

      lv_supportteam = <fs_value>.

      lo_cl_ags_crm_1o_api_sd->set_team( lv_supportteam ).

    endif. " if er_entity-priority is not initial

    " Preparing a list of custom fields which are not initial


    loop at mt_db_struct_fields_map assigning field-symbol(<ls_db_struct_fields_map>).

      if <fs_value> is assigned.
        unassign <fs_value>.
      endif.

      assign component <ls_db_struct_fields_map>-struct_field of structure <fs_structure> to <fs_value>.

      if <fs_value> is not initial.

        ls_custom_fields_map-db_field = <ls_db_struct_fields_map>-db_field.

        assign ls_custom_fields_map-value->* to <field>.

        if <field> is assigned.
          unassign <field>.
        endif.

        if ( mv_custom_fields_db_table eq 'CRMD_CUSTOMER_H' ).

          assign component ls_custom_fields_map-db_field of structure ls_crmd_customer_h to <field>.

        elseif ( mv_custom_fields_db_table eq 'CRMD_ORDERADM_H' ).

          assign component ls_custom_fields_map-db_field of structure ls_crmd_orderadm_h to <field>.

        endif. " if ( mv_custom_fields_db_table eq 'CRMD_CUSTOMER_H' )


        <field> = <fs_value>.

        get reference of <field> into ls_custom_fields_map-value.

        append ls_custom_fields_map to lt_custom_fields_map.


      endif. " if <fs_value> is not initial


    endloop. " loop at mt_db_struct_fields_map assigning field-symbol(<ls_db_struct_fields_map>)

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Custom fields in case we use CRMD_CUSTOMER_H
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    if ( mv_custom_fields_db_table eq 'CRMD_CUSTOMER_H' ).

      clear ls_customer_h.

      ls_customer_h = me->assign_customer_h_fields(
          exporting
            it_custom_fields_map = lt_custom_fields_map ).

      if ls_customer_h is not initial.

        lo_cl_ags_crm_1o_api->set_customer_h( exporting is_customer_h = ls_customer_h ).

      endif.

    endif. " if ( mv_custom_fields_db_table eq 'CRMD_CUSTOMER_H' )


    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Saving document
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^

    lo_cl_ags_crm_1o_api->save(
      changing
        cv_log_handle = lo_log_handle
      exceptions
        others = 1 ).

    if ( sy-subrc <> 0 ).


      raise exception type ycx_crm_order_api_exc
        exporting
          textid = ycx_crm_order_api_exc=>error_saving_document.


    endif. " if ( sy-subrc <> 0 )

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Custom fields in case we use CRMD_ORDERADM_H
    "   We must update CRMD_CUSTOMER_H after we saved an order
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^


    if ( mv_custom_fields_db_table eq 'CRMD_ORDERADM_H' ).

      select count( * ) up to 1 rows from crmd_orderadm_h
        where guid =  ip_guid.

      if sy-subrc = 0.

        lv_rc = update_custom_orderadm_h_flds(
            exporting
              ip_guid = ip_guid
              it_custom_fields_map = lt_custom_fields_map ).

        if  lv_rc <> 0.

          raise exception type ycx_crm_order_api_exc
            exporting
              textid = ycx_crm_order_api_exc=>error_saving_cust_flds_of_doc.

        endif. " if  lv_rc <> 0

      endif. " if sy-subrc = 0

    endif. " if ( mv_custom_fields_db_table eq 'CRMD_ORDERADM_H' )


  endmethod.

  method yif_custom_crm_order_init~set_status_profile.

    if me->mv_status_profile is initial.

      me->mv_status_profile = ip_status_profile.

    endif.

  endmethod.

  method yif_custom_crm_order_init~set_db_struct_fields_map.

    if me->mt_db_struct_fields_map is initial.

      me->mt_db_struct_fields_map = it_db_struct_fields_map.

    endif.

  endmethod.

  method yif_custom_crm_order_init~set_custom_fields_db_table.

    if me->mv_custom_fields_db_table is initial.

      me->mv_custom_fields_db_table = ip_custom_fields_db_table.

    endif.

  endmethod.

  method yif_custom_crm_order_init~set_process_type.

    if me->mv_process_type is initial.

      me->mv_process_type = ip_process_type.

    endif.

  endmethod.

  method yif_custom_crm_order_init~set_structure_name.

    if me->mv_structure_name is initial.

      me->mv_structure_name = ip_structure_name.

    endif.

  endmethod.

  method yif_custom_crm_order_init~set_sold_to_party.

    if me->mv_sold_to_party is initial.

      me->mv_sold_to_party = ip_sold_to_party.

    endif.

  endmethod.

  method yif_custom_crm_order_init~set_crm_category1.

    if me->mv_crm_category1 is initial.

      me->mv_crm_category1 = ip_crm_category1.

    endif.

  endmethod.

  method yif_custom_crm_order_init~set_crm_category2.

    if me->mv_crm_category2 is initial.

      me->mv_crm_category2 = ip_crm_category2.

    endif.

  endmethod.

  method yif_custom_crm_order_init~set_crm_category3.

    if me->mv_crm_category3 is initial.

      me->mv_crm_category3 = ip_crm_category3.

    endif.

  endmethod.

  method yif_custom_crm_order_init~set_crm_category4.

    if me->mv_crm_category4 is initial.

      me->mv_crm_category4 = ip_crm_category4.

    endif.

  endmethod.

  method yif_custom_crm_order_init~set_crm_cat_schema.

    if me->mv_crm_cat_schema is initial.

      me->mv_crm_cat_schema = ip_crm_cat_schema.

    endif.

  endmethod.

  method get_texts.

    " Copied from standard cl_ai_crm_gw_text=>get_text

    data:
      lo_cl_ags_crm_1o_api type ref to cl_ags_crm_1o_api,

      lt_text_gen          type crmt_text_gen_ext_tab,
      lt_text_mapping      type tihttpnvp,
      lt_texts             type comt_text_textdata_t,
      ls_orderadm_h_wrk    type crmt_orderadm_h_wrk,
      lt_text_cust         type comt_text_cust_struc1_tab,
      lv_guid              type crmt_object_guid,
      ls_text              type cl_ai_crm_gw_mymessage_mpc=>ts_text.

    call method cl_ags_crm_1o_api=>get_instance
      exporting
        iv_header_guid                = ip_guid
        iv_process_mode               = 'C'
        iv_process_type               = mv_process_type
      importing
        eo_instance                   = lo_cl_ags_crm_1o_api
      exceptions
        invalid_parameter_combination = 1
        error_occurred                = 2
        others                        = 3.

    if sy-subrc <> 0.

      raise exception type ycx_crm_order_api_exc
        exporting
          textid  = ycx_crm_order_api_exc=>cant_get_document_by_guid
          ip_guid = ip_guid.

    endif. "  if sy-subrc <> 0


    lo_cl_ags_crm_1o_api->get_texts(
         importing
           et_text_all          =    lt_texts
           et_text_gen          =    lt_text_gen
           et_text_cust         =    lt_text_cust " Table type for CUST_STRUC1 (Sorted by Sequence)
           et_text_mapping      =    lt_text_mapping
         exceptions
           document_locked      = 1
           document_not_found   = 2
           error_occurred       = 3
           no_change_allowed    = 4
           no_change_authority  = 5
           no_display_authority = 6
           others               = 7
       ).
    if sy-subrc <> 0.

      raise exception type ycx_crm_order_api_exc
        exporting
          textid  = ycx_crm_order_api_exc=>cant_get_texts_by_guid
          ip_guid = ip_guid.

    endif.

    call function 'CRM_ORDERADM_H_READ_OB'
      exporting
        iv_guid                       = lv_guid
      importing
        es_orderadm_h_wrk             = ls_orderadm_h_wrk
      exceptions
        parameter_error               = 1
        record_not_found              = 2
        at_least_one_record_not_found = 3
        others                        = 4.
    if sy-subrc <> 0.
      "do not cancel
    endif.

    sort lt_texts by stxh-tdname  descending.

    loop at lt_texts assigning field-symbol(<texts>) where stxh-tdid ne 'SUSD'.

      authority-check object 'CRM_TXT_ID'
       id 'TEXTOBJECT' field <texts>-stxh-tdobject
       id 'TEXTID' field <texts>-stxh-tdid
       id 'ACTVT' field '03'.    "#EC NOTEXT "gc_authorization-display.

      if sy-subrc <> 0.
        continue.
      endif.

      "Check if mapping for HTML enabled Text exists
      read table lt_text_mapping assigning field-symbol(<mapping>) with key name = sy-tabix.
      if sy-subrc = 0.
        data(lv_html_text_no) = <mapping>-value.
      else.
        clear lv_html_text_no.
      endif.

      ls_text = get_text_details_odata(
              is_text           =  <texts>
              it_text_cust      =  lt_text_cust
              is_orderadm_h_wrk =  ls_orderadm_h_wrk
          ).

      if lv_html_text_no is initial.

        data lv_stream_text type string_table.
        data lt_lines type comt_text_lines_t.
        move <texts>-lines to lt_lines.

        call function 'CONVERT_ITF_TO_STREAM_TEXT'
          exporting
            lf           = 'X'
          importing
            stream_lines = lv_stream_text
          tables
            itf_text     = lt_lines.

        loop at lv_stream_text assigning field-symbol(<stream>).
          concatenate ls_text-text <stream> into ls_text-text separated
by '<br>'.
        endloop.

        data(lv_itf_text) = cl_gstext_tools=>transform_itf_to_itfstring( it_itf_text = lt_lines ).

      else.

        read table lt_text_gen index lv_html_text_no assigning field-symbol(<html_text>).
        if sy-subrc = 0.
          ls_text-text = <html_text>-text_content.
        endif.
      endif.

      append ls_text to et_texts.

    endloop.

  endmethod.

  method get_text_details_odata.

    " Copied from standard cl_ai_crm_gw_text=>get_text_details_odata

    data: lv_email_subject               type string,
          lv_object_no                   type string,
          lv_descripton                  type string,
          lv_process_type_short_text_str type string,
          lv_systimezone                 type ad_tzone,
          lv_utc                         type timestamp,
          lv_utc_char                    type c length 14,
          lv_date_char(20)               type c,
          lv_time_char(20)               type c,
          lv_username                    type bapibname-bapibname,
          ls_address                     type bapiaddr3,
          lv_addtel                      type table of  bapiadtel,
          l_country_text                 type t005t-landx,
          t_return                       type table of bapiret2,
          l_street_str                   type string,
          l_postal_code_str              type string,
          l_city_str                     type string,
          l_house_number_str             type string,
          l_counry_text_str              type string.

    move-corresponding is_text-stxh to rs_entity.

    " Get description of process_type

    select single p_description_20 from  crmc_proc_type_t
     into @lv_process_type_short_text_str
     where process_type = @is_orderadm_h_wrk-process_type and
     langu = @sy-langu.

    lv_object_no = is_orderadm_h_wrk-object_id.
    lv_descripton = is_orderadm_h_wrk-description.

    concatenate lv_process_type_short_text_str lv_object_no into lv_email_subject separated by space.
    concatenate lv_email_subject lv_descripton into lv_email_subject
separated by ':'.

    read table it_text_cust assigning field-symbol(<text_cust>) with key textid = is_text-stxh-tdid.

    if sy-subrc = 0.
      rs_entity-tdid_txt = <text_cust>-ttxit_text.
    endif.

    rs_entity-user_text = cl_ags_crm_bp_util=>get_bp_full_name_by_user(
                      iv_syuname       = is_text-stxh-tdfuser
                      iv_with_bp_no    = abap_false
                  ).


    lv_username = is_text-stxh-tdfuser.

    call function 'BAPI_USER_GET_DETAIL'
      exporting
        username = lv_username
      importing
        address  = ls_address
      tables
        return   = t_return
        addtel   = lv_addtel.


    rs_entity-email =  ls_address-e_mail.
    rs_entity-emailsubject  = lv_email_subject.
    rs_entity-department =  ls_address-department.

    read table  lv_addtel assigning field-symbol(<ls_addtel>) with key r_3_user = '1'. " Default LandLine
    if sy-subrc = 0.
      rs_entity-contactphone = <ls_addtel>-tel_no.
    endif.

    read table  lv_addtel assigning  <ls_addtel> with key r_3_user = '3'. " Default Mobile
    if sy-subrc = 0.
      rs_entity-contactmobile = <ls_addtel>-tel_no.
    endif.


    " Get Address

    rs_entity-company =  ls_address-name.
    l_street_str =  ls_address-street.
    l_house_number_str =  ls_address-house_no.
    l_postal_code_str =  ls_address-postl_cod1.
    l_city_str =  ls_address-city.

    cl_ags_work_bp_info=>get_bp_country(
      exporting
        tland1 =    ls_address-country
      importing
       tlandx = l_country_text ).

    l_counry_text_str = l_country_text.

    if l_street_str is not initial.
      concatenate l_street_str l_house_number_str into data(l_street_w_no) separated by space.
    endif.

    if l_city_str is not initial.
      concatenate l_postal_code_str l_city_str into data(l_city_w_code) separated by space.
    endif.

    if l_street_w_no is not initial and l_city_w_code  is not initial and l_counry_text_str is not initial.
      concatenate l_street_w_no l_city_w_code l_counry_text_str into data(l_address_str) separated by ','.
    endif.


    rs_entity-company_adress = l_address_str.

    " Get System Timezone

    call function 'GET_SYSTEM_TIMEZONE'
      importing
        timezone            = lv_systimezone
      exceptions
        customizing_missing = 1
        others              = 2.
    if sy-subrc <> 0.
      lv_systimezone = sy-zonlo.
    endif.

    " Make conversion to UTC since TDLDATE / TDLTIME is not UTC
    convert date is_text-stxh-tdfdate time is_text-stxh-tdftime
    into time stamp lv_utc time zone lv_systimezone.

    " Pass the UTC time to fiori front end, Fiori should display as Client time according to this UTC time
    rs_entity-date_time_text = lv_utc.

    move is_orderadm_h_wrk-guid to rs_entity-ref_guid.

  endmethod.

  method yif_custom_crm_order_read~get_all_texts.

    " Copied from standard cl_ai_crm_gw_text=>textset_get_entityset

    call method get_texts
      exporting
        ip_guid  = ip_guid
      importing
        et_texts = et_texts.

    sort et_texts by tdfdate descending tdftime descending.


  endmethod.

  method yif_custom_crm_order_read~get_attachment_by_keys.

    data: ls_attachment type  aic_s_attachment_incdnt_odata,
          lt_docs       type ict_crm_documents,
          lt_urls       type ict_crm_urls,
          ls_loio       type skwf_io,
          ls_phio       type skwf_io.

    field-symbols <fs_link> type obl_s_link.

    ls_loio = value skwf_io( objid = ip_loio class = 'CRM_L_ORD' objtype
= 'L' ).
    ls_phio = value skwf_io( objid = ip_phio class = 'CRM_P_ORD' objtype
= 'P' ).

    call function 'ICT_READ_ATTACHMENTS'
      exporting
        i_crm_id     = ip_guid
      importing
        e_t_document = lt_docs
        e_t_url      = lt_urls.

    read table lt_docs assigning field-symbol(<doc>) with key phio =
ls_phio loio = ls_loio.
    if sy-subrc = 0.
      ls_attachment = cl_ai_crm_gw_attachment=>get_attachment_details_odata( is_abap_doc = <doc> iv_guid = ip_guid ).
    endif.

    er_attachment = ls_attachment.

  endmethod.

  method yif_custom_crm_order_read~get_attachment_content_by_keys.

    data:ls_attachment type aic_s_attachment_incdnt_odata.

    try.
        ls_attachment = me->yif_custom_crm_order_read~get_attachment_by_keys(
            ip_guid = ip_guid
            ip_loio = ip_loio ip_phio = ip_phio ).

        er_stream = value /iwbep/if_mgw_appl_types=>ty_s_media_resource(
        value = ls_attachment-document mime_type = ls_attachment-mimetype  ).

        er_attachment = ls_attachment.

      catch cx_root.
        "No attachment found --> no problem...
    endtry.

  endmethod.

  method yif_custom_crm_order_create~create_attachment.

    data:
       lo_cl_ags_crm_1o_api type ref to cl_ags_crm_1o_api.


    call method cl_ags_crm_1o_api=>get_instance
      exporting
        iv_header_guid                = ip_guid
        iv_process_mode               = 'C'
        iv_process_type               = mv_process_type
      importing
        eo_instance                   = lo_cl_ags_crm_1o_api
      exceptions
        invalid_parameter_combination = 1
        error_occurred                = 2
        others                        = 3.

    if sy-subrc <> 0.

      raise exception type ycx_crm_order_api_exc
        exporting
          textid  = ycx_crm_order_api_exc=>cant_get_document_by_guid
          ip_guid = ip_guid.

    endif. "  if sy-subrc <> 0

    lo_cl_ags_crm_1o_api->add_attachment(
    exporting
      iv_doc_content  = ip_content
      iv_file_name    = ip_file_name
      iv_content_type = ip_mime_type
    importing
      es_loio         = es_loio
      es_phio         = es_phio
      es_error        = data(ls_error)
  ).

  endmethod.

  method yif_custom_crm_order_update~delete_attachment.

    data: ls_phio          type skwf_io,
          ls_bo            type         sibflporb,
          ls_err_msg       type skwf_error,
          lt_io            type skwf_ios,
          lv_error_message type string.


    clear ls_phio.
    ls_phio-objtype = 'P'.                                  "#EC NOTEXT
    ls_phio-class = 'CRM_P_ORD'.                            "#EC NOTEXT
    ls_phio-objid = ip_phio.
    append ls_phio to lt_io.

    ls_bo-instid = ip_guid.
    ls_bo-typeid = 'BUS2000223'.                            "#EC NOTEXT
    ls_bo-catid  = 'BO'.                                    "#EC NOTEXT

    cl_crm_documents=>delete(
      exporting business_object = ls_bo
        ios = lt_io
      importing error = ls_err_msg  ).

    if ls_err_msg is not initial.

      lv_error_message = |{ ls_err_msg-v1 }| && | | && |{ ls_err_msg-v2 }| && | | && |{ ls_err_msg-v3 }| && | | &&  |{ ls_err_msg-v4 }|.

      raise exception type ycx_crm_order_api_exc
        exporting
          textid           = ycx_crm_order_api_exc=>internal_error
          ip_error_message = lv_error_message.

    else.

      commit work.

    endif.

  endmethod.

  method yif_custom_crm_order_create~create_text.

    " Copied from standard CL_AI_CRM_GW_TEXT->TEXTSET_CREATE_ENTITY_ACTION

    data:
      lo_cl_ags_crm_1o_api type ref to cl_ags_crm_1o_api,
      lt_tline             type comt_text_lines_t,
      lv_text              type string,
      ls_text_com          type crmt_text_com,
      lt_text_com          type crmt_text_comt.


    call method cl_ags_crm_1o_api=>get_instance
      exporting
        iv_header_guid                = ip_guid
        iv_process_mode               = 'B'
        iv_process_type               = mv_process_type
      importing
        eo_instance                   = lo_cl_ags_crm_1o_api
      exceptions
        invalid_parameter_combination = 1
        error_occurred                = 2
        others                        = 3.

    if sy-subrc <> 0.

      raise exception type ycx_crm_order_api_exc
        exporting
          textid  = ycx_crm_order_api_exc=>cant_get_document_by_guid
          ip_guid = ip_guid.

    endif. "  if sy-subrc <> 0

    lv_text = cl_bspwd_util=>replace_crlf_with_lf( iv_original_string = ip_text ).

    cl_wd_ags_inci_trans_helper=>get_formatted_text_list(
       exporting
         iv_string        = lv_text
       importing
         et_crm_text_list =   lt_tline ).

    clear ls_text_com.

    ls_text_com-ref_guid    = lo_cl_ags_crm_1o_api->av_header_guid.
    ls_text_com-ref_kind    = 'A'.
    ls_text_com-text_object = 'CRM_ORDERH'.
    ls_text_com-tdid        = ip_tdid.
    ls_text_com-tdspras     = sy-langu.
    ls_text_com-tdstyle     = 'SYSTEM'.
    ls_text_com-tdform      = 'SYSTEM'.
    ls_text_com-mode        = 'I'.
    ls_text_com-lines       = lt_tline.

    insert ls_text_com into table lt_text_com.

    lo_cl_ags_crm_1o_api->set_texts(
      exporting
        it_text           =  lt_text_com
      exceptions
        error_occurred    = 1
        document_locked   = 2
        no_change_allowed = 3
        no_authority      = 4
        others            = 5 ).

    data   lo_log_handle type balloghndl  .


    lo_cl_ags_crm_1o_api->save(
      changing
        cv_log_handle = lo_log_handle
      exceptions
        others = 1 ).

    if ( sy-subrc <> 0 ).


      raise exception type ycx_crm_order_api_exc
        exporting
          textid = ycx_crm_order_api_exc=>error_saving_document.

    endif.

  endmethod.

  method yif_custom_crm_order_read~get_last_text.

    data lt_text_cust type comt_text_cust_struc1_tab.
    data lt_texts type comt_text_textdata_t.
    data ls_orderadm_h_wrk type crmt_orderadm_h_wrk.

    call function 'CRM_DNO_READ_ORDER_TEXT'
      exporting
        iv_header_guid = ip_guid
      importing
        et_alltexts    = lt_texts
        et_text_cust   = lt_text_cust.


    call function 'CRM_ORDERADM_H_READ_OB'
      exporting
        iv_guid                       = ip_guid
      importing
        es_orderadm_h_wrk             = ls_orderadm_h_wrk
      exceptions
        parameter_error               = 1
        record_not_found              = 2
        at_least_one_record_not_found = 3
        others                        = 4.
    if sy-subrc <> 0.
      "do not cancel
    endif.

    sort lt_texts by stxh-tdname  descending.

    read table lt_texts assigning field-symbol(<text>) index 1.


    if <text> is assigned.

      es_text = get_text_details_odata(
                is_text           = <text>
                it_text_cust      = lt_text_cust
                is_orderadm_h_wrk = ls_orderadm_h_wrk
            ).

    endif.


  endmethod.


  method get_category_and_aspect_guids.

    select single asp_guid cat_guid from crmv_erms_cat_ca
    into (ep_asp_guid, ep_cat_guid)
    where asp_id = ip_asp_id
    and cat_id = ip_cat_id
    and asp_state = 'V'
    and lang = sy-langu.

  endmethod.

  method get_category_tree.

    call method cl_crm_ml_category_util=>get_selected_category_tree
      exporting
        iv_selected_cat_guid = ip_cat_guid
        iv_schema_guid       = ip_asp_guid
      importing
        et_cat_tree          = rt_category_tree.
  endmethod.

  method yif_custom_crm_order_organizer~is_order_matching_to_filters.

    data: lt_filter_sel_opt    type /iwbep/t_cod_select_options,
          ls_filter_sel_opt    like line of lt_filter_sel_opt,
          ls_bp_filter_sel_opt like line of lt_filter_sel_opt,
          lo_structure_ref     type ref to data,
          lo_descr_ref         type ref to cl_abap_typedescr,
          lv_bp_number         type bu_partner,
          lo_bp_master_data    type ref to yif_bp_master_data.

    field-symbols: <fs_structure> type any,
                   <fs_value>     type any.

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Decode incoming abstract entity
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^




    create data lo_structure_ref type standard table of (mv_structure_name).
    assign lo_structure_ref->* to <fs_structure>.

    if ( ir_entity is bound ).

      assign ir_entity->* to <fs_structure>.

    endif. " if ( ir_entity is bound )

    loop at it_set_filters assigning field-symbol(<ls_set_filters>).

      assign component <ls_set_filters>-property of structure <fs_structure> to <fs_value>.

      if ( sy-subrc eq 0 ).

        clear: lt_filter_sel_opt,
        ls_filter_sel_opt.

        lo_descr_ref = cl_abap_typedescr=>describe_by_data( <fs_value> ).

        " Special cases for various types

        case lo_descr_ref->absolute_name.

          when '\TYPE=BU_PARTNER'.

            " If we have Business Partners for filtering, we need to add leading zeroes
            " to LOW and HIGH values of filter
            " In addition we have to add leading zeroes for structure value to keep filter and value
            " in a same format of BU_PARTNER with leading zeroes

            clear: ls_filter_sel_opt, lv_bp_number, lo_bp_master_data, ls_bp_filter_sel_opt.

            loop at <ls_set_filters>-select_options assigning field-symbol(<sel_option>).

              ls_filter_sel_opt =  <sel_option>.

              ls_bp_filter_sel_opt-option = ls_filter_sel_opt-option.
              ls_bp_filter_sel_opt-sign = ls_filter_sel_opt-sign.

              if ls_filter_sel_opt-low is not initial.
                lv_bp_number = ls_filter_sel_opt-low.
                lo_bp_master_data  = new ycl_bp_master_data( lv_bp_number ).
                ls_bp_filter_sel_opt-low = lo_bp_master_data->get_bp_number( ).

              endif.

              clear: lv_bp_number, lo_bp_master_data.

              if ls_filter_sel_opt-high is not initial.

                lv_bp_number = ls_filter_sel_opt-high.
                lo_bp_master_data  = new ycl_bp_master_data( lv_bp_number ).
                ls_bp_filter_sel_opt-high = lo_bp_master_data->get_bp_number( ).

              endif.

              append ls_bp_filter_sel_opt to lt_filter_sel_opt.

            endloop.

            lv_bp_number = <fs_value>.
            lo_bp_master_data  = new ycl_bp_master_data( lv_bp_number ).
            lv_bp_number = lo_bp_master_data->get_bp_number( ).

            if ( lt_filter_sel_opt is not initial ) and (  lv_bp_number not in lt_filter_sel_opt ).

              cp_include_record = abap_false.
              exit.

            endif.

          when others.

            lt_filter_sel_opt = <ls_set_filters>-select_options.

            if ( lt_filter_sel_opt is not initial ) and ( <fs_value> not in lt_filter_sel_opt ).

              cp_include_record = abap_false.
              exit.

            endif.

        endcase.




      endif. " if ( sy-subrc eq 0 ) and ( <fs_value> is not initial )

    endloop. " loop at it_set_filters ASSIGNING FIELD-SYMBOL(<ls_set_filters>)

  endmethod.

  method yif_custom_crm_order_organizer~sort_orders.

    data: lo_table_ref type ref to data,
          lt_order_tab type abap_sortorder_tab,
          ls_order_tab type abap_sortorder.

    field-symbols:
      <fs_table> type any table,
      <fs_value> type any.

    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    "   Decode incoming abstract entity
    "~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~^~~^~^~^~^~^~^~^~^
    create data lo_table_ref type standard table of (mv_structure_name).
    assign lo_table_ref->* to <fs_table>.

    if ( ir_entity is bound ).

      assign ir_entity->* to <fs_table>.

    endif. " if ( ir_entity is bound )

    loop at it_order assigning field-symbol(<ls_order>).

      if <ls_order>-order = 'desc'.
        ls_order_tab-descending = abap_true.
      else.
        ls_order_tab-descending = abap_false.
      endif.


      search <ls_order>-property for 'Date' .
      if sy-subrc <> 0.
        ls_order_tab-astext = abap_true.
      endif.

      ls_order_tab-name = <ls_order>-property.

      translate ls_order_tab-name to upper case.

      append ls_order_tab to lt_order_tab.

      clear:
        ls_order_tab.

    endloop. " loop at it_order assigning field-symbol(<ls_order>)

    try.
        sort <fs_table> by (lt_order_tab).
      catch cx_sy_dyn_table_ill_comp_val.
    endtry.

    get reference of <fs_table> into er_entity.

  endmethod.

  method yif_custom_crm_order_read~get_all_appointments_by_guid.

    data:
       lo_cl_ags_crm_1o_api     type ref to cl_ags_crm_1o_api.


    call method cl_ags_crm_1o_api=>get_instance
      exporting
        iv_header_guid                = ip_guid
        iv_process_mode               = 'C'
        iv_process_type               = mv_process_type
      importing
        eo_instance                   = lo_cl_ags_crm_1o_api
      exceptions
        invalid_parameter_combination = 1
        error_occurred                = 2
        others                        = 3.

    lo_cl_ags_crm_1o_api->get_appointments(
        importing
            et_appointment = rt_appointments ).

  endmethod.

  method yif_custom_crm_order_update~create_text.

    me->yif_custom_crm_order_create~create_text( exporting
        ip_guid = ip_guid
        ip_tdid = ip_tdid
        ip_text = ip_text ).

  endmethod.

  method yif_custom_crm_order_read~get_sla_status_by_guid.


    data:
       lo_cl_ags_crm_1o_api     type ref to cl_ags_crm_1o_api.


    call method cl_ags_crm_1o_api=>get_instance
      exporting
        iv_header_guid                = ip_guid
        iv_process_mode               = 'C'
        iv_process_type               = mv_process_type
      importing
        eo_instance                   = lo_cl_ags_crm_1o_api
      exceptions
        invalid_parameter_combination = 1
        error_occurred                = 2
        others                        = 3.

    rs_sla_status = lo_cl_ags_crm_1o_api->get_sla_status(  ).


  endmethod.

endclass.
