class ycl_slpm_problem_history_store definition
  public
  final
  create public .

  public section.

    interfaces:

      yif_slpm_problem_history_store,

      yif_slpm_problem_observer.

    methods:
      constructor
        importing
          ip_guid type crmt_object_guid.

  protected section.
  private section.
    data:
      mv_guid              type crmt_object_guid,
      mv_event             type char1,
      ms_yslpm_pr_his_hdr  type yslpm_pr_his_hdr,
      ms_problem           type ycrm_order_ts_sl_problem,
      mv_file_name         type string,
      mv_change_guid       type sysuuid_x16,
      mo_password          type string value 'veYlJeW&C6',
      mt_fields_to_skip    type table of abap_compname,
      mt_translation_table type table of yslpm_pr_fld_trs.

    methods:

      add_creation_event_record
        importing
          is_problem type ycrm_order_ts_sl_problem
        raising
          ycx_assistant_utilities_exc,

      add_update_event_record
        importing
          is_problem type ycrm_order_ts_sl_problem
        raising
          ycx_assistant_utilities_exc,

      add_event_record
        importing
          is_problem type ycrm_order_ts_sl_problem
        raising
          ycx_assistant_utilities_exc,

      add_hist_header_record_to_db,

      set_hist_header_record,

      add_hist_detail_record_to_db
        raising
          ycx_assistant_utilities_exc,

      set_problem_record
        importing
          is_problem type ycrm_order_ts_sl_problem,

      add_att_upload_event_record
        importing
          ip_file_name type string,

      add_att_remove_event_record
        importing
          ip_file_name type string,

      add_att_event_record
        importing
          ip_file_name type string,

      set_file_name
        importing
          ip_file_name type string,

      add_hist_att_record_to_db,

      fill_fields_to_skip,

      translate_field_name
        importing
          ip_field_name         type char50
        returning
          value(rp_translation) type char50,

      set_translation_table.

endclass.


class ycl_slpm_problem_history_store implementation.

  method constructor.

    mv_guid = ip_guid.

    me->fill_fields_to_skip( ).

  endmethod.


  method add_creation_event_record.

    mv_event = 'C'.

    if is_problem is not initial.

      me->add_event_record( is_problem ).

    endif.

  endmethod.

  method add_update_event_record.

    mv_event = 'U'.

    if is_problem is not initial.

      me->add_event_record( is_problem ).

    endif.

  endmethod.

  method set_hist_header_record.

    mv_change_guid = ycl_assistant_utilities=>generate_x16_guid(  ).

    ms_yslpm_pr_his_hdr-guid = mv_guid.
    ms_yslpm_pr_his_hdr-username = sy-uname.
    ms_yslpm_pr_his_hdr-change_date = sy-datum.
    ms_yslpm_pr_his_hdr-change_time = sy-uzeit.
    ms_yslpm_pr_his_hdr-event = mv_event.
    ms_yslpm_pr_his_hdr-change_guid = mv_change_guid.

  endmethod.

  method add_hist_header_record_to_db.

    insert  yslpm_pr_his_hdr from ms_yslpm_pr_his_hdr.

  endmethod.

  method set_problem_record.

    ms_problem = is_problem.

  endmethod.

  method add_event_record.


    me->set_problem_record( is_problem ).

    me->set_hist_header_record(  ).

    me->add_hist_header_record_to_db(  ).

    me->add_hist_detail_record_to_db(  ).

  endmethod.



  method add_hist_detail_record_to_db.

    data:
      wa_yslpm_pr_his_rec    type yslpm_pr_his_rec,
      lt_problem             type table of ycrm_order_ts_sl_problem,
      lo_descr               type ref to cl_abap_tabledescr,
      lo_type                type ref to cl_abap_datadescr,
      lo_struct              type ref to cl_abap_structdescr,
      lt_components          type  abap_compdescr_tab,
      lo_descr_ref           type ref to cl_abap_typedescr,
      lv_adapted_field_value type string.

    field-symbols : <lv_value> type any.

    append ms_problem to lt_problem.

    lo_descr ?= cl_abap_typedescr=>describe_by_data( lt_problem ).
    lo_type = lo_descr->get_table_line_type( ).
    lo_struct ?= cl_abap_typedescr=>describe_by_name( lo_type->absolute_name ).

    " The approach below is used, as it
    " also picks all structure includes up

    lt_components =  lo_struct->components.

    loop at lt_components assigning field-symbol(<ls_component>).

      clear lv_adapted_field_value.

      if <lv_value> is assigned.
        unassign <lv_value>.
      endif.

      if <ls_component>-name is not initial.

        if line_exists(  mt_fields_to_skip[ table_line = <ls_component>-name ] ).

          continue.

        endif.

        assign component <ls_component>-name of structure ms_problem to <lv_value>.

        if sy-subrc = 0.

          if ( <lv_value> is assigned ) and ( <lv_value> is not initial ).

            lo_descr_ref = cl_abap_typedescr=>describe_by_data( <lv_value> ).

            if ( lo_descr_ref->absolute_name eq '\TYPE=COMT_CREATED_AT_USR' ) or
                ( lo_descr_ref->absolute_name eq '\TYPE=CRMT_DATE_TIMESTAMP_FROM' ) .

              lv_adapted_field_value = ycl_assistant_utilities=>format_timestamp( <lv_value> ).

            else.

              lv_adapted_field_value = <lv_value>.

            endif.

            clear wa_yslpm_pr_his_rec.

            wa_yslpm_pr_his_rec-change_guid = mv_change_guid.
            wa_yslpm_pr_his_rec-field = <ls_component>-name.
            wa_yslpm_pr_his_rec-value = lv_adapted_field_value.

            insert yslpm_pr_his_rec from wa_yslpm_pr_his_rec.

          endif.

        endif.

      endif.

    endloop.

  endmethod.

  method yif_slpm_problem_history_store~get_problem_history_headers.

    select
         change_guid
         guid
         username
         change_date
         change_time
         event
      into corresponding fields of table rt_yslpm_pr_his_hdr
         from yslpm_pr_his_hdr
         where guid = mv_guid.


  endmethod.

  method yif_slpm_problem_history_store~get_problem_history_records.

    data: lt_yslpm_pr_his_hdr type yslpm_tt_pr_his_hdr,
          lt_yslpm_pr_his_rec type yslpm_tt_pr_his_rec.

    select
       change_guid
       guid
       username
       change_date
       change_time
       event
    into corresponding fields of table lt_yslpm_pr_his_hdr
       from yslpm_pr_his_hdr
       where guid = mv_guid.

    loop at lt_yslpm_pr_his_hdr assigning field-symbol(<ls_yslpm_pr_his_hdr>).

      clear lt_yslpm_pr_his_rec.

      select
        change_guid
        field
        value
     into corresponding fields of table lt_yslpm_pr_his_rec
        from yslpm_pr_his_rec
        where change_guid = <ls_yslpm_pr_his_hdr>-change_guid.

      append lines of lt_yslpm_pr_his_rec to rt_yslpm_pr_his_rec.


    endloop.

  endmethod.

  method yif_slpm_problem_history_store~get_problem_history_hierarchy.

    data: lt_yslpm_pr_his_hdr type yslpm_tt_pr_his_hdr,
          lt_yslpm_pr_his_rec type yslpm_tt_pr_his_rec,
          ls_yslpm_pr_his_hry type yslpm_ts_pr_his_hry,
          lv_nodeid_counter   type int4 value 1,
          lv_parent_nodeid    type int4.

    if mt_translation_table is initial.

      me->set_translation_table( ).

    endif.

    select
       change_guid
       guid
       username
       change_date
       change_time
       event
    into corresponding fields of table lt_yslpm_pr_his_hdr
       from yslpm_pr_his_hdr
       where guid = mv_guid.


    loop at lt_yslpm_pr_his_hdr assigning field-symbol(<ls_yslpm_pr_his_hdr>).

      clear ls_yslpm_pr_his_hry.

      ls_yslpm_pr_his_hry-nodeid = lv_nodeid_counter.
      ls_yslpm_pr_his_hry-hierarchylevel = 0.
      ls_yslpm_pr_his_hry-description = 'asd'.
      ls_yslpm_pr_his_hry-drillstate = 'expanded'.

      ls_yslpm_pr_his_hry-guid = <ls_yslpm_pr_his_hdr>-guid.
      ls_yslpm_pr_his_hry-username = <ls_yslpm_pr_his_hdr>-username.
      ls_yslpm_pr_his_hry-change_date = <ls_yslpm_pr_his_hdr>-change_date.
      ls_yslpm_pr_his_hry-change_time = <ls_yslpm_pr_his_hdr>-change_time.
      ls_yslpm_pr_his_hry-event = <ls_yslpm_pr_his_hdr>-event.
      ls_yslpm_pr_his_hry-change_guid = <ls_yslpm_pr_his_hdr>-change_guid.

      append ls_yslpm_pr_his_hry to rt_yslpm_pr_his_hry.

      lv_parent_nodeid = lv_nodeid_counter.

      lv_nodeid_counter = lv_nodeid_counter + 1.


      clear lt_yslpm_pr_his_rec.

      select
        change_guid
        field
        value
     into corresponding fields of table lt_yslpm_pr_his_rec
        from yslpm_pr_his_rec
        where change_guid = <ls_yslpm_pr_his_hdr>-change_guid.


      loop at lt_yslpm_pr_his_rec assigning field-symbol(<ls_yslpm_pr_his_rec>).

        ls_yslpm_pr_his_hry-nodeid = lv_nodeid_counter.
        ls_yslpm_pr_his_hry-hierarchylevel = 1.
        ls_yslpm_pr_his_hry-description = 'asd'.
        ls_yslpm_pr_his_hry-parentnodeid = lv_parent_nodeid.
        ls_yslpm_pr_his_hry-drillstate = 'leaf'.


        "ls_yslpm_pr_his_hry-field = <ls_yslpm_pr_his_rec>-field.

        ls_yslpm_pr_his_hry-field = translate_field_name( <ls_yslpm_pr_his_rec>-field ).

        ls_yslpm_pr_his_hry-value = <ls_yslpm_pr_his_rec>-value.


        append ls_yslpm_pr_his_hry to rt_yslpm_pr_his_hry.

        lv_nodeid_counter = lv_nodeid_counter + 1.

      endloop.

    endloop.


  endmethod.

  method yif_slpm_problem_observer~problem_created.


    add_creation_event_record( is_problem ).


  endmethod.


  method yif_slpm_problem_observer~problem_updated.

    add_update_event_record( is_problem ).

  endmethod.

  method yif_slpm_problem_observer~attachment_uploaded.

    add_att_upload_event_record( ip_file_name ).

  endmethod.

  method yif_slpm_problem_observer~attachment_removed.

    add_att_remove_event_record( ip_file_name ).

  endmethod.

  method add_att_upload_event_record.

    " Attachment upload event

    mv_event = 'A'.

    me->add_att_event_record( ip_file_name ).

  endmethod.

  method add_att_remove_event_record.

    " Attachment removal event

    mv_event = 'R'.

    me->add_att_event_record( ip_file_name ).

  endmethod.

  method add_att_event_record.

    me->set_file_name( ip_file_name ).

    me->set_hist_header_record(  ).

    me->add_hist_header_record_to_db(  ).

    me->add_hist_att_record_to_db(  ).

  endmethod.

  method set_file_name.

    mv_file_name = ip_file_name.

  endmethod.

  method add_hist_att_record_to_db.

    data:
      wa_yslpm_pr_his_rec type yslpm_pr_his_rec.

    wa_yslpm_pr_his_rec-change_guid = mv_change_guid.
    wa_yslpm_pr_his_rec-field = 'FILENAME'.
    wa_yslpm_pr_his_rec-value = mv_file_name.

    insert yslpm_pr_his_rec from wa_yslpm_pr_his_rec.

  endmethod.

  method yif_slpm_problem_history_store~arch_orphaned_history_records.

    data:
      lo_system_user          type ref to yif_system_user,
      lo_slpm_user            type ref to yif_slpm_user,
      lo_active_configuration type ref to yif_slpm_configuration,
      lo_slpm_problem_api     type ref to ycl_slpm_problem_api,
      lt_problems_guids       type ycrm_order_tt_guids,
      lt_orphaned_guids       type table of crmt_object_guid,
      lt_history_store        type table of yslpm_pr_his_hdr.


    lo_slpm_user = new ycl_slpm_user( sy-uname ).

    lo_system_user ?= lo_slpm_user.

    if lo_slpm_user->is_auth_to_read_problems(  ) eq abap_true.

      lo_active_configuration = new ycl_slpm_configuration(  ).

      lo_slpm_problem_api = new ycl_slpm_problem_api( lo_active_configuration ).

      lt_problems_guids      = lo_slpm_problem_api->yif_custom_crm_order_read~get_guids_list(  ).

      select
        mandt change_guid guid
        username change_date change_time
        event archived
            into table lt_history_store
                from yslpm_pr_his_hdr
                    where archived is null.

      loop at lt_history_store assigning field-symbol(<ls_history_rec>).

        if not line_exists( lt_problems_guids[ guid = <ls_history_rec>-guid ] ).

          update yslpm_pr_his_hdr set archived = 'X'
              where guid = <ls_history_rec>-guid.

        endif.

      endloop.


    else.

      " User has no authorizations to read problems

      raise exception type ycx_slpm_data_manager_exc
        exporting
          textid         = ycx_slpm_data_manager_exc=>not_authorized_for_read
          ip_system_user = sy-uname.

    endif.

  endmethod.

  method yif_slpm_problem_history_store~delete_arch_history_records.

    data lt_archived_his_records type table of sysuuid_x16.

    select
     change_guid
        into table lt_archived_his_records
            from yslpm_pr_his_hdr
                where archived eq 'X'.

    if ip_password eq mo_password. " Just a safety protection for db records deletion

      loop at lt_archived_his_records assigning field-symbol(<ls_archived_his_record>).

        delete from yslpm_pr_his_rec where change_guid eq <ls_archived_his_record>.

        delete from yslpm_pr_his_hdr where change_guid eq <ls_archived_his_record>.

      endloop.

    endif.

  endmethod.

  method fill_fields_to_skip.

    mt_fields_to_skip = value #(

      ( 'IRT_TIMESTAMP_UTC' )
      ( 'MPT_TIMESTAMP_UTC' )
      ( 'TOT_DURA_UNIT' )
      ( 'WORK_DURA_UNIT' )
      ( 'REQUESTERUPDATEENABLED' )
      ( 'REQUESTERWITHDRAWENABLED' )
      ( 'PROCESSORTAKEOVERENABLED' )
      ( 'IRT_ICON_BSP' )
      ( 'ITEM_GUID' )
      ( 'MPT_ICON_BSP' )

  ).


  endmethod.

  method translate_field_name.

    try.

        rp_translation = mt_translation_table[ field_name = ip_field_name ]-translation.

      catch cx_sy_itab_line_not_found.

        rp_translation = ip_field_name.

    endtry.


  endmethod.

  method set_translation_table.

    select mandt field_name spras translation into table mt_translation_table
        from yslpm_pr_fld_trs
        where spras = sy-langu.

  endmethod.



endclass.
