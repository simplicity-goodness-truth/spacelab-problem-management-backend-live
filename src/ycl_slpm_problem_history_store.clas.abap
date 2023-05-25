class YCL_SLPM_PROBLEM_HISTORY_STORE definition
  public
  final
  create public .

  public section.

    interfaces yif_slpm_problem_history_store.

    methods:
      constructor
        importing
          ip_guid type crmt_object_guid.

  protected section.
  private section.
    data:
      mv_guid             type crmt_object_guid,
      mv_event            type char1,
      ms_yslpm_pr_his_hdr type yslpm_pr_his_hdr,
      ms_problem          type ycrm_order_ts_sl_problem,
      mv_change_guid      type sysuuid_x16.

    methods:

      add_event_record
        importing
          is_problem type ycrm_order_ts_sl_problem,

      add_hist_header_record_to_db,

      set_hist_header_record,

      add_hist_detail_record_to_db,

      set_problem_record
        importing
          is_problem type ycrm_order_ts_sl_problem.

endclass.


class YCL_SLPM_PROBLEM_HISTORY_STORE implementation.

  method constructor.

    mv_guid = ip_guid.

  endmethod.


  method yif_slpm_problem_history_store~add_creation_event_record.

    mv_event = 'C'.

    me->add_event_record( is_problem ).

  endmethod.

  method yif_slpm_problem_history_store~add_update_event_record.

    mv_event = 'U'.

    me->add_event_record( is_problem ).

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
      wa_yslpm_pr_his_rec type yslpm_pr_his_rec,
      lt_problem          type table of ycrm_order_ts_sl_problem,
      lo_descr            type ref to cl_abap_tabledescr,
      lo_type             type ref to cl_abap_datadescr,
      lo_struct           type ref to cl_abap_structdescr,
      lt_components       type  abap_compdescr_tab.

    field-symbols : <lv_value> type any.

    append ms_problem to lt_problem.

    lo_descr ?= cl_abap_typedescr=>describe_by_data( lt_problem ).
    lo_type = lo_descr->get_table_line_type( ).
    lo_struct ?= cl_abap_typedescr=>describe_by_name( lo_type->absolute_name ).

    " The approach below is used, as it
    " also picks all structure includes up

    lt_components =  lo_struct->components.

    loop at lt_components assigning field-symbol(<ls_component>).

      if <lv_value> is assigned.
        unassign <lv_value>.
      endif.

      if <ls_component>-name is not initial.

        assign component <ls_component>-name of structure ms_problem to <lv_value>.

        if sy-subrc = 0.

          if ( <lv_value> is assigned ) and ( <lv_value> is not initial ).

            clear wa_yslpm_pr_his_rec.

            wa_yslpm_pr_his_rec-change_guid = mv_change_guid.
            wa_yslpm_pr_his_rec-field = <ls_component>-name.
            wa_yslpm_pr_his_rec-value = <lv_value>.

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


        ls_yslpm_pr_his_hry-field = <ls_yslpm_pr_his_rec>-field.
        ls_yslpm_pr_his_hry-value = <ls_yslpm_pr_his_rec>-value.

        append ls_yslpm_pr_his_hry to rt_yslpm_pr_his_hry.

        lv_nodeid_counter = lv_nodeid_counter + 1.

      endloop.

    endloop.


  endmethod.

endclass.
