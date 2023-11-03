class ycl_crm_order_auto_stat_setter definition
  public
  final
  create public .

  public section.

    interfaces yif_crm_order_auto_stat_setter .

    methods:
      constructor.

  protected section.

  private section.

    types: begin of ty_changed_orders,
             guid      type crmt_object_guid,
             statusin  type crm_j_status,
             statusout type crm_j_status,
           end of ty_changed_orders.


    types: tt_changed_orders type table of ty_changed_orders.

    data:
      mt_crmo_auto_stat_settings type table of ycrmo_auto_stat.

    methods:

      set_auto_stat_settings,

      is_time_to_set_status
        importing
          ip_last_status_date          type cddatum
          ip_last_status_time          type cduzeit
          ip_thresholdinhours          type integer
        returning
          value(rp_time_to_set_status) type abap_bool,

      set_order_status
        importing
          ip_guid                  type crmt_object_guid
          ip_status                type j_estat
          io_custom_crm_order_init type ref to yif_custom_crm_order_init
        raising
          ycx_crm_order_api_exc,

      add_order_text
        importing
          ip_tdid                  type tdid
          ip_text_name             type tdobname
          ip_guid                  type crmt_object_guid
          io_custom_crm_order_init type ref to yif_custom_crm_order_init
        raising
          ycx_crm_order_api_exc,

      get_compiled_text
        importing
          ip_use_tags             type abap_bool optional
          ip_text_name            type tdobname
        returning
          value(rp_compiled_text) type string,

      print_results
        importing
          it_changed_orders type tt_changed_orders.

endclass.



class ycl_crm_order_auto_stat_setter implementation.

  method yif_crm_order_auto_stat_setter~process_orders_status_setting.

    constants lc_new_line  type c length 2
             value cl_abap_char_utilities=>cr_lf.

    types: begin of ty_crm_stat_hist,
             stat  type crm_j_status,
             udate type cddatum,
             utime type  cduzeit,
           end of ty_crm_stat_hist.

    data:
      lo_crm_order_init type ref to yif_custom_crm_order_init,
      lo_crm_order_read type ref to yif_custom_crm_order_read,
      lt_guids          type ycrm_order_tt_guids,
      lt_crm_stat_hist  type table of ty_crm_stat_hist,
      lv_max_date       type cddatum,
      lv_max_time       type cduzeit,
      lt_changed_orders type tt_changed_orders,
      wa_changed_order  type ty_changed_orders.


    loop at mt_crmo_auto_stat_settings assigning field-symbol(<fs_crmo_auto_stat_setting>).

      " Looping through CRM orders

      lo_crm_order_init = new ycl_custom_crm_order_api(  ).

      lo_crm_order_init->set_process_type( <fs_crmo_auto_stat_setting>-processtype ).

      lo_crm_order_init->set_structure_name( ).

      lo_crm_order_read ?= lo_crm_order_init.

      lt_guids = lo_crm_order_read->get_guids_list(  ).

      loop at lt_guids assigning field-symbol(<ls_guid>).

        clear: lt_crm_stat_hist, lv_max_date, lv_max_time.

        select stat udate utime
            from crm_jcds
               into table lt_crm_stat_hist
                where objnr eq <ls_guid>-guid and
                 inact ne 'X'.

        if lt_crm_stat_hist is not initial.

          try.

              " Getting the latest date and time of CRM order status change

              sort lt_crm_stat_hist by udate descending utime descending.

              lv_max_date = lt_crm_stat_hist[ 1 ]-udate.
              lv_max_time = lt_crm_stat_hist[ 1 ]-utime.

              loop at lt_crm_stat_hist assigning field-symbol(<fs_crm_stat_hist>)
                where stat eq <fs_crmo_auto_stat_setting>-statusin and
                    udate eq lv_max_date and
                        utime eq lv_max_time.

                " Comparison of the dates

                if ( is_time_to_set_status( ip_last_status_date = lv_max_date
                    ip_last_status_time = lv_max_time ip_thresholdinhours = <fs_crmo_auto_stat_setting>-stayonstatusinhours ) eq abap_true ).

                  " If last status change contains a user status from automatic setup table,
                  " then change a status accordingly

                  " Addition of a text (if required)

                  if <fs_crmo_auto_stat_setting>-sttext is not initial.

                    me->add_order_text(
                        ip_guid = <ls_guid>-guid
                        io_custom_crm_order_init = lo_crm_order_init
                        ip_text_name = <fs_crmo_auto_stat_setting>-sttext
                        ip_tdid = <fs_crmo_auto_stat_setting>-texttype ).

                  endif.

                  me->set_order_status(
                      exporting
                          ip_guid = <ls_guid>-guid
                          ip_status = <fs_crmo_auto_stat_setting>-statusout
                          io_custom_crm_order_init = lo_crm_order_init
                  ).


                  wa_changed_order-guid = <ls_guid>-guid.
                  wa_changed_order-statusout =  <fs_crmo_auto_stat_setting>-statusout.
                  wa_changed_order-statusin =  <fs_crmo_auto_stat_setting>-statusin.

                  append wa_changed_order to lt_changed_orders.


                endif.

              endloop.

            catch cx_sy_itab_line_not_found.

          endtry.

        endif.

      endloop.

    endloop.

    me->print_results( lt_changed_orders ).

  endmethod.

  method set_auto_stat_settings.

    select mandt processtype statusin statusout
        stayonstatusinhours sttext texttype
            from ycrmo_auto_stat
                into table mt_crmo_auto_stat_settings.

  endmethod.

  method constructor.

    me->set_auto_stat_settings( ).

  endmethod.

  method set_order_status.

    data:
      lo_custom_crm_order_update type ref to yif_custom_crm_order_update,
      ls_payload                 type ycrm_order_ts,
      lr_payload                 type ref to data.

    lo_custom_crm_order_update ?= io_custom_crm_order_init.

    ls_payload-status = ip_status.

    get reference of ls_payload into lr_payload.

    lo_custom_crm_order_update->update_order(
     exporting
      ir_entity = lr_payload
      ip_guid = ip_guid ).


  endmethod.

  method is_time_to_set_status.

    data: lv_last_status_timestamp     type timestamp,
          lv_current_timestamp         type timestamp,
          lv_system_timezone           type timezone,
          lv_seconds_since_last_status type integer,
          lv_hours_since_last_status   type integer.

    lv_system_timezone =  ycl_assistant_utilities=>get_system_timezone(  ).

    convert date ip_last_status_date time ip_last_status_time
         into time stamp lv_last_status_timestamp time zone lv_system_timezone.

    get time stamp field lv_current_timestamp.

    lv_seconds_since_last_status = ycl_assistant_utilities=>calc_duration_btw_timestamps(
        exporting
            ip_timestamp_1 = lv_current_timestamp
            ip_timestamp_2 = lv_last_status_timestamp

    ).

    lv_hours_since_last_status = lv_seconds_since_last_status div 3600.

    if lv_hours_since_last_status > ip_thresholdinhours.

      rp_time_to_set_status = abap_true.

    endif.

  endmethod.

  method add_order_text.

    data:
      lo_custom_crm_order_create type ref to yif_custom_crm_order_create,
      lv_text                    type string.

    lv_text = me->get_compiled_text(
        exporting
            ip_text_name = ip_text_name ).

    lo_custom_crm_order_create ?= io_custom_crm_order_init.

    lo_custom_crm_order_create->create_text( ip_guid = ip_guid ip_tdid = ip_tdid ip_text = lv_text ).


  endmethod.

  method get_compiled_text.

    data: lo_standard_text    type ref to yif_standard_text,
          lt_variables_values type yst_text_tt_variables_values.

    lo_standard_text = new ycl_standart_text(
    ip_text_language = 'R'
    ip_text_name = ip_text_name ).

    rp_compiled_text = lo_standard_text->get_compiled_text_by_name(
        exporting
            ip_use_tags = ip_use_tags
            it_variables_values =  lt_variables_values ).

  endmethod.

  method print_results.

    data : lv_result_text_string type string,
           lv_total_counter      type integer.


    loop at it_changed_orders assigning field-symbol(<fs_changed_order>).

      lv_total_counter = lv_total_counter + 1.

      write:  |Статус документа | && |{ <fs_changed_order>-guid }| && | изменен с | &&
             |{ <fs_changed_order>-statusin }| && | на | & |{ <fs_changed_order>-statusout }|.

      new-line.

    endloop.

    write: |Всего изменено | && |{ lv_total_counter }| && | документов(а).|.

  endmethod.
endclass.
