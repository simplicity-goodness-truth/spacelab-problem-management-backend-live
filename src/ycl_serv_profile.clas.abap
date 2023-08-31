class YCL_SERV_PROFILE definition
  public
  final
  create public .

  public section.

    methods:

      constructor
        importing
          ip_service_profile type char258.

    interfaces yif_serv_profile.

  protected section.
  private section.

    types: begin of ty_from,
             from type sc_rulewfr,
             to   type sc_rulewfr,
           end of ty_from,

           begin of ty_times_array_tt,
             added_hours_left type sc_rulewfr,
           end of ty_times_array_tt,

           tt_from type table of ty_from.


    data:
      mv_service_profile     type char258,
      mv_ruleid              type sc_ruleid,
      mt_rule_tab            type rule_tab,
      mv_factory_calendar    type wfcid,
      ms_scrule_exp          type scrule_exp,
      mt_idd07v              type standard table of dd07v,
      mt_added_hours_array   type standard table of ty_times_array_tt,
      mv_added_hours_total   type int2,
      mv_added_seconds_total type int4.

    methods:
      set_rule_id,

      set_service_profile
        importing
          ip_service_profile type char258,

      set_rule_tab,

      set_factory_calendar,

      set_rule_details,

      set_domain_vals_for_week_days,

      get_field_name_for_week_day
        importing
          ip_day_of_week_name type char60
        exporting
          ep_from_field_name  type string
          ep_to_field_name    type string,

      is_date_holiday
        importing
          ip_date           type sy-datum
        returning
          value(rp_holiday) type abap_bool,

      get_field_name_for_date
        importing
          ip_date            type sy-datum
        exporting
          ep_from_field_name type string
          ep_to_field_name   type string,

      get_day_number_of_week
        importing
          ip_date                      type sy-datum
        returning
          value(ep_day_of_week_number) type char1,

      get_date_day_name
        importing
          ip_date                    type sy-datum
        returning
          value(rp_day_of_week_name) type char60,

      set_added_hrs_array_from_hrs,

      set_added_hours_total
        importing
          ip_added_hours_total type int2,

      set_added_hrs_array_from_sec,

      set_added_seconds_total
        importing
          ip_added_seconds_total type int4,

      set_list_of_periods
        importing
          ip_date            type sy-datum
          ip_from_field_name type string
          ip_to_field_name   type string
        exporting
          et_all_from_to     type tt_from.


endclass.



class YCL_SERV_PROFILE implementation.

  method constructor.

    me->set_service_profile( ip_service_profile ).

    me->set_rule_id(  ).

    me->set_rule_tab(  ).

    me->set_factory_calendar(  ).

    me->set_rule_details(  ).

    me->set_domain_vals_for_week_days(  ).

  endmethod.

  method set_service_profile.

    data lv_service_profile       type char258.

    lv_service_profile = ip_service_profile.

    condense lv_service_profile.

    mv_service_profile = lv_service_profile.

  endmethod.

  method set_rule_id.

    data lv_crmt_srv_serwi type crmt_srv_serwi.

    " Taking Rule of Service Profile

    lv_crmt_srv_serwi = mv_service_profile.

    call function 'CRM_SERVICE_ENT_GET_RULEID'
      exporting
        iv_serwi  = lv_crmt_srv_serwi
      importing
        ev_ruleid = mv_ruleid.

  endmethod.


  method set_rule_tab.

    call function 'APPT_RULE_READ'
      exporting
        rule_id = mv_ruleid
      importing
        rule    = mt_rule_tab
      exceptions
        no_rule = 1
        others  = 2.

    if sy-subrc <> 0 .
      return.
    endif.

  endmethod.

  method set_factory_calendar.

    loop at mt_rule_tab assigning field-symbol(<ls_rule_tab>).

      if <ls_rule_tab>-fcalid is not initial.
        mv_factory_calendar = <ls_rule_tab>-fcalid.
        exit.
      endif.

    endloop. " loop at mt_rule_tab ASSIGNING FIELD-SYMBOL(<ls_rule_tab>)

  endmethod.


  method set_rule_details.

    " Getting details of a rule

    call function 'APPT_RULE_CONV_FROM_RULE_TAB'
      exporting
        it_rule_tab   = mt_rule_tab
      importing
        s_scrule_exp  = ms_scrule_exp
      exceptions
        nothing_found = 1
        others        = 2.

    if sy-subrc <> 0 .
      exit.
    endif.

  endmethod.

  method set_domain_vals_for_week_days.

    " Preparing domain values for week days

    call function 'DD_DOMVALUES_GET'
      exporting
        domname        = 'RSFO_WEEKDAY'
        text           = 'X'
        langu          = 'E'
      tables
        dd07v_tab      = mt_idd07v
      exceptions
        wrong_textflag = 1
        others         = 2.

    if sy-subrc <> 0 .
      exit.
    endif.

  endmethod.


  method get_field_name_for_week_day.

    case ip_day_of_week_name. " Dynamic approach not possible as field names of RULE_W has strange names

      when 'monday'.
        ep_from_field_name = 'monda_from'.
        ep_to_field_name = 'monday_to'.
      when 'tuesday'.
        ep_from_field_name = 'tuesd_from'.
        ep_to_field_name = 'tuesday_to'.
      when 'wednesday'.
        ep_from_field_name = 'wedne_from'.
        ep_to_field_name = 'wednesd_to'.
      when 'thursday'.
        ep_from_field_name = 'thurs_from'.
        ep_to_field_name = 'thursda_to'.
      when 'friday'.
        ep_from_field_name = 'frida_from'.
        ep_to_field_name = 'friday_to'.
      when 'saturday'.
        ep_from_field_name = 'satur_from'.
        ep_to_field_name = 'saturda_to'.
      when 'sunday'.
        ep_from_field_name = 'sunda_from'.
        ep_to_field_name = 'sunday_to'.

    endcase. " case lv_day_of_week_name

  endmethod.

  method is_date_holiday.

    call function 'HOLIDAY_CHECK_AND_GET_INFO'
      exporting
        date                = ip_date
        holiday_calendar_id = mv_factory_calendar
      importing
        holiday_found       = rp_holiday.

  endmethod.


  method get_date_day_name.

    data: lv_day_of_week_number type char1,
          lv_day_of_week_name   type char60.

    lv_day_of_week_number = me->get_day_number_of_week( ip_date ).

    lv_day_of_week_name = mt_idd07v[ domvalue_l = lv_day_of_week_number
]-ddtext.

    translate lv_day_of_week_name to lower case.

    rp_day_of_week_name = lv_day_of_week_name.

  endmethod.

  method get_field_name_for_date.

    data:

    lv_day_of_week_name   type char60.

    lv_day_of_week_name = me->get_date_day_name( ip_date ).

    me->get_field_name_for_week_day(
        exporting
        ip_day_of_week_name = lv_day_of_week_name
        importing
        ep_from_field_name = ep_from_field_name
        ep_to_field_name = ep_to_field_name ).

  endmethod.

  method get_day_number_of_week.


    call function 'DATE_COMPUTE_DAY'
      exporting
        date = ip_date
      importing
        day  = ep_day_of_week_number.

  endmethod.

  method set_added_hours_total.

    mv_added_hours_total = ip_added_hours_total.

  endmethod.

  method set_added_seconds_total.

    mv_added_seconds_total = ip_added_seconds_total.

  endmethod.

  method set_added_hrs_array_from_sec.

    data: lv_added_hours_left  type sc_rulewfr,
          wa_added_hours_array type ty_times_array_tt.


    clear mt_added_hours_array.

    data(lv_added_full_days) = mv_added_seconds_total div 86400.
    data(lv_left_seconds) = mv_added_seconds_total mod 86400.

    data(lv_added_hours) =  mv_added_seconds_total div 3600.

    " We are adding less than 24 hours

    if lv_added_full_days = 0.

      wa_added_hours_array-added_hours_left  = mv_added_seconds_total.
      append wa_added_hours_array to mt_added_hours_array.

      " We are adding exactly 24 hours

    elseif ( lv_added_full_days = 1 ) and ( lv_left_seconds = 0 ).

      lv_added_hours = lv_added_hours - 1.
      lv_added_hours_left = lv_added_hours * 3600.
      lv_added_hours_left = lv_added_hours_left + ( 59 * 60 ) + 59.

      wa_added_hours_array-added_hours_left  = lv_added_hours_left.
      append wa_added_hours_array to mt_added_hours_array.

    elseif lv_added_full_days > 1.

      " Adding what is left

      lv_added_hours_left = 23 * 3600.
      lv_added_hours_left = lv_added_hours_left + ( 59 * 60 ) + 59.

      do lv_added_full_days times.

        wa_added_hours_array-added_hours_left  = lv_added_hours_left.

        append wa_added_hours_array to mt_added_hours_array.

      enddo. "  do lv_amount_of_days_left times

      clear wa_added_hours_array.

      data(lv_added_minutes) = ( mv_added_seconds_total - (
lv_added_hours * 3600 ) ) div 60.
      data(lv_added_seconds) = mv_added_seconds_total - ( (
lv_added_hours * 3600 ) + ( lv_added_minutes * 60 ) ).


      wa_added_hours_array-added_hours_left =
       ( ( lv_added_full_days * 24 ) - lv_added_hours ) * 3600 +
           ( lv_added_minutes * 60 ) +
            lv_added_seconds.

      if wa_added_hours_array-added_hours_left ne '000000'.
        append wa_added_hours_array to mt_added_hours_array.
      endif.

    endif.

  endmethod.

  method set_added_hrs_array_from_hrs.

    data: lv_added_hours_left  type sc_rulewfr,
          wa_added_hours_array type ty_times_array_tt,
          lv_added_hours_total type /iwbep/sb_odata_ty_int2.

    clear mt_added_hours_array.

    lv_added_hours_total = mv_added_hours_total.

    " Preparing for calculations when amount of hours is < = or > 24 hours

    if lv_added_hours_total < 24.

      lv_added_hours_left = lv_added_hours_total * 3600.
      wa_added_hours_array-added_hours_left  = lv_added_hours_left.
      append wa_added_hours_array to mt_added_hours_array.

    endif. " if lv_added_hours_total < 24

    if  lv_added_hours_total = 24.

      lv_added_hours_total = lv_added_hours_total - 1.
      lv_added_hours_left = lv_added_hours_total * 3600.
      lv_added_hours_left = lv_added_hours_left + ( 59 * 60 ) + 59.
      wa_added_hours_array-added_hours_left  = lv_added_hours_left.

      append wa_added_hours_array to mt_added_hours_array.

    endif. " if  lv_added_hours_total = 24


    if  lv_added_hours_total > 24.

      " Calculate amount of days

      data(lv_amount_of_days_left) = lv_added_hours_total div 24.

      " Calculate amount of hours left in a last day

      data(lv_amount_of_hours_left) = lv_added_hours_total mod 24.
      lv_added_hours_left = 23 * 3600.
      lv_added_hours_left = lv_added_hours_left + ( 59 * 60 ) + 59.

      do lv_amount_of_days_left times.

        wa_added_hours_array-added_hours_left  = lv_added_hours_left.

        append wa_added_hours_array to mt_added_hours_array.

      enddo. "  do lv_amount_of_days_left times

      clear wa_added_hours_array.

      if lv_amount_of_hours_left is not initial.

        lv_amount_of_hours_left = lv_amount_of_hours_left * 3600.
        wa_added_hours_array-added_hours_left  =
lv_amount_of_hours_left.

        append wa_added_hours_array to mt_added_hours_array.

      endif. " if lv_amount_of_hours_left is not INITIAL

    endif. " if  lv_added_hours_total > 24



  endmethod.


  method set_list_of_periods.

    data wa_from_to type ty_from.

    field-symbols :
      <lv_weekday_active> type any,
      <lv_from>           type any,
      <lv_to>             type any.

    loop at ms_scrule_exp-tab_w into data(wa_tab_w).

      if <lv_from> is assigned.
        unassign <lv_from>.
      endif.

      if <lv_to> is assigned.
        unassign <lv_to>.
      endif.

      if <lv_weekday_active> is assigned.
        unassign <lv_weekday_active>.
      endif.

      assign component ip_from_field_name of structure wa_tab_w to
<lv_from>.
      assign component ip_to_field_name of structure wa_tab_w to
<lv_to>.
      assign component me->get_date_day_name( ip_date ) of structure
wa_tab_w to <lv_weekday_active>.

      " Filling work times for active periods

      clear wa_from_to.

      if <lv_weekday_active> = 'X'.

        if <lv_from> is not initial.
          wa_from_to-from = <lv_from>.
        endif. " if <lv_from> is not initial

        if <lv_to> is not initial.
          wa_from_to-to = <lv_to>.
        endif. " if <lv_to> is not initial

        if wa_from_to is not initial.
          append wa_from_to to et_all_from_to.
        endif. " if wa_from_to is not initial

      endif. " if <lv_weekday_active> = 'X'

    endloop. " loop at ls_scrule_exp-tab_w into data(wa_tab_w)

  endmethod.

  method yif_serv_profile~add_hours_to_date.

    if ( mv_ruleid is initial ) or ( mt_rule_tab is initial ).
      return.
    endif.

    data:

      lv_added_hours_left      type sc_rulewfr,
      lv_time_shifted          type sy-uzeit,
      lv_date_shifted          type sy-datum,
      lt_all_from_to           type standard table of ty_from,
      lt_selected_from_to      type standard table of ty_from,
      wa_from_to               type ty_from,
      lv_from_field_name       type string,
      lv_to_field_name         type string,
      lv_how_much_in_period    type sc_rulewfr,
      lv_period_boundaries_fit type char1,
      lv_gap_start             type sc_rulewfr,
      lv_gap_length            type sc_rulewfr,
      lv_periods_count         type int4,
      lv_calculation_period    type sc_rulewfr,
      lv_added_hours_total     type /iwbep/sb_odata_ty_int2.

    field-symbols :
      <lv_weekday_active>   type any,
      <ls_all_from_to>      like line of lt_all_from_to,
      <ls_selected_from_to> like line of lt_selected_from_to.

    me->set_added_hours_total( ip_added_hours_total  ).

    me->set_added_hrs_array_from_hrs(  ).

    if ( ip_date_from is initial ) and ( ip_time_from is initial ).
      lv_date_shifted = sy-datum.
      lv_time_shifted = sy-uzeit.
    else.
      lv_date_shifted = ip_date_from.
      lv_time_shifted = ip_time_from.
    endif.

    loop at mt_added_hours_array assigning field-symbol(<ls_added_hours_array>).

      lv_added_hours_left = <ls_added_hours_array>-added_hours_left.

      while lv_added_hours_left > 0.

        " Check if day is a business day

        if me->is_date_holiday( lv_date_shifted ).

          lv_date_shifted = lv_date_shifted + 1.

          continue.

        endif.

        me->get_field_name_for_date(
            exporting
                ip_date = lv_date_shifted
            importing
                ep_from_field_name = lv_from_field_name
                ep_to_field_name = lv_to_field_name ).

        clear: lt_all_from_to,
                wa_from_to.

        " Preparing a list of work periods within a particular work day

        me->set_list_of_periods(
            exporting
             ip_date = lv_date_shifted
             ip_from_field_name = lv_from_field_name
             ip_to_field_name = lv_to_field_name
            importing
             et_all_from_to = lt_all_from_to ).

        if <ls_all_from_to> is assigned.
          unassign <ls_all_from_to>.
        endif.

        " Finding periods of work day, valid for processing

        clear: lt_selected_from_to,
               lv_period_boundaries_fit.

        loop at lt_all_from_to assigning <ls_all_from_to>.

          " If we have switched to a new day, then we take all periods of a day and quit

          if ( lv_time_shifted is initial ).

            lv_time_shifted = <ls_all_from_to>-from .
            lt_selected_from_to = lt_all_from_to.
            exit.

          endif. " if ( lv_time_shifted is initial )

          " We add more periods (if there are any) if on a previous step we were inside a particular period

          if lv_period_boundaries_fit = 'X'.

            wa_from_to-from = <ls_all_from_to>-from.
            wa_from_to-to = <ls_all_from_to>-to.
            append wa_from_to to lt_selected_from_to.

          endif. " if lv_period_boundaries_fit = 'X'

          " If we currently we are inside a particular period

          if ( lv_time_shifted ge <ls_all_from_to>-from  ) and ( lv_time_shifted le <ls_all_from_to>-to ).

            clear wa_from_to.
            wa_from_to-from = <ls_all_from_to>-from.
            wa_from_to-to = <ls_all_from_to>-to.
            append wa_from_to to lt_selected_from_to.
            lv_period_boundaries_fit = 'X'.

          endif. " if ( lv_time_shifted ge <ls_all_from_to>-from  ) and ( lv_time_shifted le <ls_all_from_to>-to )

        endloop. " loop at lt_all_from_to assigning <ls_all_from_to>

        " If there are no valid periods we have to switch to the next day

        if lt_selected_from_to is initial.

          lv_date_shifted = lv_date_shifted + 1.
          clear lv_time_shifted .

        endif. " if lt_selected_from_to is initial

        clear:
              lv_gap_start,
              lv_gap_length,
              lv_periods_count.

        " Getting amount of valid periods for the work day

        sort lt_selected_from_to.
        delete adjacent duplicates from lt_selected_from_to.

        lv_periods_count = lines( lt_selected_from_to ).

        " Scanning the period

        loop at lt_selected_from_to assigning <ls_selected_from_to>.

          " Check if there was a gap from previous period: if yes - we are shifing time on a period length

          if ( lv_gap_start is not initial ) and
            ( ( <ls_selected_from_to>-from - lv_gap_start ) > 0 ) .

            " If yes then shift time
            lv_gap_length = <ls_selected_from_to>-from - lv_gap_start.

            lv_time_shifted = lv_time_shifted + lv_gap_length.

            clear: lv_gap_start,
                   lv_gap_length.

          endif. " if ( lv_gap_start is not initial )

          " Calculation period is a time between end of period and time where are we now

          lv_calculation_period = <ls_selected_from_to>-to - lv_time_shifted.

          " Calculating time we can shift for a current period
          " lv_how_much_in_period - how much time we can cover if we are in period until period ends

          if  ( lv_calculation_period > lv_added_hours_left ).
            lv_how_much_in_period = lv_added_hours_left.
          else.
            lv_how_much_in_period =  lv_calculation_period.
          endif. " if  ( lv_calculation_period > lv_added_hours_left )

          " Shifting time and reducing amount of hours left for addition

          lv_time_shifted = lv_time_shifted + lv_how_much_in_period.

          if lv_how_much_in_period < lv_added_hours_left.
            lv_added_hours_left = lv_added_hours_left - lv_how_much_in_period.

          else.
            clear lv_added_hours_left.
          endif. " if lv_how_much_in_period < lv_added_hours_left

          " Setting a new gap for further processing

          lv_gap_start = <ls_selected_from_to>-to.

          " If passed through all day - shift date and time

          if ( lv_added_hours_left > 0  ) and ( sy-tabix = lv_periods_count ).

            lv_date_shifted = lv_date_shifted + 1.

            clear lv_time_shifted .

            " If already added all required hours then exiting

          elseif ( lv_added_hours_left is initial ) or ( lv_added_hours_left eq '000000' ).

            exit.

          endif. " if ( lv_added_hours_left > 0  ) and ( sy-tabix = lv_periods_count )

        endloop. " loop at lt_selected_from_to assigning <ls_selected_from_to>

      endwhile. "while lv_added_hours_left > 0

    endloop. " loop at lt_added_hours_array ASSIGNING FIELD-SYMBOL(<ls_added_hours_array>)

    " Finalizing results

    ep_sla_date = lv_date_shifted.
    ep_sla_time = lv_time_shifted.

  endmethod.

  method yif_serv_profile~add_seconds_to_date.

    if ( mv_ruleid is initial ) or ( mt_rule_tab is initial ).
      return.
    endif.

    data:

      lv_added_hours_left      type sc_rulewfr,
      lv_time_shifted          type sy-uzeit,
      lv_date_shifted          type sy-datum,
      lt_all_from_to           type standard table of ty_from,
      lt_selected_from_to      type standard table of ty_from,
      wa_from_to               type ty_from,
      lv_from_field_name       type string,
      lv_to_field_name         type string,
      lv_how_much_in_period    type sc_rulewfr,
      lv_period_boundaries_fit type char1,
      lv_gap_start             type sc_rulewfr,
      lv_gap_length            type sc_rulewfr,
      lv_periods_count         type int4,
      lv_calculation_period    type sc_rulewfr,
      lv_added_hours_total     type /iwbep/sb_odata_ty_int2.

    field-symbols :
      <lv_weekday_active>   type any,
      <ls_all_from_to>      like line of lt_all_from_to,
      <ls_selected_from_to> like line of lt_selected_from_to.

    me->set_added_seconds_total( ip_added_seconds_total  ).

    me->set_added_hrs_array_from_sec(  ).

    if ( ip_date_from is initial ) and ( ip_time_from is initial ).
      lv_date_shifted = sy-datum.
      lv_time_shifted = sy-uzeit.
    else.
      lv_date_shifted = ip_date_from.
      lv_time_shifted = ip_time_from.
    endif.

    loop at mt_added_hours_array assigning field-symbol(<ls_added_hours_array>).

      lv_added_hours_left = <ls_added_hours_array>-added_hours_left.

      while lv_added_hours_left > 0.

        " Check if day is a business day

        if me->is_date_holiday( lv_date_shifted ).

          lv_date_shifted = lv_date_shifted + 1.

          continue.

        endif.

        me->get_field_name_for_date(
            exporting
                ip_date = lv_date_shifted
            importing
                ep_from_field_name = lv_from_field_name
                ep_to_field_name = lv_to_field_name ).

        clear: lt_all_from_to,
                wa_from_to.

        " Preparing a list of work periods within a particular work day

        me->set_list_of_periods(
            exporting
             ip_date = lv_date_shifted
             ip_from_field_name = lv_from_field_name
             ip_to_field_name = lv_to_field_name
            importing
             et_all_from_to = lt_all_from_to ).

        if <ls_all_from_to> is assigned.
          unassign <ls_all_from_to>.
        endif.

        " Finding periods of work day, valid for processing

        clear: lt_selected_from_to,
               lv_period_boundaries_fit.

        loop at lt_all_from_to assigning <ls_all_from_to>.

          " If we have switched to a new day, then we take all periods of a day and quit

          if ( lv_time_shifted is initial ).

            lv_time_shifted = <ls_all_from_to>-from .
            lt_selected_from_to = lt_all_from_to.
            exit.

          endif. " if ( lv_time_shifted is initial )

          " We add more periods (if there are any) if on a previous step we were inside a particular period

          if lv_period_boundaries_fit = 'X'.

            wa_from_to-from = <ls_all_from_to>-from.
            wa_from_to-to = <ls_all_from_to>-to.
            append wa_from_to to lt_selected_from_to.

          endif. " if lv_period_boundaries_fit = 'X'

          " If we currently we are inside a particular period

          if ( lv_time_shifted ge <ls_all_from_to>-from  ) and ( lv_time_shifted le <ls_all_from_to>-to ).

            clear wa_from_to.
            wa_from_to-from = <ls_all_from_to>-from.
            wa_from_to-to = <ls_all_from_to>-to.
            append wa_from_to to lt_selected_from_to.
            lv_period_boundaries_fit = 'X'.

          endif. " if ( lv_time_shifted ge <ls_all_from_to>-from  ) and ( lv_time_shifted le <ls_all_from_to>-to )

        endloop. " loop at lt_all_from_to assigning <ls_all_from_to>

        " If there are no valid periods we have to switch to the next day

        if lt_selected_from_to is initial.

          lv_date_shifted = lv_date_shifted + 1.
          clear lv_time_shifted .

        endif. " if lt_selected_from_to is initial

        clear:
              lv_gap_start,
              lv_gap_length,
              lv_periods_count.

        " Getting amount of valid periods for the work day

        sort lt_selected_from_to.
        delete adjacent duplicates from lt_selected_from_to.

        lv_periods_count = lines( lt_selected_from_to ).

        " Scanning the period

        loop at lt_selected_from_to assigning <ls_selected_from_to>.

          " Check if there was a gap from previous period: if yes - we are shifing time on a period length

          if ( lv_gap_start is not initial ) and
            ( ( <ls_selected_from_to>-from - lv_gap_start ) > 0 ) .

            " If yes then shift time
            lv_gap_length = <ls_selected_from_to>-from - lv_gap_start.

            lv_time_shifted = lv_time_shifted + lv_gap_length.

            clear: lv_gap_start,
                   lv_gap_length.

          endif. " if ( lv_gap_start is not initial )

          " Calculation period is a time between end of period and time where are we now

          lv_calculation_period = <ls_selected_from_to>-to - lv_time_shifted.

          " Calculating time we can shift for a current period
          " lv_how_much_in_period - how much time we can cover if we are in period until period ends

          if  ( lv_calculation_period > lv_added_hours_left ).
            lv_how_much_in_period = lv_added_hours_left.
          else.
            lv_how_much_in_period =  lv_calculation_period.
          endif. " if  ( lv_calculation_period > lv_added_hours_left )

          " Shifting time and reducing amount of hours left for addition

          lv_time_shifted = lv_time_shifted + lv_how_much_in_period.

          if lv_how_much_in_period < lv_added_hours_left.
            lv_added_hours_left = lv_added_hours_left - lv_how_much_in_period.

          else.
            clear lv_added_hours_left.
          endif. " if lv_how_much_in_period < lv_added_hours_left

          " Setting a new gap for further processing

          lv_gap_start = <ls_selected_from_to>-to.

          " If passed through all day - shift date and time

          if ( lv_added_hours_left > 0  ) and ( sy-tabix = lv_periods_count ).

            lv_date_shifted = lv_date_shifted + 1.

            clear lv_time_shifted .

            " If already added all required hours then exiting

          elseif ( lv_added_hours_left is initial ) or ( lv_added_hours_left eq '000000' ).

            exit.

          endif. " if ( lv_added_hours_left > 0  ) and ( sy-tabix = lv_periods_count )

        endloop. " loop at lt_selected_from_to assigning <ls_selected_from_to>

      endwhile. "while lv_added_hours_left > 0

    endloop. " loop at lt_added_hours_array ASSIGNING FIELD-SYMBOL (<ls_added_hours_array>)

    " Finalizing results

    ep_sla_date = lv_date_shifted.
    ep_sla_time = lv_time_shifted.

  endmethod.

  METHOD yif_serv_profile~is_now_an_availability_time.

      data:
      lv_time_now        type sy-uzeit,
      lv_date_now        type sy-datum,
      lv_timestamp_now   type timestamp,
      lv_from_field_name type string,
      lv_to_field_name   type string,
      lt_all_from_to     type standard table of ty_from.


    rp_result = abap_true.

    " Getting current time and date

    get time stamp field lv_timestamp_now.

    ycl_assistant_utilities=>get_date_time_from_timestamp(
        exporting
            ip_timestamp = lv_timestamp_now
        importing
            ep_date = lv_date_now
            ep_time = lv_time_now ).

    " Check if today is not a holiday

    if me->is_date_holiday( lv_date_now ) eq abap_false.

      me->get_field_name_for_date(
         exporting
             ip_date = lv_date_now
         importing
             ep_from_field_name = lv_from_field_name
             ep_to_field_name = lv_to_field_name ).

      " Preparing a list of work periods within a particular work day

      me->set_list_of_periods(
          exporting
           ip_date = lv_date_now
           ip_from_field_name = lv_from_field_name
           ip_to_field_name = lv_to_field_name
          importing
           et_all_from_to = lt_all_from_to ).

      " Checking if now is a working time

      if lt_all_from_to is initial.

        rp_result = abap_false.

      endif.

      loop at lt_all_from_to assigning field-symbol(<ls_all_from_to>).

        if ( lv_time_now > <ls_all_from_to>-to ) or
        ( lv_time_now < <ls_all_from_to>-from ).

          rp_result = abap_false.

        endif.      .

      endloop.

    else.

      rp_result = abap_false.

    endif.

  ENDMETHOD.

endclass.
