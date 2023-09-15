class ycl_assistant_utilities definition
  public
  create public .

  public section.

    class-methods: convert_timestamp_to_timezone
      importing
        ip_timestamp                    type timestamp
        ip_timezone                     type timezone
      returning
        value(rp_timestamp_in_timezone) type timestamp ,
      get_date_from_timestamp
        importing
          ip_timestamp   type timestamp
        returning
          value(rp_date) type dats ,
      get_time_from_timestamp
        importing
          ip_timestamp   type timestamp
        returning
          value(rp_time) type uzeit ,
      get_date_time_from_timestamp
        importing
          ip_timestamp   type timestamp
        exporting
          value(ep_date) type dats
          value(ep_time) type uzeit,
      get_system_timezone
        returning
          value(rp_system_timezone) type timezone,
      get_user_timezone
        importing
          ip_username        type xubname
        returning
          value(rp_timezone) type timezone
        raising
          resumable(ycx_assistant_utilities_exc),
      get_emails_by_user_name
        importing
          ip_user_name              type xubname
        returning
          value(et_email_addresses) type addrt_email_address
        raising
          ycx_assistant_utilities_exc ,
      get_http_param_value
        importing
          ip_header        type tihttpnvp
          ip_param_name    type char100
        returning
          value(ep_result) type string
        raising
          ycx_assistant_utilities_exc ,
      convert_char32_guid_to_raw16
        importing
          ip_guid_char32       type ags_sd_api_if_ws_obj_guid
        returning
          value(ep_guid_raw16) type sysuuid_x16
        raising
          ycx_assistant_utilities_exc ,
      convert_char36_guid_to_raw16
        importing
          ip_guid_char36       type char_36
        returning
          value(ep_guid_raw16) type sysuuid_x16
        raising
          ycx_assistant_utilities_exc ,
      format_date
        importing
          ip_format                type string
          ip_date                  type dats
        returning
          value(ep_formatted_date) type string
        raising
          ycx_assistant_utilities_exc ,
      format_time
        importing
          ip_time                  type tims
        returning
          value(ep_formatted_time) type string,
      format_timestamp
        importing
          ip_timestamp                  type timestamp
        returning
          value(rp_formatted_timestamp) type string
        raising
          ycx_assistant_utilities_exc,
      generate_x16_guid
        returning
          value(rp_sysuuid_x16_guid) type sysuuid_x16,
      calc_duration_btw_timestamps
        importing
          ip_timestamp_1     type timestamp
          ip_timestamp_2     type timestamp
        returning
          value(rp_duration) type integer ,

      remove_html_tags
        changing
          cs_string type string,

      is_valid_email_address
        importing
          ip_email_address     type string
        returning
          value(ep_is_correct) type abap_bool,

      convert_time_to_seconds
        importing
          ip_amount_in_input_time_unit type int4
          ip_input_time_unit           type timeunitdu
        returning
          value(rp_output_in_seconds)  type int4.

  protected section.
  private section.
endclass.



class ycl_assistant_utilities implementation.


  method convert_char32_guid_to_raw16.

    data:
      lv_guid_char_32 type char32.

    if ( strlen( ip_guid_char32 ) eq 32 ).

      lv_guid_char_32 = ip_guid_char32.

      translate lv_guid_char_32 to upper case.

      ep_guid_raw16 = lv_guid_char_32.

    else.

      raise exception type ycx_assistant_utilities_exc
        exporting
          textid = ycx_assistant_utilities_exc=>incorrect_guid_format.

    endif. " if ( sy-subrc eq 0 ) and ( strlen( ip_guid_char36 ) eq 36 )

  endmethod.

  method convert_char36_guid_to_raw16.

    data:
      lv_guid_char_36 type char_36,
      lv_guid_char_32 type char32.

    search ip_guid_char36 for '-'.

    if ( sy-subrc eq 0 ) and ( strlen( ip_guid_char36 ) eq 36 ).

      lv_guid_char_36 = ip_guid_char36.

      replace all occurrences of '-' in lv_guid_char_36 with ' '.

      lv_guid_char_32 = lv_guid_char_36.

      translate lv_guid_char_32 to upper case.

      ep_guid_raw16 = lv_guid_char_32.

    else.

      raise exception type ycx_assistant_utilities_exc
        exporting
          textid = ycx_assistant_utilities_exc=>incorrect_guid_format.

    endif. " if ( sy-subrc eq 0 ) and ( strlen( ip_guid_char36 ) eq 36 )

  endmethod.


  method convert_timestamp_to_timezone.
    data:
      lv_date type sy-datum,
      lv_time type sy-uzeit.

    convert time stamp ip_timestamp time zone ip_timezone
        into date lv_date time lv_time.

    rp_timestamp_in_timezone = |{ lv_date }| && |{ lv_time }|.

  endmethod.


  method format_date.

    data: lv_year  type char4,
          lv_month type char2,
          lv_day   type char2.

    case ip_format.

      when 'DD.MM.YYYY'.

        lv_year = ip_date+0(4).
        lv_month = ip_date+4(2).
        lv_day  = ip_date+6(2).

        ep_formatted_date = |{ lv_day }| && |.| && |{ lv_month }| && |.|
&& |{ lv_year }|.

      when others.

        raise exception type ycx_assistant_utilities_exc
          exporting
            textid = ycx_assistant_utilities_exc=>unknown_conversion_format.

    endcase.

  endmethod.


  method format_time.

    data: lv_hour   type char2,
          lv_minute type char2,
          lv_second type char2.

    lv_hour = ip_time+0(2).
    lv_minute = ip_time+2(2).
    lv_second  = ip_time+4(2).

    ep_formatted_time = |{ lv_hour }| && |:| && |{ lv_minute }| && |:|
&& |{ lv_second  }|.

  endmethod.


  method get_date_from_timestamp.

    data:
      lv_date type sy-datum,
      lv_time type sy-uzeit.

    convert time stamp ip_timestamp time zone 'UTC'
        into date lv_date time lv_time.

    rp_date = lv_date.

  endmethod.

  method get_time_from_timestamp.

    data:
      lv_date type sy-datum,
      lv_time type sy-uzeit.

    convert time stamp ip_timestamp time zone 'UTC'
        into date lv_date time lv_time.

    rp_time = lv_time.

  endmethod.


  method get_emails_by_user_name.

    data: lv_addrnumber type ad_addrnum,
          lv_persnumber type ad_persnum.

    select single persnumber addrnumber into (lv_persnumber,
lv_addrnumber)
          from usr21 where bname eq ip_user_name.

    select smtp_addr from adr6 into corresponding fields of table
et_email_addresses
      where addrnumber = lv_addrnumber and persnumber = lv_persnumber.

    if sy-subrc <> 0.



      raise exception type ycx_assistant_utilities_exc
        exporting
          textid  = ycx_assistant_utilities_exc=>user_emails_not_found
          ip_user = ip_user_name.

    endif.

  endmethod.


  method get_http_param_value.

    data: lv_substring_to_search  type string.

    field-symbols: <fs_header> type /iwbep/s_mgw_name_value_pair.

    lv_substring_to_search = |{ ip_param_name }| && |=|.

    " Getting full URL

    read table ip_header assigning <fs_header> with key name =
'~request_uri'.

    " Searching for a parameter

    search <fs_header>-value for ip_param_name.


    if ( sy-fdpos > 0 ).

      ep_result = substring_after( val = <fs_header>-value sub =
lv_substring_to_search ).

      search ep_result for '&'.

      if ( sy-fdpos > 0 ).

        ep_result = substring_before( val = ep_result sub = '&' ).

      endif.

    else.

      raise exception type ycx_assistant_utilities_exc
        exporting
          textid       = ycx_assistant_utilities_exc=>cant_get_http_parameter_value
          ip_parameter = ip_param_name.


    endif. "  if ( sy-fdpos > 0 )

  endmethod.

  method get_system_timezone.

    call function 'GET_SYSTEM_TIMEZONE'
      importing
        timezone = rp_system_timezone.

  endmethod.

  method get_user_timezone.

    data: lv_timezone type timezone.

    " First look into usr02

    select single tzone from usr02 into lv_timezone
      where bname = ip_username.

    " Look into customizing table TTZCU

    if sy-subrc <> 0.

      raise exception type ycx_assistant_utilities_exc
        exporting
          textid = ycx_assistant_utilities_exc=>not_valid_user.

    endif.

    if lv_timezone is initial.

      select single tzonedef from ttzcu into lv_timezone.

      if sy-subrc <> 0 or lv_timezone is initial.

        raise exception type ycx_assistant_utilities_exc
          exporting
            textid = ycx_assistant_utilities_exc=>no_timezone_customizing.

      endif. " if sy-subrc <> 0 or lv_timezone is initial

    endif. " if lv_timezone is initial

    rp_timezone = lv_timezone.

  endmethod.

  method format_timestamp.

    data: lv_date           type dats,
          lv_time           type uzeit,
          lv_date_formatted type string,
          lv_time_formatted type string.

    get_date_time_from_timestamp(
        exporting ip_timestamp = ip_timestamp
        importing ep_date = lv_date ep_time = lv_time ).


    lv_date_formatted = format_date(
        exporting ip_format = 'DD.MM.YYYY'
        ip_date = lv_date ).

    lv_time_formatted = format_time( lv_time ).

    rp_formatted_timestamp = |{ lv_date_formatted }| && | | && |{
lv_time_formatted }|.


  endmethod.

  method get_date_time_from_timestamp.

    data:
      lv_date            type sy-datum,
      lv_time            type sy-uzeit,
      lv_system_timezone type timezone.

    lv_system_timezone = get_system_timezone(  ).

    convert time stamp ip_timestamp time zone lv_system_timezone
        into date lv_date time lv_time.

    ep_date = lv_date.
    ep_time = lv_time.


  endmethod.

  method generate_x16_guid.

    " Standard generation of X16 GUID

    try.
        rp_sysuuid_x16_guid = cl_system_uuid=>create_uuid_x16_static( ).

      catch cx_uuid_error.
        rp_sysuuid_x16_guid = '0'.
    endtry.

  endmethod.


  method calc_duration_btw_timestamps.


    data lv_timestamp_1   type c length 14.
    data lv_timestamp_2   type c length 14.


    data: lv_date_1 type d,
          lv_date_2 type d,
          lv_time_1 type t,
          lv_time_2 type t.

    " Check if one of provided parameters is not zero

    if ( ip_timestamp_1 = 0 ) or ( ip_timestamp_2 = 0 ).

      rp_duration = 0.
      return.

    endif.

    " Difference calculation

    lv_timestamp_1 = ip_timestamp_1.
    lv_timestamp_2 = ip_timestamp_2.

    if ip_timestamp_1 < ip_timestamp_2.
      lv_date_2       = lv_timestamp_1(8).
      lv_time_2       = lv_timestamp_1+8(6).
      lv_date_1       = lv_timestamp_2(8).
      lv_time_1       = lv_timestamp_2+8(6).
    else.
      lv_date_1       = lv_timestamp_1(8).
      lv_time_1       = lv_timestamp_1+8(6).
      lv_date_2       = lv_timestamp_2(8).
      lv_time_2       = lv_timestamp_2+8(6).
    endif.

    " Duration in seconds

    rp_duration = ( ( ( lv_date_1 - lv_date_2 ) * 86400
                 + ( lv_time_1 - lv_time_2 ) ) ).

  endmethod.

  method remove_html_tags.

    replace all occurrences of regex '<[a-zA-Z\/][^>]*>' in cs_string with space.

  endmethod.

  method is_valid_email_address.

    data: go_regex   type ref to cl_abap_regex,
          go_matcher type ref to cl_abap_matcher,
          go_match   type c length 1,
          gv_msg     type string.

    ep_is_correct = abap_false.

    create object go_regex
      exporting
        pattern     = '\w+(\.\w+)*@(\w+\.)+(\w{2,4})'
        ignore_case = abap_true.

    go_matcher = go_regex->create_matcher( text = ip_email_address ).

    if go_matcher->match( ) is not initial.

      ep_is_correct = abap_true.

    endif.

  endmethod.

  method convert_time_to_seconds.

    data lv_multiplier type int4.

    lv_multiplier = switch timeunitdu(
        ip_input_time_unit
            when 'SECOND' then 1
            when 'MINUTE' then 60
            when 'HOUR' then 3600
            when 'DAY' then 86400
            when 'WEEK' then 604800 ).

    rp_output_in_seconds = ip_amount_in_input_time_unit * lv_multiplier.

  endmethod.

endclass.
