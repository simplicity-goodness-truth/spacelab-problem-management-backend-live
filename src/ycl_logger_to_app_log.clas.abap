class YCL_LOGGER_TO_APP_LOG definition
  public
  final
  create private .

  public section.

    interfaces yif_logger .

    class-methods: get_instance
      returning
        value(er_app_logger_instance) type ref to YCL_LOGGER_TO_APP_LOG,

      set_object_and_subobject
        importing
          !ip_object    type balobj_d
          !ip_subobject type balsubobj .


  protected section.
  private section.

    class-data: mv_message_type        type symsgty,
                mv_object              type balobj_d,
                mv_subobject           type balsubobj,
                mo_app_logger_instance type ref to
YCL_LOGGER_TO_APP_LOG.

    class-methods add_record
      importing
        !ip_message type string .

endclass.

class YCL_LOGGER_TO_APP_LOG implementation.

  method add_record.

    " Data declaration for Application Log operations

    data: ls_log        type bal_s_log,
          ev_log_handle type balloghndl,
          ls_msg        type bal_s_msg,
          lt_log_handle type bal_t_logh,
          lt_log_num    type bal_t_lgnm,
          lv_subrc      type sysubrc.
    data:
      begin of ls_string,
        part1 type symsgv,
        part2 type symsgv,
        part3 type symsgv,
        part4 type symsgv,
      end of ls_string.

    concatenate sy-uzeit ip_message into ls_string separated by space.

    " Check if object and sub-object are provided

    if ( mv_object is initial ) and ( mv_subobject is initial ).
      return.
    endif.

    " Preparing the Application Log

    clear ev_log_handle.

    ls_log-object    = mv_object.
    ls_log-subobject = mv_subobject.

    ls_log-aldate    = sy-datum.
    ls_log-altime    = sy-uzeit.
    ls_log-aluser    = sy-uname.

    if ( ls_log-object is initial ) or ( ls_log-subobject is initial ).
      return.
    endif.

    call function 'BAL_LOG_CREATE'
      exporting
        i_s_log                 = ls_log
      importing
        e_log_handle            = ev_log_handle
      exceptions
        log_header_inconsistent = 1
        others                  = 2.
    if sy-subrc <> 0.
      lv_subrc = 1.
      return.
    endif.

    ls_msg-msgv1     = ls_string-part1.
    ls_msg-msgv2     = ls_string-part2.
    ls_msg-msgv3     = ls_string-part3.
    ls_msg-msgv4     = ls_string-part4.

    ls_msg-msgty = mv_message_type.
    ls_msg-msgid = 'BL'.
    ls_msg-msgno = '001'.

    call function 'BAL_LOG_MSG_ADD'
      exporting
        i_log_handle  = ev_log_handle
        i_s_msg       = ls_msg
      exceptions
        log_not_found = 0
        others        = 1.

    if sy-subrc <> 0.
      message id sy-msgid type sy-msgty number sy-msgno
      with sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.

    endif.

    " Finalizing the Application Log records

    insert ev_log_handle into lt_log_handle index 1.

    call function 'BAL_DB_SAVE'
      exporting
        i_client         = sy-mandt
        i_save_all       = ' '
        i_t_log_handle   = lt_log_handle
      importing
        e_new_lognumbers = lt_log_num
      exceptions
        log_not_found    = 1
        save_not_allowed = 2
        numbering_error  = 3
        others           = 4.
    if sy-subrc <> 0.
      lv_subrc = sy-subrc.
    endif.

  endmethod.

  method yif_logger~err.


    mv_message_type = 'E'.

    add_record( ip_message_text ).

  endmethod.

  method get_instance.

    if mo_app_logger_instance is initial.
      create object mo_app_logger_instance.
    endif.

    er_app_logger_instance = mo_app_logger_instance.

  endmethod.

  method yif_logger~info.

    mv_message_type = 'I'.

    add_record( ip_message_text ).

  endmethod.

  method set_object_and_subobject.

    mv_object = ip_object.
    mv_subobject = ip_subobject.

  endmethod.


  method yif_logger~warn.

    mv_message_type = 'W'.

    add_record( ip_message_text ).


  endmethod.

endclass.
