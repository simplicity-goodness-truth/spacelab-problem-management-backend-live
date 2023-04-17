class ycl_standart_text definition
  public
  create public .

  public section.

    interfaces yif_standard_text .

    methods constructor
      importing
        ip_text_name     type tdobname
        ip_text_language type spras.

  protected section.

  private section.
    data: mt_text_lines type table of tline.

    class-data: mo_log               type ref to ycl_logger_to_app_log,
                mv_app_log_object    type balobj_d,
                mv_app_log_subobject type balsubobj.

    methods set_app_logger
      raising
        ycx_slpm_configuration_exc.

endclass.

class ycl_standart_text implementation.

  method yif_standard_text~get_compiled_text_by_name.

    data: lv_text_for_processing type string,
          lv_text_token          type string.

    lv_text_for_processing = me->yif_standard_text~get_raw_text_by_name(  ).

    loop at it_variables_values assigning field-symbol(<is_variable_value>).

      lv_text_token = <is_variable_value>-value.

      if ( ip_use_tags eq abap_true ).

        if <is_variable_value>-opentag is not initial.

          lv_text_token = |{ <is_variable_value>-opentag }| && |{ lv_text_token }|.

        endif.

        if <is_variable_value>-closetag is not initial.

          lv_text_token = |{ lv_text_token }| && |{ <is_variable_value>-closetag }|.

        endif.

      endif.

      replace all occurrences of <is_variable_value>-variable in lv_text_for_processing with lv_text_token.

    endloop.

    rp_compiled_text = lv_text_for_processing.

  endmethod.

  method yif_standard_text~get_raw_text_by_name.

    if mt_text_lines is not initial.

      data ls_text_line type char255.

      loop at mt_text_lines into ls_text_line.

        rp_raw_text  = rp_raw_text && | | && |{ ls_text_line+2 }|.

      endloop. "loop at lt_line into ls_line

    endif.

  endmethod.

  method constructor.

    call function 'READ_TEXT'
      exporting
        client                  = sy-mandt
        id                      = 'ST'
        language                = ip_text_language
        name                    = ip_text_name
        object                  = 'TEXT'
      tables
        lines                   = mt_text_lines
      exceptions
        id                      = 1
        language                = 2
        name                    = 3
        not_found               = 4
        object                  = 5
        reference_check         = 6
        wrong_access_to_archive = 7
        others                  = 8.

    if sy-subrc ne 0.

      data lv_log_record_text type string.

      lv_log_record_text = |{ sy-msgid  }| && |{ sy-msgty }| && |{ sy-msgno }| &&
        |{ sy-msgv1 }| && |{ sy-msgv2 }| && |{ sy-msgv3 }| && |{ sy-msgv4 }|.

      mo_log->yif_logger~err( lv_log_record_text  ).

    endif.

  endmethod.

  method set_app_logger.

    mv_app_log_object = 'YTEXTS'.
    mv_app_log_subobject = 'YREADTEXTS'.

    mo_log = ycl_logger_to_app_log=>get_instance( ).
    mo_log->set_object_and_subobject(
          exporting
            ip_object    =   mv_app_log_object
            ip_subobject =   mv_app_log_subobject ).

  endmethod.

endclass.
