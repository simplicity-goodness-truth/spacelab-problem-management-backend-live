class YCL_SLPM_PROB_EXP_DEC_TXT definition
  public
  inheriting from ycl_slpm_prob_exp_dec
  final
  create public .

  public section.

    methods:

      yif_slpm_prob_exp~export_problems redefinition.

  protected section.

  private section.

    data:

      mt_problems_export        type yif_slpm_prob_exp=>ty_problems,
      mv_problems_export_txt    type string,
      mv_problems_export_folder type string value 'C:\tmp\',
      mo_file_system            type ref to yif_file_system.

    methods:

      save_problems_exp_txt_to_fs
        raising
          ycx_assistant_utilities_exc,

      save_problem_exp_txt_to_fs
        importing
          is_problem                    type yif_slpm_prob_exp=>ty_problem
          ip_problem_file_path_and_name type string
        raising
          ycx_assistant_utilities_exc,

      save_problem_exp_txt
        importing
          ip_export_file_path_and_name type string,

      set_problem_exp_folder
        raising
          ycx_slpm_configuration_exc,

      compose_problem_attributes
        importing
          is_problem                    type ycrm_order_ts_sl_problem
        exporting
          ep_problem_folder_path        type string
          ep_problem_file_path_and_name type string,


      set_file_system,

      create_problem_folder
        importing
          ip_folder_path type string.


endclass.

class YCL_SLPM_PROB_EXP_DEC_TXT implementation.

  method yif_slpm_prob_exp~export_problems .

    if it_problems_export is initial.

      mt_problems_export = mo_slpm_prob_exp->yif_slpm_prob_exp~export_problems(  ).

    else.

      mt_problems_export = it_problems_export.

    endif.

    me->set_file_system(  ).

    me->set_problem_exp_folder(  ).

    me->save_problems_exp_txt_to_fs(  ).


  endmethod.




  method save_problems_exp_txt_to_fs.

    data:
      lv_problem_file_path_and_name type string,
      lv_problem_folder_path        type string.

    loop at mt_problems_export assigning field-symbol(<fs_problem_export>).

      " Composing folder name and file name

      me->compose_problem_attributes(
          exporting
              is_problem = <fs_problem_export>-data
          importing
              ep_problem_file_path_and_name = lv_problem_file_path_and_name
              ep_problem_folder_path = lv_problem_folder_path
       ).

      " Create a dedicated single problem folder

      me->create_problem_folder( lv_problem_folder_path ).


      " Save a generic single problem data xlsx in a problem folder

      me->save_problem_exp_txt_to_fs( is_problem = <fs_problem_export> ip_problem_file_path_and_name = lv_problem_file_path_and_name ).




    endloop.




  endmethod.



  method set_problem_exp_folder.

    try.

        mv_problems_export_folder = mo_active_configuration->get_parameter_value( 'PROBLEMS_XLSX_EXPORT_FOLDER' ).

      catch ycx_slpm_configuration_exc.

        return.

    endtry.

  endmethod.

  method set_file_system.

    mo_file_system = new ycl_file_system(  ).

  endmethod.


  method compose_problem_attributes.

    ep_problem_folder_path = |{ mv_problems_export_folder }| && |{ is_problem-objectid }|.

    ep_problem_file_path_and_name = ep_problem_folder_path && |\\| && |{ is_problem-objectid }.txt|.

  endmethod.

  method save_problem_exp_txt_to_fs.

    types: begin of ty_prob_data,
             field type string,
             value type string,
           end of ty_prob_data.

    constants lc_new_line  type c length 2
             value cl_abap_char_utilities=>cr_lf.

    data: lt_prob_data           type table of ty_prob_data,
          lv_problem_data_string type string,
          lv_problem_text_string type string.

    lt_prob_data = value #(

    ( field = 'Номер' value = is_problem-data-objectid )
    ( field = 'Описание/тема' value =  is_problem-data-description )
    ( field = 'Статус' value = is_problem-data-statustext )
    ( field = 'Приоритет' value = is_problem-data-prioritytext )
    ( field = 'Создал оператор или Заказчик' value = switch #( is_problem-data-createdinternally
                          when abap_true then 'Оператор'
                          else 'Заказчик' ) )
    ( field = 'Вид сервиса' value = |{ is_problem-data-productname } / { is_problem-data-producttext } | )

    ( field = 'Приоритет виден Заказчику' value = switch #( is_problem-data-showpriorities
                          when abap_true then 'Да'
                          else 'Нет' ) )
    ( field = 'Система' value = |{ is_problem-data-sapsystemname } { is_problem-data-sapsystemdescription } { is_problem-data-sapsystemrole }| )
    ( field = 'Дата проблемы' value = ycl_assistant_utilities=>format_date( ip_format = 'DD.MM.YYYY' ip_date = is_problem-data-postingdate ) )
    ( field = 'Дата и время создания' value = ycl_assistant_utilities=>format_timestamp( is_problem-data-created_at ) )
    ( field = 'Полное время обработки' value = is_problem-data-totalproctimeminutes )
    ( field = 'Дата и время изменения' value =  ycl_assistant_utilities=>format_timestamp( is_problem-data-changedat ) )
    ( field = 'Номер делового партнера компании' value = is_problem-data-companybusinesspartner  )
    ( field = 'Название компании' value = is_problem-data-companyname )
    ( field = 'Номер делового партнера автора' value = is_problem-data-requestorbusinesspartner )
    ( field = 'Имя автора' value = is_problem-data-requestorfullname )
    ( field = 'Взять в работу до (план)' value = ycl_assistant_utilities=>format_timestamp( is_problem-data-irt_timestamp ) )
    ( field = 'Статус IRT' value = |{ is_problem-data-irt_perc }%| )
    ( field = 'Срок исполнения' value = ycl_assistant_utilities=>format_timestamp( is_problem-data-mpt_timestamp ) )
    ( field = 'Статус MPT' value = |{ is_problem-data-mpt_perc }%| )
    ( field = 'Контактный адрес электронной почты' value = is_problem-data-contactemail )
    ( field = 'Использовать контактный адрес электронной почты для уведомлений при изменении статуса этой проблемы'
        value = switch #( is_problem-data-notifybycontactemail
                          when abap_true then 'Да'
                          else 'Нет' ) )
    ( field = 'Время взятия проблемы в работу (факт)' value =
        |{ ycl_assistant_utilities=>format_time( is_problem-data-firstreactiontime ) } { ycl_assistant_utilities=>format_date( ip_format = 'DD.MM.YYYY' ip_date = is_problem-data-firstreactiondate ) }| )

    ).


    " Filling a generic data

    loop at lt_prob_data assigning field-symbol(<ls_prob_data>).

      lv_problem_data_string = |{ lv_problem_data_string }| && |{ <ls_prob_data>-field }: | && |{  <ls_prob_data>-value }| && lc_new_line.

    endloop.

    " Filling text data

    loop at is_problem-texts assigning field-symbol(<ls_text>).

      lv_problem_text_string = |{ lv_problem_text_string }| &&
        |{ <ls_text>-tdid_txt } / { ycl_assistant_utilities=>format_timestamp( <ls_text>-date_time_text ) } / { <ls_text>-user_text }: | && lc_new_line &&
        |{ <ls_text>-text }| && lc_new_line && lc_new_line .

    endloop.



    mv_problems_export_txt = |{ lv_problem_data_string }| && lc_new_line && lc_new_line && |{ lv_problem_text_string }|.

    me->save_problem_exp_txt( ip_problem_file_path_and_name ).

  endmethod.

  method save_problem_exp_txt.

    mo_file_system->save_text_to_fs(
    ip_file_contents_text = mv_problems_export_txt
    ip_file_path_and_name = ip_export_file_path_and_name ).

  endmethod.

  method create_problem_folder.

    mo_file_system->create_folder( ip_folder_path ).

  endmethod.

endclass.
