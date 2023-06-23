class YCL_SLPM_PROB_EXP_DEC_DOCX definition
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

      mt_problems_export             type yif_slpm_prob_exp=>ty_problems,
      mv_problems_export_docx_binary type xstringval,
      mv_problems_export_folder      type string value 'C:\tmp\',
      mo_file_system                 type ref to yif_file_system.

    methods:

      save_problems_exp_docx_to_fs,

      save_problem_exp_docx_to_fs
        importing
          is_problem                    type yif_slpm_prob_exp=>ty_problem
          ip_problem_file_path_and_name type string,

      save_problem_exp_docx_binary
        importing
          ip_export_file_path_and_name type string,

      set_problem_exp_folder
        raising
          zcx_slpm_configuration_exc,

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

class YCL_SLPM_PROB_EXP_DEC_DOCX implementation.

  method yif_slpm_prob_exp~export_problems .

    if it_problems_export is initial.

      mt_problems_export = mo_slpm_prob_exp->yif_slpm_prob_exp~export_problems(  ).

    else.

      mt_problems_export = it_problems_export.

    endif.

    me->set_file_system(  ).

    me->set_problem_exp_folder(  ).

    me->save_problems_exp_docx_to_fs(  ).


  endmethod.




  method save_problems_exp_docx_to_fs.

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

      me->save_problem_exp_docx_to_fs( is_problem = <fs_problem_export> ip_problem_file_path_and_name = lv_problem_file_path_and_name ).




    endloop.




  endmethod.



  method set_problem_exp_folder.

    try.

        mv_problems_export_folder = mo_active_configuration->get_parameter_value( 'PROBLEMS_XLSX_EXPORT_FOLDER' ).

      catch zcx_slpm_configuration_exc.

        return.

    endtry.

  endmethod.

  method set_file_system.

    mo_file_system = new ycl_file_system(  ).

  endmethod.


  method compose_problem_attributes.

    ep_problem_folder_path = |{ mv_problems_export_folder }| && |{ is_problem-objectid }|.

    ep_problem_file_path_and_name = ep_problem_folder_path && |\\| && |data.docx|.

  endmethod.







  method save_problem_exp_docx_to_fs.

    " ZZZZZZZZZZ SIMPLE DOCX TRYOUT

TYPES
: begin of t_TABLE3
,       PERSON type string
,       SALARY type string
, end of t_TABLE3

, tt_TABLE3 type table of t_TABLE3 with empty key


, begin of t_T2
,       F1 type string
,       F2 type string
,       F3 type string
, end of t_T2

, tt_T2 type table of t_T2 with empty key


, begin of t_T1
,       H1 type string
, T2 type tt_T2
, end of t_T1

, tt_T1 type table of t_T1 with empty key


, begin of t_LINE1
,       FIELD1 type string
,       FIELD2 type string
,       FIELD3 type string
,       FIELD4 type string
, end of t_LINE1

, tt_LINE1 type table of t_LINE1 with empty key


, begin of t_TAB1
,       TITLE1 type string
, LINE1 type tt_LINE1
, end of t_TAB1

, tt_TAB1 type table of t_TAB1 with empty key


, begin of t_LINE2
,       FIELD1 type string
,       FIELD2 type string
,       FIELD3 type string
, end of t_LINE2

, tt_LINE2 type table of t_LINE2 with empty key


, begin of t_TAB2
,       TITLE2 type string
, LINE2 type tt_LINE2
, end of t_TAB2

, tt_TAB2 type table of t_TAB2 with empty key


, begin of t_data
,       USER type string
,       SH01 type string
,       DATE type string
,       TIME type string

, TABLE3 type tt_TABLE3
, T1 type tt_T1
, TAB1 type tt_TAB1
, TAB2 type tt_TAB2
, end of t_data

, tt_data type table of t_data with empty key


.


*
*    types: begin of t_data,
*
*             text type string,
*
*           end of t_data.

    data
          : gs_templ_data type t_data.

  APPEND INITIAL LINE TO gs_templ_data-tab1 ASSIGNING FIELD-SYMBOL(<ls_tab1>).
  <ls_tab1>-title1 = |'Душа моя озарена неземной радостью, как эти чудесные весенние утра, которыми я наслаждаюсь от всего сердца. Я совсем один и блаженствую в здешнем краю, словно созданном для таких, как я'|.



    mv_problems_export_docx_binary = zcl_docx3=>get_document(
        iv_w3objid    = 'ZSLPM_PROB_EXP_0'
        iv_data       = gs_templ_data
        iv_no_save    = 'X'   ).

    me->save_problem_exp_docx_binary( ip_problem_file_path_and_name ).

  endmethod.

  method save_problem_exp_docx_binary.

    mo_file_system->save_binary_to_fs(
    ip_file_contents_binary = mv_problems_export_docx_binary
    ip_file_path_and_name = ip_export_file_path_and_name ).

  endmethod.

  method create_problem_folder.

    mo_file_system->create_folder( ip_folder_path ).

  endmethod.

endclass.
