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

class ycl_slpm_prob_exp_dec_docx implementation.

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

      catch ycx_slpm_configuration_exc.

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

    "me->save_problem_exp_docx_binary( ip_problem_file_path_and_name ).

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
