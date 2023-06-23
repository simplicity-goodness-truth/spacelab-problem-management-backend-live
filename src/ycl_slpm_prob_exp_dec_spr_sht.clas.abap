class ycl_slpm_prob_exp_dec_spr_sht definition
  public
  inheriting from ycl_slpm_prob_exp_dec
  final
  create public .

  public section.

    methods:

      yif_slpm_prob_exp~export_problems redefinition.

  protected section.

  private section.

    constants:
               lc_typekind_string type abap_typekind value cl_abap_typedescr=>typekind_string.

    data:

      mt_problems_export             type yif_slpm_prob_exp=>ty_problems,
      mt_problems_export_summary     type ycrm_order_tt_sl_problems,
      mv_problems_export_xlsx_binary type xstringval,
      mv_problems_export_sum_binary  type xstringval,
      mv_problems_export_folder      type string value 'C:\tmp\',
      mo_file_system                 type ref to yif_file_system.

    methods:

      save_problems_exp_data_to_fs,

      save_problem_exp_xlsx_to_fs
        importing
          is_problem                    type yif_slpm_prob_exp=>ty_problem
          ip_problem_file_path_and_name type string,

      save_problem_exp_xlsx_binary
        importing
          ip_export_file_path_and_name type string,

      save_problem_exp_sum_binary
        importing
          ip_export_file_path_and_name type string,

      set_problem_exp_folder
        raising
          ycx_slpm_configuration_exc,

      create_problem_folder
        importing
          ip_folder_path type string,

      compose_problem_attributes
        importing
          is_problem                    type ycrm_order_ts_sl_problem
        exporting
          ep_problem_folder_path        type string
          ep_problem_file_path_and_name type string,

      save_problem_attachments_on_fs
        importing
          ip_problem_folder_path type string
          it_attachments         type yif_slpm_prob_exp=>ty_attachments,

      set_file_system,

      save_problem_exp_sumary_to_fs,

      set_all_fcat_columns_to_string
        changing
          ct_field_catalog type zexcel_t_fieldcatalog,

      get_arial10_style_guid
        importing
          io_excel                     type ref to zcl_excel
        returning
          value(rp_style_arial10_guid) type zexcel_cell_style,

      set_all_fcat_col_wdth_and_name
        importing
          it_field_catalog type zexcel_t_fieldcatalog
          io_worksheet     type ref to zcl_excel_worksheet.

endclass.

class ycl_slpm_prob_exp_dec_spr_sht implementation.

  method yif_slpm_prob_exp~export_problems .

    if it_problems_export is initial.

      mt_problems_export = mo_slpm_prob_exp->yif_slpm_prob_exp~export_problems(  ).

    else.

      mt_problems_export = it_problems_export.

    endif.

    me->set_file_system(  ).

    me->set_problem_exp_folder(  ).

    me->save_problems_exp_data_to_fs(  ).

    rt_problems = mt_problems_export.


  endmethod.



  method save_problem_exp_xlsx_binary.

    mo_file_system->save_binary_to_fs(
      ip_file_contents_binary = mv_problems_export_xlsx_binary
      ip_file_path_and_name = ip_export_file_path_and_name ).

  endmethod.

  method save_problems_exp_data_to_fs.

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

      me->save_problem_exp_xlsx_to_fs( is_problem = <fs_problem_export> ip_problem_file_path_and_name = lv_problem_file_path_and_name ).

      " Save single problem attachments in a dedicated folder

      if <fs_problem_export>-attachments is not initial.

        me->save_problem_attachments_on_fs( it_attachments = <fs_problem_export>-attachments ip_problem_folder_path = lv_problem_folder_path ).

      endif.

      " Appending a new record into a summary table

      append <fs_problem_export>-data to mt_problems_export_summary.

    endloop.

    " Saving a summary

    me->save_problem_exp_sumary_to_fs(  ).



  endmethod.

  method save_problem_exp_xlsx_to_fs.


    data: lo_excel                type ref to zcl_excel,
          lo_worksheet            type ref to zcl_excel_worksheet,
          ls_table_settings       type zexcel_s_table_settings,
          cl_writer               type ref to zif_excel_writer,
          lt_problem_data_export  type ycrm_order_tt_sl_problems,
          lt_problem_texts_export type yif_slpm_prob_exp=>ty_texts,
          lt_field_catalog        type zexcel_t_fieldcatalog.

    append is_problem-data to lt_problem_data_export.

    lt_problem_texts_export = is_problem-texts .

    try.

        " Creates active sheet

        create object lo_excel.

        " Adding style

        lo_excel->set_default_style( me->get_arial10_style_guid( lo_excel ) ).

        " Table settings

        ls_table_settings-table_style       = zcl_excel_table=>builtinstyle_medium2.
        ls_table_settings-show_row_stripes  = abap_true.
        ls_table_settings-nofilters         = abap_true.

        " Problem properties tab

        lo_worksheet = lo_excel->get_active_worksheet( ).
        lo_worksheet->set_title( ip_title = 'Основные данные' ).

        lt_field_catalog = zcl_excel_common=>get_fieldcatalog( ip_table = lt_problem_data_export ).

        " Converting all cells into STRING format

        me->set_all_fcat_columns_to_string(
            changing
                ct_field_catalog = lt_field_catalog ).


        lo_worksheet->bind_table( ip_table  = lt_problem_data_export
                          is_table_settings = ls_table_settings
                          it_field_catalog = lt_field_catalog
                                     ).

        " Setting header names from a field catalogue and column width

        me->set_all_fcat_col_wdth_and_name( io_worksheet = lo_worksheet it_field_catalog = lt_field_catalog ).

        " Communication tab

        lo_worksheet = lo_excel->add_new_worksheet( ).

        lo_worksheet->set_title( ip_title = 'Коммуникация' ).

        lt_field_catalog = zcl_excel_common=>get_fieldcatalog( ip_table = lt_problem_texts_export ).

        " Converting all cells into STRING format

        me->set_all_fcat_columns_to_string(
            changing
                ct_field_catalog = lt_field_catalog ).

        lo_worksheet->bind_table( ip_table  = lt_problem_texts_export
                          is_table_settings = ls_table_settings
                          it_field_catalog = lt_field_catalog
                          ).

        " Setting header names from a field catalogue and column width

        me->set_all_fcat_col_wdth_and_name( io_worksheet = lo_worksheet it_field_catalog = lt_field_catalog ).

        " Preparing binary file

        cl_writer = new zcl_excel_writer_2007( ).

        mv_problems_export_xlsx_binary = cl_writer->write_file( lo_excel ).

        " Saving binary of a xls on a file system

        me->save_problem_exp_xlsx_binary( ip_problem_file_path_and_name ).

      catch zcx_excel.

    endtry.


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

  method create_problem_folder.

    mo_file_system->create_folder( ip_folder_path ).

  endmethod.

  method compose_problem_attributes.

    ep_problem_folder_path = |{ mv_problems_export_folder }| && |{ is_problem-objectid }|.

    ep_problem_file_path_and_name = ep_problem_folder_path && |\\| && |data.xlsx|.

  endmethod.

  method save_problem_attachments_on_fs.

    data lv_file_path_and_name type string.

    loop at it_attachments assigning field-symbol(<fs_attachment>).

      lv_file_path_and_name = |{ ip_problem_folder_path }| && |\\| && |{ <fs_attachment>-name }|.

      mo_file_system->save_binary_to_fs(
        ip_file_contents_binary = <fs_attachment>-document
        ip_file_path_and_name = lv_file_path_and_name ).

    endloop.

  endmethod.

  method save_problem_exp_sumary_to_fs.

    if mt_problems_export_summary is not initial.

      data: lo_excel              type ref to zcl_excel,
            lo_worksheet          type ref to zcl_excel_worksheet,
            ls_table_settings     type zexcel_s_table_settings,
            cl_writer             type ref to zif_excel_writer,
            lv_file_path_and_name type string,
            lt_field_catalog      type zexcel_t_fieldcatalog.

      try.

          " Creates active sheet

          create object lo_excel.

          " Adding style

          lo_excel->set_default_style( me->get_arial10_style_guid( lo_excel ) ).

          " Table settings

          ls_table_settings-table_style       = zcl_excel_table=>builtinstyle_medium2.
          ls_table_settings-show_row_stripes  = abap_true.
          ls_table_settings-nofilters         = abap_true.

          " Problem properties tab

          lo_worksheet = lo_excel->get_active_worksheet( ).
          lo_worksheet->set_title( ip_title = 'Список проблем' ).

          lt_field_catalog = zcl_excel_common=>get_fieldcatalog( ip_table = mt_problems_export_summary ).

          " Converting all cells into STRING format

          me->set_all_fcat_columns_to_string(
              changing
                  ct_field_catalog = lt_field_catalog ).

          lo_worksheet->bind_table( ip_table  = mt_problems_export_summary
                            is_table_settings = ls_table_settings
                            it_field_catalog = lt_field_catalog
                            ).

          " Setting header names from a field catalogue and column width

          me->set_all_fcat_col_wdth_and_name( io_worksheet = lo_worksheet it_field_catalog = lt_field_catalog ).

          " Preparing binary file

          cl_writer = new zcl_excel_writer_2007( ).

          mv_problems_export_sum_binary = cl_writer->write_file( lo_excel ).

          " Preparing a filename

          lv_file_path_and_name = |{ mv_problems_export_folder }| && |\\| && |problemsSummary.xlsx|.

          " Saving binary of a xls on a file system

          me->save_problem_exp_sum_binary( lv_file_path_and_name ).

        catch zcx_excel.

      endtry.



    endif.

  endmethod.

  method save_problem_exp_sum_binary.

    mo_file_system->save_binary_to_fs(
     ip_file_contents_binary = mv_problems_export_sum_binary
     ip_file_path_and_name = ip_export_file_path_and_name ).

  endmethod.

  method set_all_fcat_columns_to_string.

    loop at ct_field_catalog assigning field-symbol(<fs_field_catalog>).

      " Converting all cells into STRING format

      <fs_field_catalog>-abap_type = lc_typekind_string.


    endloop.

  endmethod.

  method get_arial10_style_guid.

    data lo_style_arial10 type ref to zcl_excel_style.

    lo_style_arial10                      = io_excel->add_new_style( ).
    lo_style_arial10->font->name          = zcl_excel_style_font=>c_name_arial.
    lo_style_arial10->font->scheme        = zcl_excel_style_font=>c_scheme_none.
    lo_style_arial10->font->size          = 10.
    lo_style_arial10->alignment->wraptext = abap_true.

    rp_style_arial10_guid             = lo_style_arial10->get_guid( ).

  endmethod.

  method set_all_fcat_col_wdth_and_name.

    data: lo_column           type ref to zcl_excel_column,
          lv_tabix            type i,
          lv_fieldname_length type i.

    try.
        loop at it_field_catalog assigning field-symbol(<fs_field_catalog>).

          " Setting names of columns from a field catalogue

          lv_tabix = sy-tabix.

          io_worksheet->set_cell(
            ip_row = 1
            ip_column = lv_tabix
            ip_value = <fs_field_catalog>-fieldname
            ).

          lo_column = io_worksheet->get_column(  lv_tabix ).

          " Setting a size of each column

          lv_fieldname_length = strlen( <fs_field_catalog>-fieldname ) + 8.

          lo_column->set_width( lv_fieldname_length ).

        endloop.

      catch zcx_excel.

    endtry.


  endmethod.

endclass.
