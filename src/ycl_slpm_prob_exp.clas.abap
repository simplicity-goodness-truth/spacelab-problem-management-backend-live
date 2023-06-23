class ycl_slpm_prob_exp definition
  public
  create public .

  public section.

    interfaces yif_slpm_prob_exp .

    methods:

      constructor
        raising
          ycx_slpm_configuration_exc.

  protected section.

    data:
         mo_active_configuration      type ref to yif_slpm_configuration.

  private section.

endclass.

class ycl_slpm_prob_exp implementation.

  method yif_slpm_prob_exp~export_problems.

    data: lo_slpm_data_provider type ref to yif_slpm_data_manager,
          lv_exception_text     type bapi_msg,
          lt_set_filters        type /iwbep/t_mgw_select_option,
          lt_problems           type ycrm_order_tt_sl_problems,
          wa_problem            like line of rt_problems,
          lt_texts              type yif_slpm_prob_exp=>ty_texts,
          lt_attachments        type yif_slpm_prob_exp=>ty_attachments,
          ls_stream             type /iwbep/if_mgw_appl_types=>ty_s_media_resource,
          ls_attachment         type aic_s_attachment_incdnt_odata,
          lv_guid               type crmt_object_guid,
          lv_loio               type string,
          lv_phio               type string,
          wa_attachment         type aic_s_attachment_incdnt_odata.

    try.
        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        lt_problems = lo_slpm_data_provider->get_problems_list( ).

        loop at lt_problems assigning field-symbol(<ls_problem>).

          clear: wa_problem, lt_texts.

          " Generic problem data

          wa_problem-data = <ls_problem>.

          " Problem texts

          lo_slpm_data_provider->get_texts(
            exporting
                ip_guid = <ls_problem>-guid
            importing
                et_texts = lt_texts ).

          wa_problem-texts = lt_texts.

          " Problem attachments

          lo_slpm_data_provider->get_attachments_list(
            exporting
                ip_guid = <ls_problem>-guid
            importing
                et_attachments_list = lt_attachments ).

          loop at lt_attachments assigning field-symbol(<fs_attachment>).

            lv_guid = <fs_attachment>-guid.
            lv_loio = <fs_attachment>-loio_id.
            lv_phio = <fs_attachment>-phio_id.

            lo_slpm_data_provider->get_attachment_content(
                exporting
                    ip_guid = lv_guid
                    ip_loio = lv_loio
                    ip_phio = lv_phio
                importing
                    er_stream = ls_stream
                    er_attachment = ls_attachment ).

            move-corresponding <fs_attachment> to wa_attachment.
            wa_attachment-document = ls_stream-value.

            append wa_attachment to  wa_problem-attachments.

          endloop.

          append wa_problem to rt_problems.

        endloop.

      catch ycx_slpm_data_manager_exc ycx_crm_order_api_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).

    endtry.

  endmethod.

  method constructor.

    mo_active_configuration = new ycl_slpm_configuration(  ).

  endmethod.

endclass.
