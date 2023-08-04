class ycl_custom_crm_order_att_trash definition
  public
  final
  create public .

  public section.

    interfaces: yif_custom_crm_order_att_trash .

    methods: constructor
      importing
        ip_guid         type crmt_object_guid optional
        ip_process_type type crmt_process_type optional.

  protected section.

  private section.

    data:
      mv_guid         type crmt_object_guid,
      mv_process_type type crmt_process_type.

    methods:

      get_object_id_by_guid
        returning
          value(rp_object_id) type crmt_object_id_db,

      create_att_for_crm_order
        importing
          ip_file_name type string
          ip_mime_type type w3conttype
          ip_content   type xstring,

      delete_trash_bin_record
        importing
          ip_guid type sysuuid_x16.

endclass.


class ycl_custom_crm_order_att_trash implementation.

  method constructor.

    mv_guid = ip_guid.
    mv_process_type = ip_process_type.

  endmethod.


  method get_object_id_by_guid.

    if mv_guid is not initial.

      select single object_id
          from crmd_orderadm_h
          into rp_object_id
          where guid = mv_guid.

    endif.

  endmethod.


  method yif_custom_crm_order_att_trash~put_att_to_trash_bin.

    data wa_ycrmo_att_trash type ycrmo_att_trash.

    if ( mv_guid is not initial ) and ( mv_process_type is not initial ) .

      wa_ycrmo_att_trash-guid = ycl_assistant_utilities=>generate_x16_guid(  ).
      wa_ycrmo_att_trash-refguid = mv_guid.
      wa_ycrmo_att_trash-content = ip_content.
      wa_ycrmo_att_trash-processtype = mv_process_type.
      wa_ycrmo_att_trash-filename = ip_file_name.
      wa_ycrmo_att_trash-objectid = me->get_object_id_by_guid(  ).
      wa_ycrmo_att_trash-deletiondate = sy-datum.
      wa_ycrmo_att_trash-deletiontime = sy-uzeit.
      wa_ycrmo_att_trash-deletedby = sy-uname.
      wa_ycrmo_att_trash-mimetype = ip_mime_type.

      insert ycrmo_att_trash from wa_ycrmo_att_trash.

    endif.

  endmethod.

  method yif_custom_crm_order_att_trash~put_att_back_to_crm_order.


    data ls_trash_bin_record type ycrmo_att_trash.

    ls_trash_bin_record = me->yif_custom_crm_order_att_trash~get_trash_bin_record( ip_guid ).

    mv_guid = ls_trash_bin_record-refguid.
    mv_process_type = ls_trash_bin_record-processtype.

    me->create_att_for_crm_order(
        ip_content = ls_trash_bin_record-content
        ip_file_name = ls_trash_bin_record-filename
        ip_mime_type = ls_trash_bin_record-mimetype ).

    " Removing a recovered attachment record from a trash bin

    me->delete_trash_bin_record( ip_guid ).

  endmethod.

  method create_att_for_crm_order.

    data:
      lo_cl_ags_crm_1o_api type ref to cl_ags_crm_1o_api,
      ls_loio              type skwf_io,
      ls_phio              type skwf_io,
      lv_mime_type         type string.

    call method cl_ags_crm_1o_api=>get_instance
      exporting
        iv_header_guid                = mv_guid
        iv_process_mode               = 'C'
        iv_process_type               = mv_process_type
      importing
        eo_instance                   = lo_cl_ags_crm_1o_api
      exceptions
        invalid_parameter_combination = 1
        error_occurred                = 2
        others                        = 3.

    if sy-subrc eq 0.

      lv_mime_type = ip_mime_type.

      lo_cl_ags_crm_1o_api->add_attachment(
      exporting
        iv_doc_content  = ip_content
        iv_file_name    = ip_file_name
        iv_content_type = lv_mime_type
      importing
        es_loio         = ls_loio
        es_phio         = ls_phio
        es_error        = data(ls_error)
    ).

    endif.


  endmethod.



  method yif_custom_crm_order_att_trash~get_trash_bin_record.

    select single
      mandt
      guid
      refguid
      objectid
      deletedby
      filename
      mimetype
      processtype
      deletiondate
      deletiontime
      content
  from ycrmo_att_trash
  into rs_att_trash_bin
  where guid = ip_guid.

  endmethod.

  method delete_trash_bin_record.

    delete from ycrmo_att_trash
      where guid = ip_guid.

  endmethod.

  method yif_custom_crm_order_att_trash~empty_trash_bin.

    delete from ycrmo_att_trash.

  endmethod.

endclass.
