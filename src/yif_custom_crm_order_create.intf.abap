interface yif_custom_crm_order_create
  public .

  methods: create_with_std_and_cust_flds
    importing
      ir_entity type ref to data
    exporting
      ep_guid   type crmt_object_guid
    raising
      ycx_crm_order_api_exc,

    create_attachment
      importing
        ip_guid      type crmt_object_guid
        ip_file_name type string
        ip_mime_type type string
        ip_content   type xstring
      exporting
        es_loio      type skwf_io
        es_phio      type skwf_io
      raising
        ycx_crm_order_api_exc,

    create_text
      importing
        ip_guid type crmt_object_guid
        ip_text type string
        ip_tdid type tdid
      raising
        ycx_crm_order_api_exc.

endinterface.
