interface yif_custom_crm_order_update
  public .

  methods: update_order
    importing
      ir_entity type ref to data
      ip_guid   type crmt_object_guid
    raising
      ycx_crm_order_api_exc,

    delete_attachment
      importing
        ip_guid type crmt_object_guid
        ip_loio type string
        ip_phio type string
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
