interface YIF_CRM_PRODUCT
  public .

  methods: get_id
    returning
      value(rp_id) type comt_product_id,

    get_name
      returning
        value(rp_name) type comt_prshtextx,

    get_guid
      returning
        value(rp_guid) type comt_product_guid.

endinterface.
