interface yif_slpm_user
  public .

  methods:

    get_slpm_products_of_user
      returning
        value(rt_slpm_products_of_user) type yslpm_tt_products
      raising
        ycx_slpm_configuration_exc,

    get_slpm_companies_bp_of_user
      returning
        value(rt_companies_bp) type crmt_bu_partner_t,

    get_slpm_prime_company_of_user
      returning
        value(rs_company) type yslpm_ts_company,

    is_auth_to_create_on_behalf
      returning
        value(rb_authorized) type bool,

    is_auth_to_view_company
      importing
        ip_company_bp        type bu_partner
      returning
        value(rb_authorized) type bool,

    is_auth_to_crea_company
      importing
        ip_company_bp        type bu_partner
      returning
        value(rb_authorized) type bool,

    is_auth_to_view_product
      importing
        ip_product_id        type comt_product_id
      returning
        value(rb_authorized) type bool,

    is_auth_to_crea_product
      importing
        ip_product_id        type comt_product_id
      returning
        value(rb_authorized) type bool,

    is_auth_to_read_problems
      returning
        value(rb_authorized) type bool,

    is_auth_to_create_problems
      returning
        value(rb_authorized) type bool,

    is_auth_to_update_problems
      returning
        value(rb_authorized) type bool,

    is_auth_to_update_product
      importing
        ip_product_id        type comt_product_id
      returning
        value(rb_authorized) type bool,

    is_auth_to_update_company
      importing
        ip_company_bp        type bu_partner
      returning
        value(rb_authorized) type bool,

    is_auth_for_internal_att
      returning
        value(rb_authorized) type bool,

    is_auth_to_open_dispute_as_pro
      returning
        value(rb_authorized) type bool,

    is_auth_to_clos_dispute_as_pro
      returning
        value(rb_authorized) type bool,

    is_auth_to_open_dispute_as_req
      returning
        value(rb_authorized) type bool,

    is_auth_to_clos_dispute_as_req
      returning
        value(rb_authorized) type bool,

    is_auth_to_view_dispute
      returning
        value(rb_authorized) type bool.

endinterface.
