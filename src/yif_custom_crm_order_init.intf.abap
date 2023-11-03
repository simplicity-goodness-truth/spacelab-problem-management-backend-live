interface yif_custom_crm_order_init
  public .

  methods: set_status_profile
    importing
      ip_status_profile type crm_j_stsma,

    set_db_struct_fields_map
      importing
        it_db_struct_fields_map type ycrm_order_tt_cust_fields_map,

    set_custom_fields_db_table
      importing
        ip_custom_fields_db_table type tabname16,

    set_process_type
      importing
        ip_process_type type crmt_process_type,

    set_structure_name
      importing
        ip_structure_name type strukname optional,

    set_sold_to_party
      importing
        ip_sold_to_party type crmt_partner_no,

    set_crm_category1
      importing
        ip_crm_category1 type crm_erms_cat_as_id,

    set_crm_category2
      importing
        ip_crm_category2 type crm_erms_cat_as_id,

    set_crm_category3
      importing
        ip_crm_category3 type crm_erms_cat_as_id,

    set_crm_category4
      importing
        ip_crm_category4 type crm_erms_cat_as_id,

    set_crm_cat_schema
      importing
        ip_crm_cat_schema type crm_erms_cat_as_id.

endinterface.
