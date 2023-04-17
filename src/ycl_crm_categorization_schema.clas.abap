class YCL_CRM_CATEGORIZATION_SCHEMA definition
  public
  final
  create public .

  public section.
    interfaces yif_crm_categorization_schema .

    methods constructor
      importing
        ip_asp_guid type crm_erms_cat_guid optional
        ip_asp_id   type crm_erms_cat_ca_id optional.

  protected section.
  private section.

    data:
      mv_asp_guid  type crm_erms_cat_guid,
      mv_asp_id    type crm_erms_cat_ca_id,
      mt_hierarchy type ycrm_tt_cat_schema_hierarchy.

    methods set_hierarchy.

    methods: get_cat_id
      importing
        ip_cat_guid      type crm_erms_cat_guid
      returning
        value(rp_cat_id) type crm_erms_cat_ca_id,
      set_asp_guid
        importing
          ip_asp_guid type crm_erms_cat_guid,
      set_asp_id
        importing
          ip_asp_id type crm_erms_cat_ca_id,
      set_asp_guid_by_asp_id
        importing
          ip_asp_id type crm_erms_cat_ca_id ,
      set_asp_id_by_asp_guid
        importing
          ip_asp_guid type crm_erms_cat_guid,
      get_node_label
        importing
          ip_asp_guid     type crm_erms_cat_guid
        returning
          value(rp_label) type crm_erms_cat_ca_desc.

endclass.

class YCL_CRM_CATEGORIZATION_SCHEMA implementation.

  method constructor.

    if ip_asp_guid is not initial.

      me->set_asp_guid( ip_asp_guid ).
      me->set_asp_id_by_asp_guid( ip_asp_guid ).

    else.

      if ip_asp_id is not initial.

        me->set_asp_id( ip_asp_id ).
        me->set_asp_guid_by_asp_id( ip_asp_id ).

      endif.

    endif.


    me->set_hierarchy(  ).

  endmethod.

  method set_asp_id_by_asp_guid.

    select single asp_id into mv_asp_id from crmc_erms_cat_as
    where asp_guid = ip_asp_guid.

  endmethod.

  method set_asp_guid_by_asp_id.

    select single asp_guid into mv_asp_guid from crmc_erms_cat_as
      where asp_id = ip_asp_id.

  endmethod.


  method set_asp_guid.

    me->mv_asp_guid = ip_asp_guid.

  endmethod.

  method set_asp_id.

    me->mv_asp_id = ip_asp_id.

  endmethod.

  method set_hierarchy.

    data:
      lt_hierarchy  type crmt_erms_cat_hi_update_ttype,
      ls_cat_schema type  ycrm_ts_cat_schema_hierarchy.

    if mv_asp_guid is initial.
      return.
    endif.

    call method cl_crm_erms_cat_hi_db=>get_kids
      exporting
        iv_tree_guid        = mv_asp_guid
        iv_tree_type        = 'CAT'
        iv_level            = 4
      importing
        et_hierarchy        = lt_hierarchy
      exceptions
        aspect_not_found    = 1
        hierarchy_not_found = 2
        others              = 3.

    loop at lt_hierarchy assigning field-symbol(<ls_hierarchy>).

      clear: ls_cat_schema.

      ls_cat_schema-node_guid = <ls_hierarchy>-node_guid.
      ls_cat_schema-pare_guid = <ls_hierarchy>-pare_guid.
      ls_cat_schema-node_level = <ls_hierarchy>-level.
      ls_cat_schema-cat_name = me->get_node_label( <ls_hierarchy>-node_guid ).
      ls_cat_schema-cat_id = me->get_cat_id( <ls_hierarchy>-node_guid ).

      append ls_cat_schema to me->mt_hierarchy.

    endloop.

  endmethod.

  method get_cat_id.

    select single cat_id into rp_cat_id from
       crmc_erms_cat_ca where cat_guid = ip_cat_guid.

  endmethod.


  method yif_crm_categorization_schema~get_hierarchy.

    rt_hierarchy = me->mt_hierarchy.

  endmethod.

  method yif_crm_categorization_schema~get_asp_label.

    select single asp_desc into rp_label from crmc_erms_cat_at
        where asp_guid = mv_asp_guid.

  endmethod.

  method get_node_label.

    data: ls_category type        crmt_erms_cat_ca_lang,
          lr_category type ref to if_crm_erms_catego_category.

    call method cl_crm_erms_catego_ma_default=>if_crm_erms_catego_manager~get_category
      exporting
        iv_cat_guid = ip_asp_guid
      importing
        ev_instance = lr_category.

    if lr_category is bound.
      call method lr_category->get_details
        importing
          ev_cat_lang = ls_category.

      rp_label = ls_category-cat_labl.

    endif.

  endmethod.

  method yif_crm_categorization_schema~get_hierarchy_cat_label.

    rp_label = mt_hierarchy[ cat_id = ip_cat_id ]-cat_name.

  endmethod.

endclass.
