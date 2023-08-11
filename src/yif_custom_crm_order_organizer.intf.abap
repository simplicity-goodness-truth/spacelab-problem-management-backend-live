interface yif_custom_crm_order_organizer
  public .
  methods is_order_matching_to_filters
    importing
      !ir_entity               type ref to data
      !it_set_filters          type /iwbep/t_mgw_select_option
    changing
      value(cp_include_record) type ac_bool
    raising
      ycx_crm_order_api_exc .

  methods sort_orders
    importing
      !ir_entity       type ref to data
      !it_order        type /iwbep/t_mgw_sorting_order
    returning
      value(er_entity) type ref to data .

endinterface.
