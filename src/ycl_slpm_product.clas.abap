class ycl_slpm_product definition
  public
  inheriting from ycl_crm_service_product
  final
  create public .

  public section.

    interfaces yif_slpm_product .

    methods constructor
      importing
        ip_guid type comt_product_guid.

  protected section.
  private section.

    data ms_product_attrs type yslpm_prod_attr.

    methods: set_product_attrs.

endclass.

class ycl_slpm_product implementation.

  method yif_slpm_product~is_show_priority_set.

    rt_show_priority_set = ms_product_attrs-showpriorities.

*    data lv_product_id type comt_product_id.
*
*    lv_product_id = me->yif_crm_product~get_id( ).
*
*    select single showpriorities into rt_show_priority_set from yslpm_prod_attr
*        where id = lv_product_id.

  endmethod.

  method yif_slpm_product~get_org_unit_for_updates.

    rp_org_unit = ms_product_attrs-statusupdatesorgunit.

  endmethod.


  method set_product_attrs.

    data lv_product_id type comt_product_id.

    if ms_product_attrs is initial.

      lv_product_id = me->yif_crm_product~get_id( ).

      select single
          mandt id description
          showpriorities processingorgunit statusupdatesorgunit statusupdatessignature
          into ms_product_attrs from yslpm_prod_attr
        where id = lv_product_id.

    endif.

  endmethod.

  method constructor.

    super->constructor( ip_guid = ip_guid ).

    me->set_product_attrs( ).

  endmethod.

  method yif_slpm_product~get_signature_for_updates.

    rp_signature = ms_product_attrs-statusupdatessignature.

  endmethod.

  method yif_slpm_product~get_processing_org_unit.

    rp_org_unit = ms_product_attrs-processingorgunit.

  endmethod.

endclass.
