class ycl_slpm_product definition
  public
  inheriting from ycl_crm_service_product
  final
  create public .

  public section.

    interfaces yif_slpm_product .
  protected section.
  private section.
endclass.

class ycl_slpm_product implementation.

  method yif_slpm_product~is_show_priority_set.

    data lv_product_id type comt_product_id.

    lv_product_id = me->yif_crm_product~get_id( ).

    select single showpriorities into rt_show_priority_set from yslpm_prod_attr
        where id = lv_product_id.

  endmethod.

endclass.
