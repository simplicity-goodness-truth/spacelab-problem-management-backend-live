class ycl_crm_product definition
  public
  create public .

  public section.
    interfaces yif_crm_product .
    methods constructor
      importing
        ip_guid type comt_product_guid.

  protected section.
  private section.
    data:
      mv_guid type   comt_product_guid,
      mv_id   type   comt_product_id,
      mv_name type   comt_prshtextx.

endclass.

class ycl_crm_product implementation.

  method constructor.

    select single  product_guid product_id into ( mv_guid, mv_id )
       from comm_product where product_guid = ip_guid.

    select single short_text into mv_name
        from comm_prshtext where product_guid = ip_guid
        and langu = sy-langu.

  endmethod.

  method yif_crm_product~get_id.
    rp_id = mv_id.
  endmethod.

  method yif_crm_product~get_name.
    rp_name = mv_name.
  endmethod.

  method yif_crm_product~get_guid.
    rp_guid = mv_guid.
  endmethod.

endclass.
