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

    data lt_products type ycrm_tt_products.

    select a~product_guid a~product_id b~short_text
         from (
             comm_product as a inner join
             comm_prshtext as b
             on a~product_guid = b~product_guid )
             into table lt_products
             where a~product_guid = ip_guid and
             b~langu = sy-langu.

    loop at lt_products assigning field-symbol(<ls_product>).

      mv_guid = <ls_product>-guid.
      mv_id = <ls_product>-id.
      mv_name = <ls_product>-name.

    endloop.

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
