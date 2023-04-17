class ycl_slpm_products_storage definition
  public
  final
  create public .

  public section.
    interfaces:
      yif_slpm_products_storage.

  protected section.
  private section.
endclass.

class ycl_slpm_products_storage implementation.

  method yif_slpm_products_storage~get_all_slpm_products.

    types: begin of ty_product_and_cust,
             productid               type comt_product_id,
             customerbusinesspartner type bu_partner,
           end of ty_product_and_cust.


    data:
      ls_slpm_product type yslpm_ts_product,
      lt_products     type table of ty_product_and_cust.

    select productid customerbusinesspartner
        from yslpm_cust_prod into corresponding fields of table lt_products.

    loop at lt_products assigning field-symbol(<ls_product>).

      select single a~product_guid a~product_id b~short_text
            from (
                comm_product as a inner join
                comm_prshtext as b
                on a~product_guid = b~product_guid )
                into ls_slpm_product
                where
                a~product_id = <ls_product>-productid and
                b~langu = sy-langu.

      ls_slpm_product-companybusinesspartner = <ls_product>-customerbusinesspartner.

      append ls_slpm_product to rt_all_slpm_products.

    endloop.

  endmethod.

endclass.
