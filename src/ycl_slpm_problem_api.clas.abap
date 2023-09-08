class YCL_SLPM_PROBLEM_API definition
  public
  inheriting from ycl_custom_crm_order_api_proxy

  create public .

  public section.
    methods constructor
      importing
        io_active_configuration type ref to yif_slpm_configuration
optional
      raising
        ycx_crm_order_api_exc
        ycx_system_user_exc
        ycx_slpm_configuration_exc.

  protected section.
  private section.
endclass.

class YCL_SLPM_PROBLEM_API implementation.

  method constructor.

    data:
      lt_db_struct_fields_map type ycrm_order_tt_cust_fields_map,
      lv_sold_to_party        type crmt_partner_no.

    super->constructor( 'YSLP' ).

    " Process type

    yif_custom_crm_order_init~set_process_type( 'YSLP' ).

    " Status profile

    yif_custom_crm_order_init~set_status_profile( 'YSLP0001' ).


    " Custom fields table

    yif_custom_crm_order_init~set_custom_fields_db_table( 'CRMD_CUSTOMER_H' ).

    " Structure name

    yif_custom_crm_order_init~set_structure_name( 'YCRM_ORDER_TS_SL_PROBLEM' ).


    " Adding custom fields mapping

    lt_db_struct_fields_map = value ycrm_order_tt_cust_fields_map(
          ( db_field = 'YYFLD000000' struct_field = 'COMPANYBUSINESSPARTNER' )
          ( db_field = 'YYFLD000001' struct_field = 'CONTACTEMAIL' )
          ( db_field = 'YYFLD000002' struct_field = 'NOTIFYBYCONTACTEMAIL' )
          ( db_field = 'YYFLD000003' struct_field = 'SAPSYSTEMNAME' )
        ).

    yif_custom_crm_order_init~set_db_struct_fields_map(
        lt_db_struct_fields_map ).

    " Sold-to-party

*    lv_sold_to_party = io_active_configuration->get_parameter_value( 'DEFAULT_SOLD_TO_PARTY' ).
*
*    yif_custom_crm_order_init~set_sold_to_party( lv_sold_to_party ).

  endmethod.


endclass.
