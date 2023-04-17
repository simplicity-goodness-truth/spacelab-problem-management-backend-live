class ycl_slpm_mpc_ext definition
  public
  inheriting from ycl_slpm_mpc
  create public .

  public section.
    methods define redefinition.
  protected section.
  private section.
endclass.



class ycl_slpm_mpc_ext implementation.

  method define.

    super->define( ).
    data: lo_entity   type ref to /iwbep/if_mgw_odata_entity_typ,
          lo_property type ref to /iwbep/if_mgw_odata_property.
    lo_entity = model->get_entity_type( iv_entity_name = 'Attachment' ).
    if lo_entity is bound.
      lo_property = lo_entity->get_property( iv_property_name = 'Mimetype').
      lo_property->set_as_content_type( ).
    endif.

  endmethod.

endclass.
