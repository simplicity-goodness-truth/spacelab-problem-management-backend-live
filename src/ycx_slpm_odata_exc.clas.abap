class ycx_slpm_odata_exc definition
  public
  inheriting from cx_static_check
  final
  create public .

  public section.

    interfaces if_t100_message .

    constants:
      begin of guid_not_provided_for_entity,
        msgid type symsgid value 'YSLPM_ODATA',
        msgno type symsgno value '000',
        attr1 type scx_attrname value 'MV_ENTITY',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of guid_not_provided_for_entity .
    constants:
      begin of filter_not_provided_for_entity,
        msgid type symsgid value 'YSLPM_ODATA',
        msgno type symsgno value '001',
        attr1 type scx_attrname value 'MV_ENTITY',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of filter_not_provided_for_entity .
    class-data mv_entity type string .

    methods constructor
      importing
        !textid    like if_t100_message=>t100key optional
        !previous  like previous optional
        !mv_entity type string optional .
  protected section.
  private section.
endclass.



class ycx_slpm_odata_exc implementation.


  method constructor ##ADT_SUPPRESS_GENERATION.
    call method super->constructor
      exporting
        previous = previous.
    me->mv_entity = mv_entity .
    clear me->textid.
    if textid is initial.
      if_t100_message~t100key = if_t100_message=>default_textid.
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.
endclass.
