class YCX_SLPM_ODATA_EXC definition
  public
  inheriting from CX_STATIC_CHECK
  final
  create public .

public section.

  interfaces IF_T100_MESSAGE .

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
  constants:
    begin of CLIENT_NOT_SUPPORTED,
      msgid type symsgid value 'YSLPM_ODATA',
      msgno type symsgno value '002',
      attr1 type scx_attrname value '',
      attr2 type scx_attrname value '',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of CLIENT_NOT_SUPPORTED .
  class-data MV_ENTITY type STRING .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !MV_ENTITY type STRING optional .
  protected section.
  private section.
ENDCLASS.



CLASS YCX_SLPM_ODATA_EXC IMPLEMENTATION.


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
ENDCLASS.
