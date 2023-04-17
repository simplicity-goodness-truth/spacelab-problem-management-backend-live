class ycx_slpm_configuration_exc definition
  public
  inheriting from cx_static_check
  create public .

  public section.

    interfaces if_t100_message .

    constants:
      begin of parameter_not_found,
        msgid type symsgid value 'YSLPM_CONFIGURATION',
        msgno type symsgno value '001',
        attr1 type scx_attrname value 'MV_PARAMETER',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of parameter_not_found .
    constants:
      begin of active_profile_not_set,
        msgid type symsgid value 'YSLPM_CONFIGURATION',
        msgno type symsgno value '002',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of active_profile_not_set .
    data mv_parameter type char50 .

    methods constructor
      importing
        !textid       like if_t100_message=>t100key optional
        !previous     like previous optional
        !ip_parameter type char50 optional .
  protected section.
  private section.
endclass.



class ycx_slpm_configuration_exc implementation.


  method constructor ##ADT_SUPPRESS_GENERATION.
    call method super->constructor
      exporting
        previous = previous.
    me->mv_parameter = ip_parameter .
    clear me->textid.
    if textid is initial.
      if_t100_message~t100key = if_t100_message=>default_textid.
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.
endclass.
