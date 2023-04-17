class YCX_ASSISTANT_UTILITIES_EXC definition
  public
  inheriting from cx_static_check
  create public .

  public section.

    interfaces if_t100_message .

    constants:
      begin of cant_get_user_data,
        msgid type symsgid value 'YASSISTANT_UTILITIES',
        msgno type symsgno value '000',
        attr1 type scx_attrname value 'MV_USER',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of cant_get_user_data .
    constants:
      begin of not_valid_user,
        msgid type symsgid value 'YASSISTANT_UTILITIES',
        msgno type symsgno value '001',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of not_valid_user .
    constants:
      begin of no_timezone_customizing,
        msgid type symsgid value 'YASSISTANT_UTILITIES',
        msgno type symsgno value '002',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of no_timezone_customizing .
    constants:
      begin of incorrect_guid_format,
        msgid type symsgid value 'YASSISTANT_UTILITIES',
        msgno type symsgno value '003',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of incorrect_guid_format .
    constants:
      begin of unknown_conversion_format,
        msgid type symsgid value 'ZASSISTANT_UTILITIES',
        msgno type symsgno value '004',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of unknown_conversion_format .
    constants:
      begin of org_unit_bp_not_found,
        msgid type symsgid value 'YASSISTANT_UTILITIES',
        msgno type symsgno value '005',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of org_unit_bp_not_found .
    constants:
      begin of user_emails_not_found,
        msgid type symsgid value 'YASSISTANT_UTILITIES',
        msgno type symsgno value '006',
        attr1 type scx_attrname value 'MV_USER',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of user_emails_not_found .
    constants:
      begin of cant_get_http_parameter_value,
        msgid type symsgid value 'YASSISTANT_UTILITIES',
        msgno type symsgno value '007',
        attr1 type scx_attrname value 'MV_PARAMETER',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of cant_get_http_parameter_value .
    constants:
      begin of cant_get_bp_name,
        msgid type symsgid value 'YASSISTANT_UTILITIES',
        msgno type symsgno value '008',
        attr1 type scx_attrname value 'MV_BP',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of cant_get_bp_name .

    data mv_user type xubname .
    data mv_parameter type char100 .
    data mv_bp type bu_partner .

    methods constructor
      importing
        !textid       like if_t100_message=>t100key optional
        !previous     like previous optional
        !ip_user      type xubname optional
        !ip_parameter type char100 optional
        !ip_bp        type bu_partner optional .
  protected section.
  private section.
endclass.



class YCX_ASSISTANT_UTILITIES_EXC implementation.


  method constructor ##ADT_SUPPRESS_GENERATION.
    call method super->constructor
      exporting
        previous = previous.

    me->mv_user = ip_user .
    me->mv_parameter = ip_parameter .
    me->mv_bp = ip_bp .

    clear me->textid.
    if textid is initial.
      if_t100_message~t100key = if_t100_message=>default_textid.
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.
endclass.
