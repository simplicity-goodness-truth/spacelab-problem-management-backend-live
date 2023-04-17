class YCX_SLPM_DATA_MANAGER_EXC definition
  public
  inheriting from cx_static_check
  create public .

  public section.

    interfaces if_t100_message .

    constants:
      begin of not_authorized_for_read,
        msgid type symsgid value 'YSLPM_DATA_MANAGER',
        msgno type symsgno value '001',
        attr1 type scx_attrname value 'MV_SYSTEM_USER',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of not_authorized_for_read .
    constants:
      begin of internal_error,
        msgid type symsgid value 'YSLPM_DATA_MANAGER',
        msgno type symsgno value '002',
        attr1 type scx_attrname value 'MV_ERROR_MESSAGE',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of internal_error .
    constants:
      begin of no_auth_for_creat_for_company,
        msgid type symsgid value 'YSLPM_DATA_MANAGER',
        msgno type symsgno value '004',
        attr1 type scx_attrname value 'MV_SYSTEM_USER',
        attr2 type scx_attrname value 'MV_COMPANY_BP',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of no_auth_for_creat_for_company .
    constants:
      begin of no_auth_for_creat_for_prod,
        msgid type symsgid value 'YSLPM_DATA_MANAGER',
        msgno type symsgno value '006',
        attr1 type scx_attrname value 'MV_SYSTEM_USER',
        attr2 type scx_attrname value 'MV_PRODUCT_ID',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of no_auth_for_creat_for_prod .
    constants:
      begin of not_authorized_for_create,
        msgid type symsgid value 'YSLPM_DATA_MANAGER',
        msgno type symsgno value '007',
        attr1 type scx_attrname value 'MV_SYSTEM_USER',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of not_authorized_for_create .
    constants:
      begin of not_authorized_for_update,
        msgid type symsgid value 'YSLPM_DATA_MANAGER',
        msgno type symsgno value '008',
        attr1 type scx_attrname value 'MV_SYSTEM_USER',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of not_authorized_for_update .
    class-data mv_system_user type xubname .
    class-data mv_error_message type string .
    class-data mv_company_bp type bu_partner .
    class-data mv_product_id type comt_product_id .

    methods constructor
      importing
        !textid           like if_t100_message=>t100key optional
        !previous         like previous optional
        !ip_system_user   type xubname optional
        !ip_error_message type string optional
        !ip_company_bp    type bu_partner optional
        !ip_product_id    type comt_product_id optional .
  protected section.
  private section.
endclass.



class YCX_SLPM_DATA_MANAGER_EXC implementation.


  method constructor ##ADT_SUPPRESS_GENERATION.
    call method super->constructor
      exporting
        previous = previous.
    me->mv_system_user = ip_system_user .
    me->mv_error_message = ip_error_message.
    me->mv_company_bp = ip_company_bp.
    me->mv_product_id = ip_product_id.
    clear me->textid.
    if textid is initial.
      if_t100_message~t100key = if_t100_message=>default_textid.
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.
endclass.
