class YCX_SLPM_DATA_MANAGER_EXC definition
  public
  inheriting from CX_STATIC_CHECK
  create public .

public section.

  interfaces IF_T100_MESSAGE .

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
  constants:
    begin of NO_AUTH_FOR_UPDATE_FOR_COMPANY,
      msgid type symsgid value 'YSLPM_DATA_MANAGER',
      msgno type symsgno value '009',
      attr1 type scx_attrname value 'MV_SYSTEM_USER',
      attr2 type scx_attrname value 'MV_COMPANY_BP',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_AUTH_FOR_UPDATE_FOR_COMPANY .
  constants:
    begin of NO_AUTH_FOR_UPDATE_FOR_PROD,
      msgid type symsgid value 'YSLPM_DATA_MANAGER',
      msgno type symsgno value '010',
      attr1 type scx_attrname value 'MV_SYSTEM_USER',
      attr2 type scx_attrname value 'MV_PRODUCT_ID',
      attr3 type scx_attrname value '',
      attr4 type scx_attrname value '',
    end of NO_AUTH_FOR_UPDATE_FOR_PROD .
  class-data MV_SYSTEM_USER type XUBNAME .
  class-data MV_ERROR_MESSAGE type STRING .
  class-data MV_COMPANY_BP type BU_PARTNER .
  class-data MV_PRODUCT_ID type COMT_PRODUCT_ID .

  methods CONSTRUCTOR
    importing
      !TEXTID like IF_T100_MESSAGE=>T100KEY optional
      !PREVIOUS like PREVIOUS optional
      !IP_SYSTEM_USER type XUBNAME optional
      !IP_ERROR_MESSAGE type STRING optional
      !IP_COMPANY_BP type BU_PARTNER optional
      !IP_PRODUCT_ID type COMT_PRODUCT_ID optional .
  protected section.
  private section.
ENDCLASS.



CLASS YCX_SLPM_DATA_MANAGER_EXC IMPLEMENTATION.


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
ENDCLASS.
