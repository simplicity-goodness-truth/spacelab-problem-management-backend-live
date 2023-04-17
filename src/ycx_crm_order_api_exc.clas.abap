class ycx_crm_order_api_exc definition
  public
  inheriting from cx_static_check
  create public .

  public section.

    interfaces if_t100_message .

    constants:
      begin of cant_get_document_by_guid,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '000',
        attr1 type scx_attrname value 'MV_GUID',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of cant_get_document_by_guid .
    constants:
      begin of cant_get_guid_of_new_instance,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '001',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of cant_get_guid_of_new_instance .
    constants:
      begin of process_type_not_set,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '002',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of process_type_not_set .
    constants:
      begin of error_saving_document,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '004',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of error_saving_document .
    constants:
      begin of standard_crm_api_internal_err,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '003',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of standard_crm_api_internal_err .
    constants:
      begin of database_access_error,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '005',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of database_access_error .
    constants:
      begin of error_saving_cust_flds_of_doc,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '006',
        attr1 type scx_attrname value '',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of error_saving_cust_flds_of_doc .
    constants:
      begin of not_authorized_for_read,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '007',
        attr1 type scx_attrname value 'MV_USER',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of not_authorized_for_read .
    constants:
      begin of not_authorized_for_init,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '008',
        attr1 type scx_attrname value 'MV_USER',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of not_authorized_for_init .
    constants:
      begin of not_authorized_for_create,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '009',
        attr1 type scx_attrname value 'MV_USER',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of not_authorized_for_create .
    constants:
      begin of not_authorized_for_update,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '010',
        attr1 type scx_attrname value 'MV_USER',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of not_authorized_for_update .
    constants:
      begin of cant_get_texts_by_guid,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '011',
        attr1 type scx_attrname value 'MV_GUID',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of cant_get_texts_by_guid .
    constants:
      begin of internal_error,
        msgid type symsgid value 'YCRM_ORDER',
        msgno type symsgno value '012',
        attr1 type scx_attrname value 'MV_ERROR_MESSAGE',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of internal_error .
    class-data mv_guid type crmt_object_guid .
    class-data mv_user type xubname .
    class-data mv_error_message type string .

    methods constructor
      importing
        textid           like if_t100_message=>t100key optional
        previous         like previous optional
        ip_guid          type crmt_object_guid optional
        ip_user          type xubname optional
        ip_error_message type string optional.
  protected section.
  private section.
endclass.



class ycx_crm_order_api_exc implementation.


  method constructor ##ADT_SUPPRESS_GENERATION.

    call method super->constructor
      exporting
        previous = previous.

    me->mv_guid = ip_guid.
    me->mv_user = ip_user.
    me->mv_error_message = ip_error_message.

    clear me->textid.
    if textid is initial.
      if_t100_message~t100key = if_t100_message=>default_textid.
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.
endclass.
