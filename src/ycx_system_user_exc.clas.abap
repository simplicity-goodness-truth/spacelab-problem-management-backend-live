class ycx_system_user_exc definition
  public
  inheriting from cx_static_check
  create public .

  public section.

    interfaces if_t100_message .

    constants:
      begin of bp_not_found_for_user,
        msgid type symsgid value 'YSYSTEM_USER',
        msgno type symsgno value '001',
        attr1 type scx_attrname value 'MV_USERNAME',
        attr2 type scx_attrname value '',
        attr3 type scx_attrname value '',
        attr4 type scx_attrname value '',
      end of bp_not_found_for_user .
    data mv_username type xubname .

    methods constructor
      importing
        textid      like if_t100_message=>t100key optional
        previous    like previous optional
        ip_username type xubname optional .
  protected section.
  private section.
endclass.



class ycx_system_user_exc implementation.

  method constructor ##ADT_SUPPRESS_GENERATION.
    call method super->constructor
      exporting
        previous = previous.
    me->mv_username = ip_username .
    clear me->textid.
    if textid is initial.
      if_t100_message~t100key = if_t100_message=>default_textid.
    else.
      if_t100_message~t100key = textid.
    endif.
  endmethod.
endclass.
