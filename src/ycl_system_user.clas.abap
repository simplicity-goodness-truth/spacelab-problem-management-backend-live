class YCL_SYSTEM_USER definition
  public
  create public .

  public section.
    methods constructor
      importing
        ip_username type xubname
      raising
        ycx_system_user_exc.

    interfaces yif_system_user .
  protected section.
  private section.

    data mv_username type xubname .
    data mv_lastname type ad_namelas .
    data mv_firstname type ad_namefir .
    data mv_fullname type ad_namecpl .
    data mv_businesspartner type bu_partner .

    methods: set_businesspartner
      raising
        ycx_system_user_exc,

      set_last_and_first_names,

      set_fullname
        raising
          ycx_system_user_exc,

      set_username
        importing
          ip_username type xubname .
endclass.

class YCL_SYSTEM_USER implementation.

  method yif_system_user~get_businesspartner.

    rp_businesspartner = me->mv_businesspartner.

  endmethod.

  method constructor.

    me->set_username( ip_username ).

    me->set_businesspartner( ).



  endmethod.

  method set_businesspartner.

    call function 'CRM_ERMS_FIND_BP_FOR_USER'
      exporting
        iv_user_id = me->mv_username
      importing
        ev_bupa_no = me->mv_businesspartner.

    if ( sy-subrc ne 0 ) or ( me->mv_businesspartner is initial ).

      raise exception type ycx_system_user_exc
        exporting
          textid      = ycx_system_user_exc=>bp_not_found_for_user
          ip_username = me->mv_username.

    endif.

    shift me->mv_businesspartner left deleting leading '0'.

  endmethod.

  method set_username.

    me->mv_username = ip_username.

  endmethod.

  method set_fullname.

    me->mv_fullname = |{ mv_lastname }| && | | && |{ mv_firstname }|.

  endmethod.

  method set_last_and_first_names.

    select single name_last name_first into ( mv_lastname, mv_firstname
)
      from user_addr
        where bname eq mv_username.

  endmethod.

  method yif_system_user~get_fullname.

    me->set_last_and_first_names( ).

    me->set_fullname(  ).

    rp_fullname = mv_fullname.

  endmethod.

endclass.
