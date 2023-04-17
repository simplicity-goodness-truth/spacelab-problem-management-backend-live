interface yif_system_user
  public .

  methods: get_businesspartner
    returning
      value(rp_businesspartner) type bu_partner,

    get_fullname
      returning
        value(rp_fullname) type ad_namecpl
      raising
        ycx_system_user_exc.

endinterface.
