interface YIF_CONTACTS_BOOK
  public .

  methods: get_full_name
    returning
      value(rp_full_name) type string,

    get_email_address
      returning
        value(rp_email_address) type string.

endinterface.
