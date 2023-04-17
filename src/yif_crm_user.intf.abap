interface YIF_CRM_USER
  public .

  methods:
    is_auth_to_read_on_proc_type
      importing
        ip_process_type      type crmt_process_type
      returning
        value(rb_authorized) type bool,

    is_auth_to_create_on_proc_type
      importing
        ip_process_type      type crmt_process_type
      returning
        value(rb_authorized) type bool,

    is_auth_to_update_on_proc_type
      importing
        ip_process_type      type crmt_process_type
      returning
        value(rb_authorized) type bool.

endinterface.
