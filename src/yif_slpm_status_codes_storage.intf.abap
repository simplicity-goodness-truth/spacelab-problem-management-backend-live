interface yif_slpm_status_codes_storage
  public .

  methods:
    get_status_codes_record
      importing
        ip_status_codes_record_id     type char64
      returning
        value(ro_status_codes_record) type ref to yif_slpm_status_codes.

endinterface.
