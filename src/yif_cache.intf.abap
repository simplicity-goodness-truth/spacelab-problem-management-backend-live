interface yif_cache
  public .

  methods:

    add_record
      importing
        ir_record type ref to data,

    get_record
      importing
        ir_guid          type ref to data
      returning
        value(rs_record) type ref to data,

    invalidate_record
      importing
        ir_guid type ref to data,

    get_all_records
      returning
        value(rt_records) type ref to data.

endinterface.
