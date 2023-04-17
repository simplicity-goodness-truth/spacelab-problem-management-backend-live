interface yif_standard_text
  public.

  methods: get_compiled_text_by_name
    importing
      ip_use_tags             type abap_bool optional
      it_variables_values     type yst_text_tt_variables_values
    returning
      value(rp_compiled_text) type string,

    get_raw_text_by_name
      returning
        value(rp_raw_text) type string.

endinterface.
