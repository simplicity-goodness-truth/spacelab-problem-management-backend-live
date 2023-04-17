interface yif_slpm_configuration
  public .

  methods: get_parameter_value
    importing
      ip_param_name   type char50
    returning
      value(rp_value) type text200
    raising
      ycx_slpm_configuration_exc,

    get_parameters_values_by_mask
      importing
        ip_param_name_mask   type char50
      returning
        value(rt_parameters) type yslpm_tt_setup_records
      raising
        ycx_slpm_configuration_exc.

endinterface.
