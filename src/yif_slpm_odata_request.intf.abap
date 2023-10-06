interface YIF_SLPM_ODATA_REQUEST
  public .

  methods:
    is_client_secure
      returning
        value(rp_secure) type abap_bool
      raising
        ycx_slpm_configuration_exc
        ycx_slpm_data_manager_exc.

endinterface.
