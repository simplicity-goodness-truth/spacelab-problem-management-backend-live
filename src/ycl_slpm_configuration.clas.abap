class YCL_SLPM_CONFIGURATION definition
  public
  final
  create public .

  public section.

    interfaces yif_slpm_configuration .

    methods constructor
      raising
        ycx_slpm_configuration_exc.

  protected section.
  private section.

    data: mv_active_profile       type char50,
          mt_active_configuration type table of yslpm_setup.

    methods: set_active_profile
      raising
        ycx_slpm_configuration_exc,

      get_value_from_db
        importing
          ip_param_name   type char50
        returning
          value(ep_value) type text200
        raising
          ycx_slpm_configuration_exc,

      set_active_configuration
        raising
          ycx_slpm_configuration_exc,

      check_active_profile
        raising
          ycx_slpm_configuration_exc,

      get_value_from_active_config
        importing
          ip_param_name   type char50
        returning
          value(ep_value) type text200
        raising
          ycx_slpm_configuration_exc,

      get_values_from_db_by_mask
        importing
          ip_param_name_mask   type char50
        returning
          value(rt_parameters) type  yslpm_tt_setup_records
        raising
          ycx_slpm_configuration_exc
        .

endclass.

class YCL_SLPM_CONFIGURATION implementation.

  method yif_slpm_configuration~get_parameter_value.

    data: lv_param_name   type char50.

    lv_param_name = |{ mv_active_profile }| && |.| && |{ ip_param_name }|.

    rp_value = me->get_value_from_active_config( lv_param_name ).

  endmethod.

  method set_active_profile.

    mv_active_profile = me->get_value_from_db( 'ACTIVE_PROFILE' ).

  endmethod.

  method get_value_from_db.

    select single value from yslpm_setup
     into ep_value
       where param eq ip_param_name.

    if sy-subrc ne 0.

      raise exception type ycx_slpm_configuration_exc
        exporting
          textid       = ycx_slpm_configuration_exc=>parameter_not_found
          ip_parameter = ip_param_name.

    endif.

  endmethod.

  method constructor.

    me->set_active_profile(  ).
    me->set_active_configuration(  ).

  endmethod.

  method set_active_configuration.

    data lv_parameter_mask type char50.

    me->check_active_profile(  ).

    lv_parameter_mask = |{ mv_active_profile }| && |.| && |%|.

    select param value from yslpm_setup
        into corresponding fields of table mt_active_configuration
        where param like lv_parameter_mask.

  endmethod.

  method check_active_profile.

    if mv_active_profile is initial.

      raise exception type ycx_slpm_configuration_exc
        exporting
          textid = ycx_slpm_configuration_exc=>active_profile_not_set.

    endif.

  endmethod.

  method get_value_from_active_config.

    data: ls_parameter_line type yslpm_setup,
          lv_param          type char50.

    try.
        lv_param = ip_param_name.

        translate lv_param to upper case.

        ls_parameter_line = mt_active_configuration[ param = lv_param ].

        ep_value = ls_parameter_line-value.

      catch cx_sy_itab_line_not_found.
        raise exception type ycx_slpm_configuration_exc
          exporting
            textid       = ycx_slpm_configuration_exc=>parameter_not_found
            ip_parameter = ip_param_name.

    endtry.

  endmethod.

  method yif_slpm_configuration~get_parameters_values_by_mask.

    data: lv_param_mask   type char50.

    lv_param_mask = |{ mv_active_profile }| && |.| && |%| && |{ ip_param_name_mask }|  && |%|.

    rt_parameters = me->get_values_from_db_by_mask( lv_param_mask ).

  endmethod.

  method get_values_from_db_by_mask.

    select param value from yslpm_setup
     into table rt_parameters
       where param like ip_param_name_mask.

  endmethod.

endclass.
