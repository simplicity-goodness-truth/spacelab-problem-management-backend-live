class ycl_slpm_problem_processor definition
  public
  final
  create public .

  public section.

    interfaces yif_slpm_problem_processor .

    methods:
      constructor
        importing
          ip_bp type bu_partner
        raising
          ycx_slpm_configuration_exc.

  protected section.
  private section.

    types:
       ty_sub_units_struc type  table of struc.

    data:

      mv_processor_bp         type bu_partner,
      mv_processor_org_unit   type pd_objid_r,
      mo_organizational_model type ref to yif_organizational_model,
      mo_active_configuration type ref to yif_slpm_configuration.

    methods:
      set_processor_bp
        importing
          ip_bp type bu_partner,

      set_processor_org_unit,

      set_organizational_model,

      set_active_configuration
        raising
          ycx_slpm_configuration_exc,

      get_support_team_level
        returning
          value(rp_level) type int1
        raising
          ycx_slpm_configuration_exc.


endclass.



class ycl_slpm_problem_processor implementation.

  method constructor.

    set_processor_bp( ip_bp ).

    set_active_configuration( ).

    set_organizational_model( ).

    set_processor_org_unit( ).

  endmethod.


  method yif_slpm_problem_processor~get_support_team_bp.

    data lv_support_team_org_unit type pd_objid_r.

    lv_support_team_org_unit = me->yif_slpm_problem_processor~get_support_team_org_unit( ).

    rp_bp = mo_organizational_model->get_bp_of_org_unit( lv_support_team_org_unit ).

  endmethod.


  method set_processor_org_unit.

    mv_processor_org_unit = mo_organizational_model->get_org_unit_of_bp( mv_processor_bp ).

  endmethod.

  method set_processor_bp.

    mv_processor_bp = ip_bp.

  endmethod.

  method set_organizational_model.

    mo_organizational_model = new ycl_organizational_model( ).

  endmethod.

  method set_active_configuration.

    mo_active_configuration = new ycl_slpm_configuration( ).

  endmethod.

  method get_support_team_level.

    rp_level = me->mo_active_configuration->get_parameter_value( 'SUPPORT_TEAM_LEVEL_IN_ORG_STRUCTURE' ).

    if ( rp_level is initial ) or ( rp_level eq  0 ).

      rp_level = 2.

    endif.

  endmethod.


  method yif_slpm_problem_processor~get_support_team_org_unit.

    data: lt_processor_org_unit type crmt_orgman_swhactor_tab,
          lt_upper_units_struc  type  ty_sub_units_struc,
          lt_upper_units        type crmt_orgman_swhactor_tab,
          lv_processor_org_unit type pd_objid_r,
          lv_support_team_level type int1.

    " Getting an org. unit of a user

    lt_processor_org_unit = mo_organizational_model->get_employee_org_unit( mv_processor_org_unit ).

    " Getting upper org. units

    try.

        lv_processor_org_unit = lt_processor_org_unit[ 1 ]-objid.

        mo_organizational_model->get_upper_units_of_org_unit(
            exporting
                ip_org_unit = lv_processor_org_unit
            importing
                et_upper_units = lt_upper_units
                et_upper_units_struc = lt_upper_units_struc ).


        " Getting support team level in org.structure

        lv_support_team_level = me->get_support_team_level( ).

        " We have to read from bottom to up, as highest org.structure level is stored at bottom

        sort lt_upper_units_struc by level descending.

        try.

            rp_org_unit = lt_upper_units_struc[ lv_support_team_level ]-objid.

          catch cx_sy_itab_line_not_found.

        endtry.

      catch cx_sy_itab_line_not_found.

    endtry.


  endmethod.

endclass.
