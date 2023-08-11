class ycl_organizational_model definition
  public
  final
  create public .

  public section.

    methods constructor
      importing
        ip_root_org_unit type pd_objid_r.

    interfaces yif_organizational_model .
  protected section.
  private section.

    types: ty_positions_users_full type  table of objec.

    data:
      mv_root_org_unit type pd_objid_r.


    methods: get_org_unit_code_and_text
      importing
        ip_org_unit      type pd_objid_r
        ip_otype         type otype
      exporting
        ep_org_unit_code type short_d
        ep_org_unit_text type stext,

      get_bp_of_org_unit
        importing
          ip_org_unit  type pd_objid_r
        returning
          value(ep_bp) type bu_partner,
      get_org_bp_name
        importing
          ip_bp          type bu_partner
        returning
          value(ep_name) type bu_nameor2,
      get_pos_and_users_of_org_unit
        exporting
          et_positions_and_users_short type crmt_orgman_swhactor_tab
          et_positions_and_users_full  type  ty_positions_users_full.
endclass.



class ycl_organizational_model implementation.


  method get_pos_and_users_of_org_unit.

    call function 'RH_STRUC_GET'
      exporting
        act_otype      = 'O'
        act_objid      = mv_root_org_unit
        act_wegid      = 'SAP_SORG'
      tables
        result_tab     = et_positions_and_users_short
        result_objec   = et_positions_and_users_full
      exceptions
        no_plvar_found = 1
        no_entry_found = 2
        others         = 3.

  endmethod.



  method get_bp_of_org_unit.


    select single sobid into ep_bp
      from hrp1001
       where objid = ip_org_unit and
         sclas = 'BP'.


  endmethod.

  method get_org_bp_name.

    if ( ip_bp is not initial ) and ( ip_bp ne '0000000000' ) .

      select single name_org2 into ep_name
        from but000
          where partner eq ip_bp.

      if sy-subrc <> 0.


      endif. " if sy-subrc <> 0

    endif. " if ip_bp is not initial

  endmethod.



  method yif_organizational_model~get_subunits_of_org_unit.

    call function 'RH_STRUC_GET'
      exporting
        act_otype      = 'O'
        act_objid      = mv_root_org_unit
        act_wegid      = 'B002'
      tables
        result_tab     = rt_sub_units
      exceptions
        no_plvar_found = 1
        no_entry_found = 2
        others         = 3.

  endmethod.

  method get_org_unit_code_and_text.

    select single short stext into (ep_org_unit_code, ep_org_unit_text)
        from hrp1000 where objid = ip_org_unit and otype = ip_otype.

  endmethod.

  method constructor.

    mv_root_org_unit = ip_root_org_unit.

  endmethod.

  method yif_organizational_model~get_assigned_pos_of_org_unit.

    data: lt_child_records_short type crmt_orgman_swhactor_tab,
          lt_child_records       type ty_positions_users_full,
          ls_position            type yorg_model_ts_position,
          lv_bp                  type bu_partner,
          lv_org_unit            type pd_objid_r,
          lo_bp_address_book     type ref to yif_contacts_book,
          lv_last_s_position     type pd_objid_r.

    me->get_pos_and_users_of_org_unit(
     importing
        et_positions_and_users_full = lt_child_records
        et_positions_and_users_short =  lt_child_records_short
      ).

    loop at lt_child_records assigning field-symbol(<ls_child_record>).

      ls_position-objid = <ls_child_record>-objid.
      ls_position-otype = <ls_child_record>-otype.

      case <ls_child_record>-otype.

        when 'S'.

          lv_last_s_position = <ls_child_record>-objid.

          if lv_last_s_position ne <ls_child_record>-objid.

            clear ls_position.

          endif.

          me->get_org_unit_code_and_text(
                       exporting
                           ip_org_unit = ls_position-objid
                           ip_otype = ls_position-otype
                       importing
                           ep_org_unit_code = ls_position-short
                           ep_org_unit_text = ls_position-stext ).

        when 'CP'.

          clear:  lv_bp, lv_org_unit.

          lv_org_unit = <ls_child_record>-objid.

          me->get_bp_of_org_unit(
            exporting
              ip_org_unit = lv_org_unit
            receiving
              ep_bp       = lv_bp ).

          if lv_bp is not initial.

            lo_bp_address_book = new ycl_bp_master_data( lv_bp ).

            ls_position-businesspartner = lv_bp.

            ls_position-fullname = lo_bp_address_book->get_full_name( ).

            call function 'CRM_ERMS_FIND_USER_FOR_BP'
              exporting
                ev_bupa_no = lv_bp
              importing
                ev_user_id = ls_position-username.

            append ls_position to rt_positions.

          endif. " if lv_bp is not initial

      endcase.

    endloop.

  endmethod.

endclass.
