class ycl_organizational_model definition
  public
  final
  create public .

  public section.

    methods constructor
      importing
        ip_root_org_unit type pd_objid_r optional.

    interfaces yif_organizational_model .
  protected section.
  private section.

    types:
      ty_positions_users_full  type  table of objec,
      ty_positions_users_struc type  table of struc.

    data:
      mv_root_org_unit type pd_objid_r.


    methods:

      get_pos_and_users_of_org_unit
        exporting
          et_positions_and_users_short type crmt_orgman_swhactor_tab
          et_positions_and_users_full  type  ty_positions_users_full
          et_positions_and_users_struc type  ty_positions_users_struc,

      get_o_type_upper_4_levels
        importing
          it_positions_and_users_struc type  ty_positions_users_struc
        changing
          cs_position                  type yorg_model_ts_position.

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
        result_struc   = et_positions_and_users_struc
      exceptions
        no_plvar_found = 1
        no_entry_found = 2
        others         = 3.

  endmethod.



  method yif_organizational_model~get_bp_of_org_unit.

*    select single sobid into ep_bp
*      from hrp1001
*       where objid = ip_org_unit and
*         sclas = 'BP'.

    " If there are several BPs exist in HRP1001, then
    " we need to take the oldest one (where begin date is higher)

    select sobid up to 1 rows into ep_bp
        from hrp1001
            where objid = ip_org_unit and
            sclas = 'BP'
            order by begda descending.

    endselect.

  endmethod.

  method yif_organizational_model~get_org_bp_name.

    if ( ip_bp is not initial ) and ( ip_bp ne '0000000000' ) .

      select single name_org2 into ep_name
        from but000
          where partner eq ip_bp.

      if sy-subrc <> 0.


      endif. " if sy-subrc <> 0

    endif. " if ip_bp is not initial

  endmethod.



  method yif_organizational_model~get_subunits_of_root_org_unit.

    call function 'RH_STRUC_GET'
      exporting
        act_otype      = 'O'
        act_objid      = mv_root_org_unit
        act_wegid      = 'B002'
      tables
        result_tab     = et_sub_units
        result_struc   = et_sub_units_struc
      exceptions
        no_plvar_found = 1
        no_entry_found = 2
        others         = 3.

  endmethod.

  method yif_organizational_model~get_org_unit_code_and_text.

    select single short stext into (ep_org_unit_code, ep_org_unit_text)
        from hrp1000
            where objid = ip_org_unit
            and otype = ip_otype
            and begda le sy-datum
            and endda ge sy-datum.

  endmethod.

  method constructor.

    mv_root_org_unit = ip_root_org_unit.

  endmethod.

  method yif_organizational_model~get_assig_pos_of_root_org_unit.

    data: lt_child_records_short type crmt_orgman_swhactor_tab,
          lt_child_records       type ty_positions_users_full,
          lt_child_records_struc type ty_positions_users_struc,
          ls_position            type yorg_model_ts_position,
          lv_bp                  type bu_partner,
          lv_org_unit            type pd_objid_r,
          lo_bp_address_book     type ref to yif_contacts_book,
          lv_last_s_position     type pd_objid_r.

    me->get_pos_and_users_of_org_unit(
     importing
        et_positions_and_users_full = lt_child_records
        et_positions_and_users_short =  lt_child_records_short
        et_positions_and_users_struc = lt_child_records_struc
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

          me->yif_organizational_model~get_org_unit_code_and_text(
                       exporting
                           ip_org_unit = ls_position-objid
                           ip_otype = ls_position-otype
                       importing
                           ep_org_unit_code = ls_position-short
                           ep_org_unit_text = ls_position-stext ).

          if ip_get_upper_levels eq abap_true.

            me->get_o_type_upper_4_levels(
              exporting
                  it_positions_and_users_struc = lt_child_records_struc
              changing
                  cs_position = ls_position
              ).

          endif.

        when 'CP'.

          clear:  lv_bp, lv_org_unit.

          lv_org_unit = <ls_child_record>-objid.

          me->yif_organizational_model~get_bp_of_org_unit(
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

  method get_o_type_upper_4_levels.

     types: begin of  ty_found_levels,
             objid type pd_objid_r,
             otype type otype,
           end of ty_found_levels.

    data:
      lv_field_name          type string,
      lv_search_complete     type abap_bool,
      lv_next_upper_position type integer,
      lt_found_levels        type table of ty_found_levels,
      wa_found_levels        type ty_found_levels,
      lv_org_unit_short_name type short_d,
      lv_org_unit_text       type stext.

    field-symbols:
      <field> type any,
      <value> type any.

    " Filling all parent records of an org.unit

    try.

        lv_next_upper_position = it_positions_and_users_struc[ objid = cs_position-objid otype = cs_position-otype ]-pup.

        while lv_search_complete eq abap_false.

          wa_found_levels-objid = it_positions_and_users_struc[ seqnr = lv_next_upper_position ]-objid.
          wa_found_levels-otype = it_positions_and_users_struc[ seqnr = lv_next_upper_position ]-otype.

          append wa_found_levels to lt_found_levels.

          if lv_next_upper_position ne 0.

            lv_next_upper_position = it_positions_and_users_struc[ seqnr = lv_next_upper_position ]-pup.

          else.

            lv_search_complete = abap_true.

          endif.

        endwhile.

      catch cx_sy_itab_line_not_found.

    endtry.

    " Only 4 levels up are supported

    do 4 times.

      " Setting org. unit code

      lv_field_name = |LEVEL| & |{ sy-index }| & |OOBJID|.

      if <field> is assigned.
        unassign <field>.
      endif.

      try.

          if lt_found_levels[ lines( lt_found_levels ) - sy-index + 1 ]  is not initial.

            assign lt_found_levels[ lines( lt_found_levels ) - sy-index + 1 ]-objid to <value>.

            assign component lv_field_name of structure cs_position to <field>.

            <field> = <value>.

          endif.

        catch cx_sy_itab_line_not_found.

      endtry.

      " Setting org.unit text

      lv_field_name = |LEVEL| & |{ sy-index }| & |OOBJIDTEXT|.

      if <field> is assigned.
        unassign <field>.
      endif.

      try.

          if lt_found_levels[ lines( lt_found_levels ) - sy-index + 1 ]  is not initial.

            me->yif_organizational_model~get_org_unit_code_and_text(
                                exporting
                                    ip_org_unit = lt_found_levels[ lines( lt_found_levels ) - sy-index + 1 ]-objid
                                    ip_otype = lt_found_levels[ lines( lt_found_levels ) - sy-index + 1 ]-otype
                                importing
                                    ep_org_unit_code = lv_org_unit_short_name
                                    ep_org_unit_text = lv_org_unit_text ).

            if lv_org_unit_text is not initial.

              assign lv_org_unit_text to <value>.

              assign component lv_field_name of structure cs_position to <field>.

              <field> = <value>.

            endif.

          endif.

        catch cx_sy_itab_line_not_found.

      endtry.

    enddo.


  endmethod.

  method yif_organizational_model~get_employee_org_unit.

    call function 'RH_STRUC_GET'
      exporting
        act_otype      = 'CP'
        act_objid      = ip_emp_org_unit
        act_wegid      = 'CP_O'
      tables
        result_tab     = rt_org_unit
      exceptions
        no_plvar_found = 1
        no_entry_found = 2
        others         = 3.

  endmethod.

  method yif_organizational_model~get_upper_units_of_org_unit.

    call function 'RH_STRUC_GET'
      exporting
        act_otype      = 'O'
        act_objid      = ip_org_unit
        act_wegid      = 'O-O'
      tables
        result_tab     = et_upper_units
        result_struc   = et_upper_units_struc
      exceptions
        no_plvar_found = 1
        no_entry_found = 2
        others         = 3.



  endmethod.

  method yif_organizational_model~get_org_unit_of_bp.

    select objid up to 1 rows into ep_org_unit
        from hrp1001
        where sobid = ip_bp and
            sclas = 'BP'
        order by begda descending.

    endselect.

  endmethod.

endclass.
