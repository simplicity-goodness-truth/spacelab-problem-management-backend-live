class ycl_slpm_service_operations definition
  public
  final
  create public .

  public section.

    interfaces yif_slpm_service_operations .

  protected section.
  private section.

    data:
       mo_password          type string value 'veYlJeW&C6'.


endclass.

class ycl_slpm_service_operations implementation.

  method yif_slpm_service_operations~clear_attachments_trash_bin.

    data lo_custom_crm_order_att_trash type ref to yif_custom_crm_order_att_trash.

    lo_custom_crm_order_att_trash = new ycl_custom_crm_order_att_trash(  ).

    lo_custom_crm_order_att_trash->empty_trash_bin( ).

  endmethod.

  method yif_slpm_service_operations~clear_problems_history.

    data lo_slpm_problem_history_store type ref to yif_slpm_problem_history_store.

    lo_slpm_problem_history_store = new ycl_slpm_problem_history_store( '00000000000000000000000000000000' ).

    lo_slpm_problem_history_store->arch_orphaned_history_records( ).

    lo_slpm_problem_history_store->delete_arch_history_records( ip_password ).

  endmethod.

  method yif_slpm_service_operations~clear_attachments_vsblty_table.

    delete from yslpm_att_vsbl.

  endmethod.

  method yif_slpm_service_operations~clear_escalation_log.

    data lo_custom_crm_order_sla_escal type ref to ycl_custom_crm_order_sla_escal.

    lo_custom_crm_order_sla_escal = new ycl_custom_crm_order_sla_escal(  ).

    lo_custom_crm_order_sla_escal->yif_custom_crm_order_sla_escal~clear_escal_log( 'YSLP' ).

  endmethod.

  method yif_slpm_service_operations~archive_irt_history.

    data:
      lo_system_user          type ref to yif_system_user,
      lo_slpm_user            type ref to yif_slpm_user,
      lo_active_configuration type ref to yif_slpm_configuration,
      lo_slpm_problem_api     type ref to ycl_slpm_problem_api,
      lt_problems_guids       type ycrm_order_tt_guids,
      lt_orphaned_guids       type table of crmt_object_guid,
      lt_irt_history_store    type ycrm_order_tt_guids.


    lo_slpm_user = new ycl_slpm_user( sy-uname ).

    lo_system_user ?= lo_slpm_user.

    if lo_slpm_user->is_auth_to_read_problems(  ) eq abap_true.

      lo_active_configuration = new ycl_slpm_configuration(  ).

      lo_slpm_problem_api = new ycl_slpm_problem_api( lo_active_configuration ).

      lt_problems_guids      = lo_slpm_problem_api->yif_custom_crm_order_read~get_guids_list(  ).

      select problemguid
        into table lt_irt_history_store
          from yslpm_irt_hist
          where archived is null
          or archived eq ''.

      loop at lt_irt_history_store assigning field-symbol(<ls_history_rec>).

        if not line_exists( lt_problems_guids[ guid = <ls_history_rec>-guid ] ).

          update yslpm_irt_hist set archived = 'X'
              where problemguid = <ls_history_rec>-guid.

        endif.

      endloop.


    else.

      " User has no authorizations to read problems

      raise exception type ycx_slpm_data_manager_exc
        exporting
          textid         = ycx_slpm_data_manager_exc=>not_authorized_for_read
          ip_system_user = sy-uname.

    endif.

  endmethod.

  method yif_slpm_service_operations~clear_irt_history.

    data lt_archived_his_records type table of sysuuid_x16.

    select
     guid
        into table lt_archived_his_records
            from yslpm_irt_hist
                where archived eq 'X'.

    if ip_password eq mo_password. " Just a safety protection for db records deletion

      loop at lt_archived_his_records assigning field-symbol(<ls_archived_his_record>).

        delete from yslpm_irt_hist where guid eq <ls_archived_his_record>.

      endloop.

    endif.

  endmethod.

  method yif_slpm_service_operations~archive_mpt_history.

    data:
      lo_system_user          type ref to yif_system_user,
      lo_slpm_user            type ref to yif_slpm_user,
      lo_active_configuration type ref to yif_slpm_configuration,
      lo_slpm_problem_api     type ref to ycl_slpm_problem_api,
      lt_problems_guids       type ycrm_order_tt_guids,
      lt_orphaned_guids       type table of crmt_object_guid,
      lt_irt_history_store    type ycrm_order_tt_guids.


    lo_slpm_user = new ycl_slpm_user( sy-uname ).

    lo_system_user ?= lo_slpm_user.

    if lo_slpm_user->is_auth_to_read_problems(  ) eq abap_true.

      lo_active_configuration = new ycl_slpm_configuration(  ).

      lo_slpm_problem_api = new ycl_slpm_problem_api( lo_active_configuration ).

      lt_problems_guids      = lo_slpm_problem_api->yif_custom_crm_order_read~get_guids_list(  ).

      select problemguid
        into table lt_irt_history_store
          from yslpm_mpt_hist
          where archived is null
          or archived eq ''.

      loop at lt_irt_history_store assigning field-symbol(<ls_history_rec>).

        if not line_exists( lt_problems_guids[ guid = <ls_history_rec>-guid ] ).

          update yslpm_mpt_hist set archived = 'X'
              where problemguid = <ls_history_rec>-guid.

        endif.

      endloop.


    else.

      " User has no authorizations to read problems

      raise exception type ycx_slpm_data_manager_exc
        exporting
          textid         = ycx_slpm_data_manager_exc=>not_authorized_for_read
          ip_system_user = sy-uname.

    endif.

  endmethod.

  method yif_slpm_service_operations~clear_mpt_history.

    data lt_archived_his_records type table of sysuuid_x16.

    select
     guid
        into table lt_archived_his_records
            from yslpm_mpt_hist
                where archived eq 'X'.

    if ip_password eq mo_password. " Just a safety protection for db records deletion

      loop at lt_archived_his_records assigning field-symbol(<ls_archived_his_record>).

        delete from yslpm_mpt_hist where guid eq <ls_archived_his_record>.

      endloop.

    endif.

  endmethod.

  method yif_slpm_service_operations~archive_dispute_history.

    data:
      lo_system_user          type ref to yif_system_user,
      lo_slpm_user            type ref to yif_slpm_user,
      lo_active_configuration type ref to yif_slpm_configuration,
      lo_slpm_problem_api     type ref to ycl_slpm_problem_api,
      lt_problems_guids       type ycrm_order_tt_guids,
      lt_orphaned_guids       type table of crmt_object_guid,
      lt_dispute_store        type ycrm_order_tt_guids.

    lo_slpm_user = new ycl_slpm_user( sy-uname ).

    lo_system_user ?= lo_slpm_user.

    if lo_slpm_user->is_auth_to_read_problems(  ) eq abap_true.

      lo_active_configuration = new ycl_slpm_configuration(  ).

      lo_slpm_problem_api = new ycl_slpm_problem_api( lo_active_configuration ).

      lt_problems_guids      = lo_slpm_problem_api->yif_custom_crm_order_read~get_guids_list(  ).

      select problemguid
        into table lt_dispute_store
          from yslpm_pr_dispute
          where archived is null
          or archived eq ''.

      loop at lt_dispute_store assigning field-symbol(<fs_dispute_record>).

        if not line_exists( lt_problems_guids[ guid = <fs_dispute_record>-guid ] ).

          update yslpm_pr_dispute set archived = 'X'
              where problemguid = <fs_dispute_record>-guid.

        endif.

      endloop.

    else.

      " User has no authorizations to read problems

      raise exception type ycx_slpm_data_manager_exc
        exporting
          textid         = ycx_slpm_data_manager_exc=>not_authorized_for_read
          ip_system_user = sy-uname.

    endif.

  endmethod.

  method yif_slpm_service_operations~clear_dispute_history.

    data lt_archived_dispute_records type table of sysuuid_x16.

    select guid
        into table lt_archived_dispute_records
        from yslpm_pr_dispute
        where archived eq 'X'.

    if ip_password eq mo_password. " Just a safety protection for db records deletion

      loop at lt_archived_dispute_records assigning field-symbol(<fs_archived_dispute_record>).

        delete from yslpm_pr_dispute where guid eq <fs_archived_dispute_record>.

      endloop.

    endif.

  endmethod.

endclass.
