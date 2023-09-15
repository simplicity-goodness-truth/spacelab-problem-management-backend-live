class ycl_slpm_service_operations definition
  public
  final
  create public .

  public section.

    interfaces yif_slpm_service_operations .

  protected section.
  private section.

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

endclass.
