class ycl_crm_user definition
  public
  inheriting from ycl_system_user
  final
  create public .

  public section.

    interfaces:
      yif_crm_user.


  protected section.
  private section.

endclass.

class ycl_crm_user implementation.

  method yif_crm_user~is_auth_to_read_on_proc_type.

    authority-check object 'YPRTYR'
        id 'PR_TYPE' field ip_process_type.

    if sy-subrc = 0.

      rb_authorized = abap_true.

    endif.


  endmethod.

  method yif_crm_user~is_auth_to_create_on_proc_type.

    authority-check object 'YPRTYC'
      id 'PR_TYPE' field ip_process_type.

    if sy-subrc = 0.

      rb_authorized = abap_true.

    endif.

  endmethod.

  method yif_crm_user~is_auth_to_update_on_proc_type.

    authority-check object 'YPRTYU'
      id 'PR_TYPE' field ip_process_type.

    if sy-subrc = 0.

      rb_authorized = abap_true.

    endif.

  endmethod.

endclass.
