class ycl_crm_service_product definition
  public
  inheriting from ycl_crm_product
  create public .

  public section.
    interfaces yif_crm_service_product.

    methods constructor
      importing
        ip_guid type comt_product_guid.

  protected section.
  private section.
    data: mt_response_profile_table type crmt_escal_recno_tab,
          mv_res_profile            type srv_escal,
          mt_priorities             type ycrm_order_tt_priorities,
          mv_avail_profile          type srv_serwi.

    methods: get_response_profile
      returning
        value(rp_res_profile) type srv_escal,

      set_response_profile_table,
      set_response_profile,
      set_priorities,
      clear_all_class_data.

endclass.


class ycl_crm_service_product implementation.

  method constructor.

    super->constructor( ip_guid ).

  endmethod.

  method clear_all_class_data.

    clear: mt_response_profile_table,
        mv_res_profile,
        mt_priorities.

  endmethod.

  method get_response_profile.

    if mv_res_profile is initial.
      me->set_response_profile(  ).
    endif.

    rp_res_profile = mv_res_profile.

  endmethod.

  method set_response_profile.

    data: ls_product_data   type comt_product_maintain_api,
          ls_product        type comt_product,
          lt_request_set    type comt_frgtype_id_tab,
          ls_srv            type comt_prod_srv_maintain_api,
          lt_crmm_pr_srvent type crmm_pr_srvent_maint_t.

    ls_product-product_guid = me->yif_crm_product~get_guid( ).
    append 'CRMM_PR_SRVENT' to lt_request_set.

    call function 'COM_PRODUCT_GETDETAIL_API'
      exporting
        is_product      = ls_product
        it_req_settypes = lt_request_set
      importing
        es_product_data = ls_product_data
        es_service_data = ls_srv.

    if ls_srv is not initial.

      move-corresponding ls_srv-crmm_pr_srvent to lt_crmm_pr_srvent.

      loop at lt_crmm_pr_srvent assigning field-symbol(<ls_crmm_pr_srvent>).

        mv_res_profile = <ls_crmm_pr_srvent>-data-srv_escal.
        mv_avail_profile = <ls_crmm_pr_srvent>-data-srv_serwi.

      endloop.

    endif.

  endmethod.

  method set_response_profile_table.

    if mv_res_profile is initial.
      me->set_response_profile(  ).
    endif.

    if mv_res_profile is not initial.

      call function 'CRM_SERVICE_ENT_READ_ESCAL_REC'
        exporting
          iv_escal          = mv_res_profile
        importing
          et_crmt_escal_rec = mt_response_profile_table.

    endif.

  endmethod.

  method yif_crm_service_product~get_resp_profile_table.

    if mt_response_profile_table is initial.

      me->set_response_profile_table(  ).

    endif.

    rt_response_profile_table = mt_response_profile_table.

  endmethod.

  method set_priorities.

    data:
      lt_priorities type ycrm_order_tt_priorities,
      ls_priority   type ycrm_order_ts_priority.

    if mt_response_profile_table is initial.

      me->set_response_profile_table(  ).

    endif.

    loop at mt_response_profile_table assigning field-symbol(<ls_response_profile_table>).

      ls_priority-code = <ls_response_profile_table>-srv_priority.
      append  ls_priority to lt_priorities.

      clear ls_priority.

    endloop.

    sort lt_priorities.

    delete adjacent duplicates from lt_priorities.

    move-corresponding lt_priorities to mt_priorities.


  endmethod.

  method yif_crm_service_product~get_resp_profile_prio_count.

    if mt_priorities is initial.
      me->set_priorities(  ).
    endif.

    rp_prio_count = lines( mt_priorities ).

  endmethod.

  method yif_crm_service_product~get_resp_profile_prio.

    if mt_priorities is initial.
      me->set_priorities(  ).
    endif.

    move-corresponding mt_priorities to rt_priorities.

  endmethod.

  method yif_crm_service_product~get_availability_profile_name.


    if mv_res_profile is initial.
      me->set_response_profile(  ).
    endif.

    rp_profile_name = mv_avail_profile.

  endmethod.

endclass.
