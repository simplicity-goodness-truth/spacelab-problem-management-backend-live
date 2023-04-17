class ycl_slpm_dpc_ext definition
  public
  inheriting from ycl_slpm_dpc
  create public .

  public section.
    methods /iwbep/if_mgw_appl_srv_runtime~get_stream redefinition .
    methods /iwbep/if_mgw_appl_srv_runtime~create_stream redefinition.
    methods /iwbep/if_mgw_appl_srv_runtime~delete_stream redefinition.
  protected section.
    methods slampthistoryset_get_entityset redefinition.
    methods slairthistoryset_get_entityset redefinition.
    methods systemset_get_entityset redefinition.
    methods frontendconfigur_get_entityset redefinition.
    methods companyset_get_entityset redefinition.
    methods processorset_get_entityset redefinition.
    methods systemuserset_get_entityset redefinition.
    methods statusset_get_entityset redefinition.
    methods problemset_update_entity redefinition.

    methods priorityset_get_entityset redefinition.
    methods problemset_create_entity redefinition.
    methods productset_get_entity redefinition.
    methods productset_get_entityset redefinition.
    methods textset_create_entity redefinition.
    methods attachmentset_delete_entity redefinition.
    methods attachmentset_get_entity redefinition.
    methods attachmentset_get_entityset redefinition.
    methods textset_get_entityset redefinition.
    methods problemset_get_entity redefinition.
    methods problemset_get_entityset redefinition.

  private section.
    class-data: mv_exception_postfix type string,
                mv_exception_text    type bapi_msg.

    methods: set_exception_response
      importing
        ip_exception_text type bapi_msg
      returning
        value(ep_msg)     type ref to /iwbep/if_message_container,
      set_exception_text
        importing
          ip_exception_text type string,
      raise_exception
        importing
          ip_exception_text type string
        raising
          /iwbep/cx_mgw_busi_exception,
      get_attachment_keys
        importing
          it_key_tab type /iwbep/t_mgw_name_value_pair
        exporting
          ep_guid    type crmt_object_guid
          ep_loio    type string
          ep_phio    type string,
      get_filter_value
        importing
          !io_tech_request_context type ref to /iwbep/if_mgw_req_entityset
          !ip_property             type string
        returning
          value(ep_value)          type string ,
      get_filter_select_options
        importing
          !ip_property             type string
          !io_tech_request_context type ref to /iwbep/if_mgw_req_entityset
        returning
          value(et_select_options) type /iwbep/t_cod_select_options .

endclass.


class ycl_slpm_dpc_ext implementation.

  method raise_exception.

    set_exception_text( ip_exception_text ).

    raise exception type /iwbep/cx_mgw_busi_exception
      exporting
        message_container = set_exception_response( mv_exception_text ).

  endmethod.

  method set_exception_text.

    mv_exception_text = ip_exception_text.

  endmethod.


  method set_exception_response.

    if mv_exception_text is not initial.

      call method /iwbep/if_mgw_conv_srv_runtime~get_message_container
        receiving
          ro_message_container = ep_msg.

      call method ep_msg->add_message_text_only
        exporting
          iv_msg_type = /iwbep/cl_cos_logger=>error
          iv_msg_text = mv_exception_text.

    endif.

  endmethod.

  method problemset_get_entityset.

    data: lo_slpm_data_provider type ref to yif_slpm_data_manager,
          lv_exception_text     type bapi_msg,
          lt_set_filters        type /iwbep/t_mgw_select_option.

    lt_set_filters = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    try.
        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        et_entityset = lo_slpm_data_provider->get_problems_list(
            exporting
            it_filters = lt_set_filters
            it_order = it_order ).

      catch ycx_slpm_data_manager_exc ycx_crm_order_api_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method problemset_get_entity.

    data: lv_guid               type crmt_object_guid,
          lo_slpm_data_provider type ref to yif_slpm_data_manager.

    try.

        read table it_key_tab into data(ls_key_tab) with key name = 'Guid'.

        lv_guid = ls_key_tab-value.

        if lv_guid is initial.

          raise exception type ycx_slpm_odata_exc
            exporting
              textid    = ycx_slpm_odata_exc=>guid_not_provided_for_entity
              mv_entity = iv_entity_name.

        endif.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        er_entity = lo_slpm_data_provider->get_problem( lv_guid ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.


  method textset_get_entityset.
    data:
      lt_texts              type ycl_slpm_mpc=>tt_text,
      ls_texts              like line of et_entityset,
      lo_slpm_data_provider type ref to yif_slpm_data_manager,
      lv_guid               type crmt_object_guid,
      lt_filter_tdid        type /iwbep/t_cod_select_options.


    lt_filter_tdid = get_filter_select_options( io_tech_request_context = io_tech_request_context
                                              ip_property = 'TDID' ).

    try.

        read table it_key_tab into data(ls_key_tab) with key name = 'Guid'.

        lv_guid = ls_key_tab-value.

        if lv_guid is initial.

          raise exception type ycx_slpm_odata_exc
            exporting
              textid    = ycx_slpm_odata_exc=>guid_not_provided_for_entity
              mv_entity = iv_entity_name.

        endif.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        lo_slpm_data_provider->get_texts(
            exporting ip_guid = lv_guid
            importing et_texts = lt_texts ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.


    loop at lt_texts assigning field-symbol(<ls_text>) where tdid in
lt_filter_tdid.

      ls_texts = <ls_text>.

      append ls_texts to et_entityset.

    endloop. " loop at lt_entityset assigning field-symbol(<ls_entityset>)

  endmethod.

  method attachmentset_get_entityset.

    data: lv_guid               type crmt_object_guid,
          lo_slpm_data_provider type ref to yif_slpm_data_manager.


    read table it_key_tab into data(ls_key_tab) with key name = 'Guid'.

    lv_guid = ls_key_tab-value.

    if lv_guid is not initial.

      try.

          lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

          lo_slpm_data_provider->get_attachments_list( exporting
              ip_guid = lv_guid
              importing
              et_attachments_list = et_entityset ).

        catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
            ycx_assistant_utilities_exc ycx_slpm_configuration_exc
            ycx_system_user_exc into data(lcx_process_exception).
          raise_exception( lcx_process_exception->get_text(  ) ).

      endtry.

    endif.

  endmethod.

  method attachmentset_get_entity.

    data: lv_guid               type crmt_object_guid,
          lv_loio               type string,
          lv_phio               type string,
          lo_slpm_data_provider type ref to yif_slpm_data_manager.

    me->get_attachment_keys(
     exporting
         it_key_tab = it_key_tab
     importing
         ep_guid = lv_guid
         ep_loio = lv_loio
         ep_phio = lv_phio ).

    if ( lv_loio is initial ) or ( lv_phio is initial ) or ( lv_guid is
initial ).
      return.
    endif.

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        er_entity = lo_slpm_data_provider->get_attachment( exporting ip_guid = lv_guid ip_loio = lv_loio ip_phio = lv_phio ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method /iwbep/if_mgw_appl_srv_runtime~get_stream.

    constants: lc_content_dispo_str type string value 'Content-Disposition', "#EC NOTEXT
               lc_content_sec_str   type string value 'Content-Security-Policy', "#EC NOTEXT
               lc_x_content_sec_str type string value 'X-Content-Security-Policy'. "#EC NOTEXT

    data: ls_header             type ihttpnvp,
          ls_stream             type  /iwbep/if_mgw_appl_types=>ty_s_media_resource,
          ls_attachment         type aic_s_attachment_incdnt_odata,
          lv_filename           type string,
          lo_slpm_data_provider type ref to yif_slpm_data_manager,
          lv_guid               type crmt_object_guid,
          lv_loio               type string,
          lv_phio               type string.


    me->get_attachment_keys(
        exporting
            it_key_tab = it_key_tab
        importing
            ep_guid = lv_guid
            ep_loio = lv_loio
            ep_phio = lv_phio ).

    if ( lv_loio is initial ) or ( lv_phio is initial ) or ( lv_guid is initial ).
      return.
    endif.

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        lo_slpm_data_provider->get_attachment_content(
            exporting
                ip_guid = lv_guid
                ip_loio = lv_loio
                ip_phio = lv_phio
            importing
                er_stream = ls_stream
                er_attachment = ls_attachment ).

        ls_header-name = lc_content_dispo_str.
        lv_filename = ls_attachment-name.

        lv_filename = escape( val = lv_filename format =
cl_abap_format=>e_url ).

        ls_header-value = |attachment; filename=| && |{ lv_filename }|.

        set_header( ls_header ).

        call method copy_data_to_ref
          exporting
            is_data = ls_stream
          changing
            cr_data = er_stream.

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method get_attachment_keys.

    data ls_key_tab_itab       type /iwbep/s_mgw_name_value_pair.

    read table it_key_tab with key name = 'LoioId' into ls_key_tab_itab.
    if sy-subrc = 0.
      ep_loio = ls_key_tab_itab-value.
    endif.

    read table it_key_tab with key name = 'PhioId' into ls_key_tab_itab.
    if sy-subrc = 0.
      ep_phio = ls_key_tab_itab-value.
    endif.

    read table it_key_tab with key name = 'refGuid' into ls_key_tab_itab.
    if sy-subrc = 0.
      move ls_key_tab_itab-value to ep_guid.
    endif.

  endmethod.

  method /iwbep/if_mgw_appl_srv_runtime~create_stream.

    data: lv_guid               type crmt_object_guid,
          lv_file_name          type string,
          lv_mime_type          type string,
          lv_content            type xstring,
          lv_dp_facade          type ref to /iwbep/if_mgw_dp_fw_facade,
          lt_request_header     type         tihttpnvp,
          ls_request_header     like line of lt_request_header,
          lo_slpm_data_provider type ref to yif_slpm_data_manager.

    loop at it_key_tab assigning field-symbol(<ls_guid>).
      move <ls_guid>-value to lv_guid.
    endloop.

    lv_dp_facade ?= me->/iwbep/if_mgw_conv_srv_runtime~get_dp_facade( ).

    lt_request_header = lv_dp_facade->/iwbep/if_mgw_dp_int_facade~get_request_header( ).

    read table lt_request_header into ls_request_header with key name = 'slug'.

    if ls_request_header is not initial.
      lv_file_name = ls_request_header-value.
    endif.


    lv_file_name = cl_http_utility=>if_http_utility~unescape_url( lv_file_name ).

    lv_mime_type = is_media_resource-mime_type.
    lv_content = is_media_resource-value.

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        lo_slpm_data_provider->create_attachment(
        exporting
        ip_content = lv_content
        ip_file_name = lv_file_name
        ip_mime_type = lv_mime_type
        ip_guid = lv_guid ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method /iwbep/if_mgw_appl_srv_runtime~delete_stream.

    data: lv_guid               type crmt_object_guid,
          lv_loio               type string,
          lv_phio               type string,
          lo_slpm_data_provider type ref to yif_slpm_data_manager.

    me->get_attachment_keys(
       exporting
           it_key_tab = it_key_tab
       importing
           ep_guid = lv_guid
           ep_loio = lv_loio
           ep_phio = lv_phio ).

    if ( lv_loio is initial ) or ( lv_phio is initial ) or ( lv_guid is initial ).
      return.
    endif.

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        lo_slpm_data_provider->delete_attachment(
            exporting
                ip_guid = lv_guid
                ip_loio = lv_loio
                ip_phio = lv_phio ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method attachmentset_delete_entity.

    data: lv_guid               type crmt_object_guid,
          lv_loio               type string,
          lv_phio               type string,
          lo_slpm_data_provider type ref to yif_slpm_data_manager.

    me->get_attachment_keys(
       exporting
           it_key_tab = it_key_tab
       importing
           ep_guid = lv_guid
           ep_loio = lv_loio
           ep_phio = lv_phio ).

    if ( lv_loio is initial ) or ( lv_phio is initial ) or ( lv_guid is initial ).
      return.
    endif.

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        lo_slpm_data_provider->delete_attachment(
            exporting
                ip_guid = lv_guid
                ip_loio = lv_loio
                ip_phio = lv_phio ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).


    endtry.


  endmethod.

  method textset_create_entity.

    data: lv_guid               type crmt_object_guid,
          lv_tdid               type tdid,
          lv_text               type string,
          lo_slpm_data_provider type ref to yif_slpm_data_manager.

    loop at it_key_tab assigning field-symbol(<ls_guid>).
      move <ls_guid>-value to lv_guid.
    endloop.

    call method io_data_provider->read_entry_data
      importing
        es_data = er_entity.

    lv_text = er_entity-text.
    lv_tdid = er_entity-tdid.

    if lv_tdid is initial.
      return.
    endif.

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        lo_slpm_data_provider->create_text(
        exporting
            ip_guid = lv_guid
            ip_tdid = lv_tdid
            ip_text = lv_text ).

        er_entity = lo_slpm_data_provider->get_last_text( exporting ip_guid = lv_guid ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method productset_get_entityset.

    data: lt_products           type yslpm_tt_products,
          ls_entity             like line of et_entityset,
          lo_slpm_product       type ref to yif_crm_service_product,
          lo_slpm_user          type ref to yif_slpm_user,
          lt_filter_customer_bp type /iwbep/t_cod_select_options.

    " Get filter

    lt_filter_customer_bp = get_filter_select_options(
        io_tech_request_context  = io_tech_request_context
        ip_property = 'COMPANYBUSINESSPARTNER' ).


    " Get products, available for user

    try.

        lo_slpm_user = new ycl_slpm_user( sy-uname ).

        lt_products = lo_slpm_user->get_slpm_products_of_user(  ).

        loop at lt_products assigning field-symbol(<ls_product>) where  companybusinesspartner in lt_filter_customer_bp.

          ls_entity-guid = <ls_product>-guid.
          ls_entity-id = <ls_product>-id.
          ls_entity-name = <ls_product>-name.
          ls_entity-companybusinesspartner = <ls_product>-companybusinesspartner.
          lo_slpm_product = new ycl_crm_service_product( <ls_product>-guid ).
          ls_entity-prioritiescount = lo_slpm_product->get_resp_profile_prio_count(  ).

          " We skip all products, which don't have proper priorities assigned
          " through a response profile

          if ls_entity-prioritiescount > 0.

            append  ls_entity to et_entityset.

          endif.

          clear ls_entity.
        endloop.

      catch ycx_system_user_exc ycx_slpm_configuration_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method productset_get_entity.

    data: lv_guid         type comt_product_guid,
          lo_slpm_product type ref to yif_crm_service_product.

    loop at it_key_tab assigning field-symbol(<ls_guid>).
      move <ls_guid>-value to lv_guid.
    endloop.

    lo_slpm_product = new ycl_crm_service_product( lv_guid ).

    er_entity-guid = lv_guid.
    er_entity-id = lo_slpm_product->yif_crm_product~get_id( ).
    er_entity-name = lo_slpm_product->yif_crm_product~get_name(  ).
    er_entity-prioritiescount = lo_slpm_product->get_resp_profile_prio_count(  ).

  endmethod.

  method problemset_create_entity.

    data: lo_slpm_data_provider type ref to yif_slpm_data_manager,
          lv_guid               type crmt_object_guid.

    io_data_provider->read_entry_data( importing es_data = er_entity ).

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        er_entity = lo_slpm_data_provider->create_problem( er_entity ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method priorityset_get_entityset.

    data: lo_slpm_data_provider    type ref to yif_slpm_data_manager,
          lv_product_guid          type comt_product_guid,
          lt_priorities            type ycrm_order_tt_priorities,
          lt_priorities_of_product type ycrm_order_tt_priorities,
          lt_priorities_range      type range  of char40,
          ls_priority_range        like line of lt_priorities_range.

    loop at it_key_tab assigning field-symbol(<ls_guid>).
      move <ls_guid>-value to lv_product_guid.
    endloop.

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        lt_priorities = lo_slpm_data_provider->get_all_priorities(  ).

        sort lt_priorities.


        " Priorities of a product
        " Filter all priorities removing priorities not relevant for a product

        if lv_product_guid is not initial.

          lt_priorities_of_product = lo_slpm_data_provider->get_priorities_of_product( lv_product_guid ).

          loop at lt_priorities_of_product assigning field-symbol(<ls_priority_of_product>).

            ls_priority_range-low = <ls_priority_of_product>-code.
            ls_priority_range-option = 'EQ'.
            ls_priority_range-sign = 'E'.

            append ls_priority_range to lt_priorities_range.

          endloop.

          delete  lt_priorities where code  in lt_priorities_range.

        endif.

        et_entityset = lt_priorities.


      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.


  method problemset_update_entity.

    data: lo_slpm_data_provider type ref to yif_slpm_data_manager,
          lv_guid               type crmt_object_guid.

    loop at it_key_tab assigning field-symbol(<ls_guid>).
      move <ls_guid>-value to lv_guid.
    endloop.

    io_data_provider->read_entry_data( importing es_data = er_entity ).

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        er_entity = lo_slpm_data_provider->update_problem(
        exporting
         ip_guid = lv_guid
         is_problem = er_entity ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.


  endmethod.


  method statusset_get_entityset.

    data: lo_slpm_data_provider type ref to yif_slpm_data_manager,
          lv_status             type j_estat.

    lv_status = get_filter_value(
       exporting
         io_tech_request_context = io_tech_request_context
         ip_property             = 'CODE').

    try.
        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        et_entityset = cond #(
            when lv_status is initial then lo_slpm_data_provider->get_all_statuses(  )
            else lo_slpm_data_provider->get_list_of_possible_statuses( lv_status )
        ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method get_filter_value.

    data  rg_filter_so     type /iwbep/t_cod_select_options.

    data(it_filter_so) = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    if line_exists( it_filter_so[ property = ip_property ] ).

      rg_filter_so = it_filter_so[ property = ip_property ]-select_options.
      loop at rg_filter_so assigning field-symbol(<rs_filter_so>).
        ep_value = <rs_filter_so>-low.
      endloop.

    endif.

  endmethod.

  method get_filter_select_options.

    data(it_filter_so) = io_tech_request_context->get_filter( )->get_filter_select_options( ).

    if line_exists( it_filter_so[ property = ip_property ] ).

      et_select_options = it_filter_so[ property = ip_property ]-select_options.

    endif.

  endmethod.

  method systemuserset_get_entityset.

    data:
      lo_system_user type ref to yif_system_user,
      lo_slpm_user   type ref to yif_slpm_user,
      ls_entity      like line of et_entityset,
      ls_company     type yslpm_ts_company.

    try.

        lo_slpm_user = new ycl_slpm_user( sy-uname ).

        lo_system_user ?= lo_slpm_user.

        ls_entity-username = sy-uname.
        ls_entity-fullname = lo_system_user->get_fullname(  ).
        ls_entity-businesspartner = lo_system_user->get_businesspartner(  ).

        ls_company = lo_slpm_user->get_slpm_prime_company_of_user(  ).

        ls_entity-companybusinesspartner = ls_company-companybusinesspartner.

        ls_entity-companyname = ls_company-companyname.

        ls_entity-authtocreateproblemonbehalf = lo_slpm_user->is_auth_to_create_on_behalf(  ).

        ls_entity-authtocreateproblem = lo_slpm_user->is_auth_to_create_problems(  ).

        ls_entity-authtoreadproblems = lo_slpm_user->is_auth_to_read_problems(  ).

        ls_entity-authtoupdateproblem = lo_slpm_user->is_auth_to_update_problems(  ).

        append ls_entity to et_entityset.

      catch ycx_system_user_exc into data(lcx_process_exception).

        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method processorset_get_entityset.

    data: lo_slpm_data_provider type ref to yif_slpm_data_manager.

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        et_entityset = lo_slpm_data_provider->get_list_of_processors( ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method companyset_get_entityset.

    data: lo_slpm_data_provider type ref to yif_slpm_data_manager.

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        et_entityset = lo_slpm_data_provider->get_list_of_companies(  ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.

  endmethod.

  method frontendconfigur_get_entityset.

    data: lv_application        type char100,
          lo_slpm_data_provider type ref to yif_slpm_data_manager.

    lv_application = get_filter_value(
        exporting
        io_tech_request_context = io_tech_request_context
        ip_property             = 'APPLICATION').

    if lv_application is initial.

      return.

    endif.

    try.

        lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

        et_entityset = lo_slpm_data_provider->get_frontend_configuration( lv_application ).

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
        ycx_assistant_utilities_exc ycx_slpm_configuration_exc
        ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.


  endmethod.

  method systemset_get_entityset.

    data:
      ls_entity             like line of et_entityset,
      lo_slpm_customer      type ref to yif_slpm_customer,
      lv_filter_customer_bp type bu_partner.

    " Providing all systems of a SLPM customer

    " Get filter of Customer BP

    lv_filter_customer_bp = get_filter_value(
       exporting
         io_tech_request_context = io_tech_request_context
         ip_property             = 'COMPANYBUSINESSPARTNER').


    try.

        if lv_filter_customer_bp is not initial.

          lo_slpm_customer  = new ycl_slpm_customer( lv_filter_customer_bp  ).
          et_entityset = lo_slpm_customer->get_slpm_systems_of_customer(  ).

        endif.

      catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
          ycx_assistant_utilities_exc ycx_slpm_configuration_exc
          ycx_system_user_exc into data(lcx_process_exception).
        raise_exception( lcx_process_exception->get_text(  ) ).

    endtry.


  endmethod.

  method slairthistoryset_get_entityset.

    data: lv_guid               type crmt_object_guid,
          lo_slpm_data_provider type ref to yif_slpm_data_manager.


    read table it_key_tab into data(ls_key_tab) with key name = 'Guid'.

    lv_guid = ls_key_tab-value.

    if lv_guid is not initial.

      try.

          lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

          et_entityset = lo_slpm_data_provider->get_problem_sla_irt_history( exporting
              ip_guid = lv_guid ).

        catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
            ycx_assistant_utilities_exc ycx_slpm_configuration_exc
            ycx_system_user_exc into data(lcx_process_exception).
          raise_exception( lcx_process_exception->get_text(  ) ).

      endtry.

    endif.

  endmethod.

  method slampthistoryset_get_entityset.

    data: lv_guid               type crmt_object_guid,
          lo_slpm_data_provider type ref to yif_slpm_data_manager.


    read table it_key_tab into data(ls_key_tab) with key name = 'Guid'.

    lv_guid = ls_key_tab-value.

    if lv_guid is not initial.

      try.

          lo_slpm_data_provider = new ycl_slpm_data_manager_proxy(  ).

          et_entityset = lo_slpm_data_provider->get_problem_sla_mpt_history( exporting
              ip_guid = lv_guid ).

        catch ycx_slpm_odata_exc ycx_crm_order_api_exc ycx_slpm_data_manager_exc
            ycx_assistant_utilities_exc ycx_slpm_configuration_exc
            ycx_system_user_exc into data(lcx_process_exception).
          raise_exception( lcx_process_exception->get_text(  ) ).

      endtry.

    endif.

  endmethod.

endclass.
