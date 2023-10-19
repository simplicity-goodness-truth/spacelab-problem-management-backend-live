class YCL_SLPM_ODATA_REQUEST definition
  public
  inheriting from ycl_odata_request
  final
  create public .

  public section.

    interfaces:
      yif_slpm_odata_request.

    methods:
      constructor
        importing
          io_request_details    type ref to /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context
          io_slpm_data_provider type ref to yif_slpm_data_manager optional  .

  protected section.

  private section.

    data :
      mt_http_headers_permissions type table of yslpm_http_hdr,
      mt_http_headers_filter      type yhttp_tt_request_header_filter,
      mv_odata_service_name       type string,
      mo_slpm_data_provider       type ref to yif_slpm_data_manager,
      mo_active_configuration     type ref to yif_slpm_configuration.

    methods:
      set_http_headers_permissions,

      set_http_headers_filter,

      set_odata_service_name
        importing
          ip_odata_service_name type string,

      is_header_secure
        returning
          value(rp_secure) type abap_bool,

      set_slpm_data_provider
        importing
          io_slpm_data_provider type ref to yif_slpm_data_manager optional,

      set_active_configuration
        importing
          io_active_configuration type ref to yif_slpm_configuration.


endclass.



class YCL_SLPM_ODATA_REQUEST implementation.

  method constructor.

    super->constructor( io_request_details = io_request_details ).

    me->set_odata_service_name( io_request_details->service_doc_name ).

    if io_slpm_data_provider is not initial.

      me->set_slpm_data_provider( io_slpm_data_provider ).

      me->set_active_configuration( io_slpm_data_provider->get_active_configuration( )  ).

    endif.



  endmethod.

  method yif_slpm_odata_request~is_client_secure.

    " If a HTTP header should be validated

    rp_secure = abap_true.

    if ( mo_active_configuration->get_parameter_value( 'VALIDATE_MAIN_ODATA_SRV_HTTP_HEADERS' ) eq 'X').

      if ( me->is_header_secure( ) eq abap_false ).

        rp_secure = abap_false.

      endif.

    endif.

  endmethod.

  method set_http_headers_permissions.

    select mandt ruleid target headerkey
        valuesign valueoption valuestring
        from yslpm_http_hdr into table mt_http_headers_permissions.

  endmethod.

  method set_http_headers_filter.

    data: wa_http_headers_filter type yhttp_ts_request_header_filter,
          wa_http_filter         type /iwbep/s_cod_select_option,
          wa_http_filters        type /iwbep/t_cod_select_options.

    loop at mt_http_headers_permissions assigning field-symbol(<fs_http_headers_permission>)
         where target eq mv_odata_service_name
         group by ( key = <fs_http_headers_permission>-headerkey )
         assigning field-symbol(<fs_http_headers_group>).

      loop at group <fs_http_headers_group> assigning field-symbol(<fs_http_headers_group_rec>).

        wa_http_filter-sign = <fs_http_headers_group_rec>-valuesign.

        wa_http_filter-option = <fs_http_headers_group_rec>-valueoption.

        wa_http_filter-low = <fs_http_headers_group_rec>-valuestring.

        append wa_http_filter to wa_http_filters.

      endloop.

      wa_http_headers_filter-key = <fs_http_headers_group>-key.
      wa_http_headers_filter-filter = wa_http_filters.

      append wa_http_headers_filter to mt_http_headers_filter.

      clear wa_http_filters.

    endloop.

  endmethod.

  method set_odata_service_name.

    mv_odata_service_name = ip_odata_service_name.

  endmethod.

  method is_header_secure.

    if mt_http_headers_permissions is initial.

      me->set_http_headers_permissions( ).

    endif.

    me->set_http_headers_filter( ).

    try.

        rp_secure = abap_true.

        if ( me->yif_http_request~is_header_aligned_to_filter( mt_http_headers_filter ) eq abap_false ).

          rp_secure = abap_false.

        endif.

      catch cx_sy_itab_line_not_found.

    endtry.



  endmethod.

  method set_slpm_data_provider.

    mo_slpm_data_provider = io_slpm_data_provider.

  endmethod.

  method set_active_configuration.

    mo_active_configuration = io_active_configuration.

  endmethod.

endclass.
