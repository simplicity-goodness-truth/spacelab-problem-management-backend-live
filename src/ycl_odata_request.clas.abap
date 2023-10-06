class ycl_odata_request definition
  public
  inheriting from ycl_http_request
  create public .

  public section.

    methods:

      constructor
        importing
          io_request_details type ref to /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context.

  protected section.

  private section.

    data:
        mo_request_details type ref to /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context.

    methods:
      set_request_details
        importing
          io_request_details type ref to /iwbep/if_mgw_core_srv_runtime=>ty_s_mgw_request_context.

endclass.


class ycl_odata_request implementation.

  method constructor.

    super->constructor( io_request_details->technical_request-request_header ).

    me->set_request_details( io_request_details ).

  endmethod.

  method set_request_details.

    mo_request_details = io_request_details.

  endmethod.

endclass.
