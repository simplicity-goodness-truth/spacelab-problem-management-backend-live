class ycl_http_request definition
  public
  create public .

  public section.

    interfaces:
      yif_http_request.

    methods:
      constructor
        importing
          it_header type tihttpnvp optional.

  protected section.
  private section.

    data mt_header type tihttpnvp .

    methods: set_header
      importing
        it_header type tihttpnvp .

endclass.

class ycl_http_request implementation.

  method set_header.

    mt_header = it_header.

  endmethod.

  method constructor.

    if it_header is not initial.

      me->set_header( it_header ).

    endif.

  endmethod.

  method yif_http_request~is_header_aligned_to_filter.

    rp_aligned = abap_true.

    loop at it_filter assigning field-symbol(<fs_filter>).

      try.

          if not ( mt_header[ name = <fs_filter>-key ]-value in <fs_filter>-filter ).

            rp_aligned = abap_false.

            exit.

          endif.

        catch cx_sy_itab_line_not_found.

          rp_aligned = abap_false.

          exit.

      endtry.

    endloop.

  endmethod.

endclass.
