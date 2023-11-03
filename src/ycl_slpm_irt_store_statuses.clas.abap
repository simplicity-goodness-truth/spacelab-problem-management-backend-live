class ycl_slpm_irt_store_statuses definition
  public
    inheriting from ycl_slpm_status_pairs
  final
  create public .

  public section.

    methods:
      constructor.

  protected section.

  private section.

endclass.

class ycl_slpm_irt_store_statuses implementation.

  method constructor.

    super->constructor(
        value #(
            ( statusin = 'E0016' statusout = 'E0017' )
        )
    ).

  endmethod.
endclass.
