class ycl_slpm_mpt_store_statuses definition
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



class ycl_slpm_mpt_store_statuses implementation.


  method constructor.

    super->constructor(
        value #(
            ( statusin = 'E0016' statusout = 'E0017' )
            ( statusin = 'E0002' statusout = 'E0003' )
            ( statusin = 'E0002' statusout = 'E0005' )
        )
    ).

  endmethod.
endclass.
