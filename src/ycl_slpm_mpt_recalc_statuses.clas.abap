class ycl_slpm_mpt_recalc_statuses definition
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



class ycl_slpm_mpt_recalc_statuses implementation.


  method constructor.

    super->constructor(
        value #(
             ( statusin = 'E0017' statusout = 'E0016' )
             ( statusin = 'E0003' statusout = 'E0002' )
             ( statusin = 'E0005' statusout = 'E0002' )
        )
    ).

  endmethod.
endclass.
