class ycl_slpm_status_pairs definition
  public
  create public .

  public section.

    interfaces yif_slpm_status_pairs .

    methods:
      constructor
        importing
          it_class_status_pairs type yslpm_tt_status_pairs.

  protected section.

    data:
      mt_class_status_pairs type yslpm_tt_status_pairs.

  private section.

    methods set_class_status_pairs
      importing
        it_class_status_pairs type yslpm_tt_status_pairs.

endclass.

class ycl_slpm_status_pairs implementation.

  method yif_slpm_status_pairs~get_all_status_pairs.

    rt_status_pairs = mt_class_status_pairs.

  endmethod.

  method constructor.

    me->set_class_status_pairs( it_class_status_pairs ).

  endmethod.

  method set_class_status_pairs.

    mt_class_status_pairs = it_class_status_pairs.

  endmethod.

endclass.
