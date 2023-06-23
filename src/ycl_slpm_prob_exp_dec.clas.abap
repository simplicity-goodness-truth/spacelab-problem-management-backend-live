class ycl_slpm_prob_exp_dec definition
  public
  inheriting from ycl_slpm_prob_exp
  abstract
  create public .

  public section.

    methods:

      constructor
        importing
          io_slpm_prob_exp type ref to ycl_slpm_prob_exp
        raising
          ycx_slpm_configuration_exc.

  protected section.

    data:
      mo_slpm_prob_exp type ref to ycl_slpm_prob_exp.

  private section.

endclass.

class ycl_slpm_prob_exp_dec implementation.

  method constructor.

    super->constructor( ).

    mo_slpm_prob_exp = io_slpm_prob_exp.

  endmethod.

endclass.
