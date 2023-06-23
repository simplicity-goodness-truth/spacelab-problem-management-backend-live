*&---------------------------------------------------------------------*
*& Report  zslpm_export_executor
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report yslpm_export_executor.


*data:
*   lo_prob_exp type ref to zcl_slpm_prob_exp.
*
*lo_prob_exp =  new zcl_slpm_prob_exp_dec_spr_sht(
*                   io_slpm_prob_exp = new zcl_slpm_prob_exp( )
*               ).
*
*lo_prob_exp->zif_slpm_prob_exp~export_problems(  ).

data:
  lo_prob_exp_spr_sht type ref to ycl_slpm_prob_exp,
  lo_prob_exp_docx    type ref to ycl_slpm_prob_exp,
  lo_prob_exp         type ref to ycl_slpm_prob_exp,
  lt_prob_exp         type yif_slpm_prob_exp=>ty_problems.


lo_prob_exp = new ycl_slpm_prob_exp( ).

lo_prob_exp_spr_sht =  new ycl_slpm_prob_exp_dec_spr_sht(
    io_slpm_prob_exp = lo_prob_exp ).

lt_prob_exp = lo_prob_exp_spr_sht->yif_slpm_prob_exp~export_problems(  ).

lo_prob_exp_docx =  new ycl_slpm_prob_exp_dec_docx(
    io_slpm_prob_exp = lo_prob_exp ).

lo_prob_exp_docx->yif_slpm_prob_exp~export_problems( it_problems_export = lt_prob_exp ).
