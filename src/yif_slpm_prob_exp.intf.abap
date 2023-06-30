interface yif_slpm_prob_exp
  public .

  types:
    ty_texts       type table of aic_s_text_incdnt_odata with non-unique default key,
    ty_attachments type table of cl_ai_crm_gw_mymessage_mpc=>ts_attachment with non-unique default key,
    ty_history     type table of yslpm_ts_pr_his_hry with non-unique default key.

  types: begin of ty_problem,

           data        type ycrm_order_ts_sl_problem,
           texts       type ty_texts,
           attachments type ty_attachments,
           history     type ty_history,

         end of ty_problem.

  types:
         ty_problems type table of ty_problem with non-unique default key.

  methods:

    export_problems
      importing
        it_problems_export type yif_slpm_prob_exp=>ty_problems optional
      returning
        value(rt_problems) type ty_problems
      raising
        ycx_slpm_configuration_exc.

endinterface.
