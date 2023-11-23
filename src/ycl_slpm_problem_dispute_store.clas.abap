class ycl_slpm_problem_dispute_store definition
  public
  final
  create public .

  public section.

    interfaces yif_slpm_problem_dispute_store .

    methods constructor
      importing
        ip_guid type crmt_object_guid.

  protected section.

  private section.

    data:
           mv_guid  type crmt_object_guid.

    methods:

      set_problem_guid
        importing
          ip_guid type crmt_object_guid,

      add_database_record
        importing
          ip_disputeopen type abap_bool.

endclass.



class ycl_slpm_problem_dispute_store implementation.

  method constructor.

    me->set_problem_guid( ip_guid ).

  endmethod.

  method yif_slpm_problem_dispute_store~open_problem_dispute.

    if me->yif_slpm_problem_dispute_store~is_problem_dispute_open( ) eq abap_false.

      me->add_database_record( abap_true ).

    endif.

  endmethod.

  method yif_slpm_problem_dispute_store~is_problem_dispute_open.

    select disputeopen
        from yslpm_pr_dispute
        into rp_dispute_active
        up to 1 rows
        where problemguid = mv_guid order by update_timestamp descending.

    endselect.

  endmethod.

  method set_problem_guid.

    mv_guid = ip_guid.

  endmethod.

  method yif_slpm_problem_dispute_store~close_problem_dispute.

    if me->yif_slpm_problem_dispute_store~is_problem_dispute_open( ) eq abap_true.

      me->add_database_record( abap_false ).

    endif.

  endmethod.

  method add_database_record.

    data wa_slpm_pr_dispute type yslpm_pr_dispute.

    get time stamp field wa_slpm_pr_dispute-update_timestamp.
    wa_slpm_pr_dispute-update_timezone = zcl_assistant_utilities=>get_system_timezone(  ).
    wa_slpm_pr_dispute-guid = zcl_assistant_utilities=>generate_x16_guid(  ).
    wa_slpm_pr_dispute-problemguid = mv_guid.
    wa_slpm_pr_dispute-disputeopen = ip_disputeopen.
    wa_slpm_pr_dispute-username = sy-uname.

    insert yslpm_pr_dispute from wa_slpm_pr_dispute.

  endmethod.

  method yif_slpm_problem_dispute_store~get_problem_dispute_history.

    select
        guid
        problemguid
        update_timestamp
        update_timezone
        username
        disputeopen
    from yslpm_pr_dispute
    into corresponding fields of table rt_dispute_history
        where problemguid = mv_guid.

  endmethod.

  method yif_slpm_problem_dispute_store~is_there_problem_dispute_hist.

    select count( * ) from yslpm_pr_dispute up to 1 rows where problemguid = mv_guid.

    if sy-subrc eq 0.

      rp_dispute_hist_exists = abap_true.

    endif.

  endmethod.

endclass.
