class ycl_slpm_sla_irt_hist definition
  public
  final
  create public .

  public section.

    interfaces yif_slpm_sla_hist .

    methods:
      constructor
        importing
          ip_guid type crmt_object_guid.

  protected section.
  private section.

    data:
      mv_guid    type crmt_object_guid.

    methods:
      set_guid
        importing
          ip_guid type crmt_object_guid.

endclass.



class ycl_slpm_sla_irt_hist implementation.


  method constructor.

    me->set_guid( ip_guid ).

  endmethod.


  method set_guid.

    mv_guid = ip_guid.

  endmethod.


  method yif_slpm_sla_hist~is_there_pending_shift.

    data: lv_last_out_stat_for_irt_hist type char5,
          lt_statuses_for_irt_recalc    type yslpm_tt_status_pairs.

    select  statusout
        from yslpm_irt_hist
        into lv_last_out_stat_for_irt_hist
       up to 1 rows
         where problemguid = mv_guid order by update_timestamp descending.

      try.

          lt_statuses_for_irt_recalc = new ycl_slpm_irt_recalc_statuses( )->yif_slpm_status_pairs~get_all_status_pairs( ).

          if line_exists( lt_statuses_for_irt_recalc[ statusin = ip_status ] ) and
                   ( ip_status eq lv_last_out_stat_for_irt_hist ).

            rp_pending_shift_exists = abap_true.

          endif.

        catch cx_sy_itab_line_not_found.

      endtry.

    endselect.

  endmethod.

  method yif_slpm_sla_hist~get_last_sla_timestamp.

    select  irttimestamp irttimezone
            from yslpm_irt_hist
            into ( ep_timestamp, ep_timezone )
           up to 1 rows
             where problemguid = mv_guid order by update_timestamp descending.

    endselect.

  endmethod.

  method yif_slpm_sla_hist~get_sla_perc_of_pending_shift.

    data:
      lv_last_out_stat_for_irt_hist type char5,
      lt_statuses_for_irt_recalc    type yslpm_tt_status_pairs,
      lv_sla_perc                   type int4.

    select  statusout irtperc
        from yslpm_irt_hist
        into ( lv_last_out_stat_for_irt_hist, lv_sla_perc )
       up to 1 rows
         where problemguid = mv_guid order by update_timestamp descending.

      try.

          lt_statuses_for_irt_recalc = new ycl_slpm_irt_recalc_statuses( )->yif_slpm_status_pairs~get_all_status_pairs( ).

          if line_exists( lt_statuses_for_irt_recalc[ statusin = ip_status ] ) and
                   ( ip_status eq lv_last_out_stat_for_irt_hist ).

            ep_pending_shift_exists = abap_true.
            ep_pending_shift_sla_perc = lv_sla_perc.

          endif.

        catch cx_sy_itab_line_not_found.

      endtry.

    endselect.


  endmethod.

endclass.
