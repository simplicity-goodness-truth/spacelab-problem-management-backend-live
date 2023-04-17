interface yif_serv_profile_date_calc
  public .

  methods: add_hours_to_date
    importing
      ip_added_hours_total type int2
      ip_time_from         type sy-uzeit optional
      ip_date_from         type sy-datum optional
    exporting
      ep_sla_date          type sy-datum
      ep_sla_time          type sy-uzeit.

  methods: add_seconds_to_date
    importing
      ip_added_seconds_total type int4
      ip_time_from           type sy-uzeit optional
      ip_date_from           type sy-datum optional
    exporting
      ep_sla_date            type sy-datum
      ep_sla_time            type sy-uzeit.

endinterface.
