*&---------------------------------------------------------------------*
*& Report  zcustom_crm_order_auto_stat
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report ycustom_crm_order_auto_stat.

data lo_crm_order_auto_stat_setter type ref to yif_crm_order_auto_stat_setter.

lo_crm_order_auto_stat_setter = new ycl_crm_order_auto_stat_setter( ).

lo_crm_order_auto_stat_setter->process_orders_status_setting( ).
