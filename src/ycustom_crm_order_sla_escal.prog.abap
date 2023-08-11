*&---------------------------------------------------------------------*
*& Report  ycustom_crm_order_sla_escal
*&
*&---------------------------------------------------------------------*
*&
*&
*&---------------------------------------------------------------------*
report ycustom_crm_order_sla_escal.

data lo_custom_crm_order_sla_escal type ref to yif_custom_crm_order_sla_escal.

lo_custom_crm_order_sla_escal = new ycl_custom_crm_order_sla_escal(  ).

lo_custom_crm_order_sla_escal->process_escalations(  ).
