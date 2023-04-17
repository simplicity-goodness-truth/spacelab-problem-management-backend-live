class ycl_company definition
  public
  create public .

  public section.
    interfaces yif_company .

    methods constructor
      importing
        ip_bp_num type bu_partner.

  protected section.
  private section.

    data:
      mo_company_master_data   type ref to yif_bp_master_data,
      mo_company_contacts_book type ref to yif_contacts_book.

endclass.

class ycl_company implementation.

  method constructor.

    mo_company_master_data = new ycl_bp_master_data( ip_bp_num ).
    mo_company_contacts_book ?= mo_company_master_data.

  endmethod.

  method yif_company~get_company_bp_number.

    rp_bp_num = me->mo_company_master_data->get_bp_number(  ).

  endmethod.

  method yif_company~get_company_bp_address_string.

    data: ls_adrc      type adrc,
          lv_bp_number type bu_partner.

    lv_bp_number =  me->mo_company_master_data->get_bp_number(  ).

    if lv_bp_number is not initial.

      select single a~city1 a~street a~house_num1 a~country
        from (
            adrc as a inner join
            but020 as b
            on a~addrnumber = b~addrnumber )
            into corresponding fields of ls_adrc
            where b~partner = lv_bp_number.

      rp_bp_address = |{ ls_adrc-street }| && | | && |{ ls_adrc-house_num1 }| && |, |
        && |{ ls_adrc-city1 }| && |, | && |{ ls_adrc-country }|.

    endif.

  endmethod.

  method yif_company~get_company_email_address.

    rp_bp_email_address = me->mo_company_contacts_book->get_email_address(  ).

  endmethod.

  method yif_company~get_company_name.

    rp_bp_company_name = me->mo_company_master_data->get_but000_record(
)-mc_name1.

  endmethod.

  method yif_company~get_contact_persons.

    rt_contact_persons_bp = me->mo_company_master_data->get_contact_persons(  ).

  endmethod.

endclass.
