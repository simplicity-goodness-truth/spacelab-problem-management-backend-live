class ycl_bp_master_data definition
  public
  create public .

  public section.
    interfaces: yif_contacts_book,
      yif_bp_master_data .
    methods constructor
      importing
        ip_bp_num type bu_partner.

  protected section.
  private section.
    data: mv_bp_num type bu_partner,
          ms_but000 type but000.

    methods: set_but000.

endclass.

class ycl_bp_master_data implementation.

  method constructor.

    call function 'CONVERSION_EXIT_ALPHA_INPUT'
      exporting
        input  = ip_bp_num
      importing
        output = mv_bp_num.

  endmethod.

  method set_but000.

    if ( mv_bp_num is not initial ) and ( mv_bp_num ne '0000000000' ).

      select single * into ms_but000 from but000
       where partner eq mv_bp_num.

    endif.

  endmethod.

  method yif_contacts_book~get_full_name.


    if ms_but000 is initial.

      me->set_but000(  ).

    endif.

*    rp_full_name = cond #(
*          when ms_but000-name1_text is not initial then ms_but000-name1_text
*          else |{ ms_but000-name_first }| && | | && |{ ms_but000-name_last }|
*        ).


    rp_full_name = cond #(
        when ms_but000-type eq '1' then cond #(
            when ms_but000-name1_text is not initial then ms_but000-name1_text
            else |{ ms_but000-name_first }| && | | && |{ ms_but000-name_last }|
        )
        when ms_but000-type eq '2' then ms_but000-name_org2
    ).


  endmethod.



  method yif_contacts_book~get_email_address.

    if ms_but000 is initial.

      me->set_but000(  ).

    endif.

    " email search depends on a type of a Business Partner

    case ms_but000-type.

      when 1.

        " Business Partner is a Person

        select single smtp_addr into rp_email_address from adr6 where
          persnumber = ms_but000-persnumber.

      when 2.

        " Business Partner is an Organization

        select single smtp_addr into rp_email_address from adr6 where
          addrnumber = ms_but000-addrcomm.

    endcase.

  endmethod.

  method yif_bp_master_data~get_contact_persons.

    select partner2 into table rt_contact_persons_bp
        from but051 where partner1 eq mv_bp_num.

  endmethod.

  method yif_bp_master_data~is_contact_person_of.

    select partner1 into table rt_companies_bp
        from but051 where partner2 eq mv_bp_num.

  endmethod.

  method yif_bp_master_data~get_bp_number.

    if ms_but000 is initial.

      me->set_but000(  ).

    endif.

    rp_bp_num = me->ms_but000-partner.

  endmethod.

  method yif_bp_master_data~get_but000_record.

    if ms_but000 is initial.

      me->set_but000(  ).

    endif.

    rs_but000 = me->ms_but000.

  endmethod.

endclass.
