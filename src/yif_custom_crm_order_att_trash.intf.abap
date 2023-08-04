interface yif_custom_crm_order_att_trash
  public .

  methods:

    put_att_to_trash_bin
      importing
        ip_file_name type sdok_filnm
        ip_content   type xstring
        ip_mime_type type w3conttype,

    get_trash_bin_record
      importing
        ip_guid                 type sysuuid_x16
      returning
        value(rs_att_trash_bin) type ycrmo_att_trash,

    put_att_back_to_crm_order
      importing
        ip_guid type sysuuid_x16,

    empty_trash_bin.


endinterface.
