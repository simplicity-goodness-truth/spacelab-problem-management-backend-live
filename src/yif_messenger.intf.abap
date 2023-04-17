interface yif_messenger
  public .

  methods: set_message_body
    importing
      ip_message_body_text type string
      ip_message_body_type type so_obj_tp optional,

    set_message_subject
      importing
        ip_message_subject_text type so_obj_des,

    set_message_sender
      importing
        ip_message_sender type ymessenger_address,

    set_message_recepients
      importing
        it_message_recepients type ymessenger_tt_addresses,

    set_message_attachments
      importing
        it_message_attachments type ymessenger_tt_attachments.

  methods send_message.

endinterface.
