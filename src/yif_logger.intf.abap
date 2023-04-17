interface yif_logger
  public .
  methods warn
    importing
      !ip_message_text type string .
  methods err
    importing
      !ip_message_text type string .
  methods info
    importing
      !ip_message_text type string .

endinterface.
