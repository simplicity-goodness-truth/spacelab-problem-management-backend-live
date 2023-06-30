interface yif_file_system
  public .

  class-methods:

    save_binary_to_fs
      importing
        ip_file_contents_binary type xstringval
        ip_file_path_and_name   type string,

    save_text_to_fs
      importing
        ip_file_contents_text type string
        ip_file_path_and_name type string,

    create_folder
      importing
        ip_folder_path type string.

endinterface.
