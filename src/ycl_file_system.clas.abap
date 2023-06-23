class YCL_FILE_SYSTEM definition
  public
  create public .

  public section.

    interfaces yif_file_system .

  protected section.

  private section.

endclass.


class YCL_FILE_SYSTEM implementation.

  method yif_file_system~save_binary_to_fs.

    try.
        open dataset ip_file_path_and_name  for output in binary mode.

        transfer ip_file_contents_binary to ip_file_path_and_name.

        close dataset ip_file_path_and_name.

      catch cx_sy_file_open cx_sy_file_authority cx_sy_too_many_files.

    endtry.


  endmethod.


  method yif_file_system~create_folder.

    data: lv_command type rlgrap-filename.

    lv_command = |cmd /c mkdir| && | | && ip_folder_path.

    call 'SYSTEM' id 'COMMAND' field lv_command.

  endmethod.

endclass.
