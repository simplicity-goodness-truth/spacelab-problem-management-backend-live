class ycl_slpm_cache_helper definition
  public
  final
  create public .

  public section.

    methods:

      constructor,

      free_cache,

      free_all_cached_problems_guids,

      free_all_cached_problems.

  protected section.

  private section.

endclass.



class ycl_slpm_cache_helper implementation.

  method constructor.

    data lt_shma_instance_infos type shm_inst_infos.

    try.

        lt_shma_instance_infos = ycl_slpm_area=>get_instance_infos(  ).

        if lt_shma_instance_infos is initial.

          ycl_slpm_area=>build( ).


        endif.

      catch cx_shm_build_failed.

        return.

      catch cx_shm_inconsistent.
        ycl_slpm_area=>free_area( ).


    endtry.



  endmethod.


  method free_cache.

    ycl_slpm_area=>free_area( ).

  endmethod.



  method free_all_cached_problems_guids.

    data:
      lo_area type ref to ycl_slpm_area,
      lo_root type ref to ycl_slpm_shma.

    try.
        lo_area = ycl_slpm_area=>attach_for_update( ).

      catch cx_shm_pending_lock_removed cx_shm_change_lock_active cx_shm_version_limit_exceeded
        cx_shm_exclusive_lock_active cx_shm_no_active_version cx_shm_build_failed .

      catch cx_shm_inconsistent.
        ycl_slpm_area=>free_area( ).

        return.

    endtry.

    try.

        lo_root ?= lo_area->get_root( ).

        if lo_root is  initial.
          create object lo_root area handle lo_area.
        endif.

        lo_root->invalidate_cached_prob_guids(  ).

        lo_area->set_root( lo_root ).

        lo_area->detach_commit( ).

      catch cx_sy_ref_is_initial.

    endtry.


  endmethod.

  method free_all_cached_problems.

    data:
      lo_area type ref to ycl_slpm_area,
      lo_root type ref to ycl_slpm_shma.

    try.
        lo_area = ycl_slpm_area=>attach_for_update( ).

      catch cx_shm_pending_lock_removed cx_shm_change_lock_active cx_shm_version_limit_exceeded
        cx_shm_exclusive_lock_active cx_shm_no_active_version cx_shm_build_failed .

      catch cx_shm_inconsistent.
        ycl_slpm_area=>free_area( ).

        return.

    endtry.

    try.

        lo_root ?= lo_area->get_root( ).

        if lo_root is  initial.
          create object lo_root area handle lo_area.
        endif.

        lo_root->invalidate_all_cached_problems(  ).

        lo_area->set_root( lo_root ).

        lo_area->detach_commit( ).

      catch cx_sy_ref_is_initial.

    endtry.


  endmethod.

endclass.
