*&---------------------------------------------------------------------*
*& Report ZREL_R_LAUNCH_UPDATE_MD
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrel_r_launch_update_md.

PARAMETERS: p_reqid TYPE zrel_t010-request_id OBLIGATORY.


START-OF-SELECTION.

  DATA(lo_change) = NEW zcl_rel_strategy_chnge_request( ).

  lo_change->launch_update_master_data(
    EXPORTING
      iv_request_id = p_reqid
    IMPORTING
      et_return     = DATA(lt_return) ).

  cl_demo_output=>display_data( lt_return ).
