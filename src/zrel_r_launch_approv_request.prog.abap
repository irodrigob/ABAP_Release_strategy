*&---------------------------------------------------------------------*
*& Report zrel_r_launch_approv_request
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT zrel_r_launch_approv_request.

PARAMETERS p_reqid TYPE zrel_t010-request_id OBLIGATORY.
PARAMETERS p_action TYPE zrel_e_action_approv OBLIGATORY.
PARAMETERS p_reason TYPE zrel_e_approval_reason.
PARAMETERS p_umd AS CHECKBOX.

START-OF-SELECTION.

  DATA(lo_change) = NEW zcl_rel_strategy_chnge_request( ).

  lo_change->approve_request(
    EXPORTING
      iv_request_id          = p_reqid
      iv_action              = p_action
      iv_reason              = p_reason
      iv_launch_update_md = p_umd
    IMPORTING
      et_return              = DATA(lt_return) ).

  cl_demo_output=>display_data( lt_return ).
