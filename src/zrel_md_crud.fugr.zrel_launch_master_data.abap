FUNCTION zrel_launch_master_data.
*"----------------------------------------------------------------------
*"*"Interfase local
*"  IMPORTING
*"     VALUE(IV_REQUEST_ID) TYPE  ZREL_T010-REQUEST_ID
*"     VALUE(IV_LANGU) TYPE  SYLANGU DEFAULT SY-LANGU
*"  EXPORTING
*"     VALUE(ET_RETURN) TYPE  BAPIRET2_T
*"----------------------------------------------------------------------

  DATA(lo_change) = NEW zcl_rel_strategy_chnge_request( iv_langu = iv_langu ).

  lo_change->launch_update_master_data(
    EXPORTING
      iv_request_id = iv_request_id
    IMPORTING
      et_return     = et_return ).



ENDFUNCTION.
