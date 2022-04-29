*----------------------------------------------------------------------*
***INCLUDE LZREL_TBLF01.
*----------------------------------------------------------------------*
FORM get_username_desc_zrel_v004.
  TRY.
      NEW zcl_rel_user( iv_user = zrel_v004-username )->get_user_detail(
        IMPORTING
          es_user_detail = DATA(ls_user_detail) ).
      zrel_v004-username_desc = ls_user_detail-fullname.
    CATCH zcx_rel. " REL - Excepciones estrategías liberación
  ENDTRY.

  TRY.
      CLEAR: ls_user_detail.
      NEW zcl_rel_user( iv_user = zrel_v004-backup_user )->get_user_detail(
        IMPORTING
          es_user_detail = ls_user_detail ).
      zrel_v004-backup_user_desc = ls_user_detail-fullname.
    CATCH zcx_rel. " REL - Excepciones estrategías liberación
  ENDTRY.

ENDFORM.
