*---------------------------------------------------------------------*
*    view related FORM routines
*---------------------------------------------------------------------*
*...processing: ZREL_V001.......................................*
FORM GET_DATA_ZREL_V001.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZZT_CA_CONSTANTS WHERE
    ID EQ 'REL' AND
(VIM_WHERETAB) .
    CLEAR ZREL_V001 .
ZREL_V001-MANDT =
ZZT_CA_CONSTANTS-MANDT .
ZREL_V001-ID =
ZZT_CA_CONSTANTS-ID .
ZREL_V001-CONSTANTE =
ZZT_CA_CONSTANTS-CONSTANTE .
ZREL_V001-VALOR =
ZZT_CA_CONSTANTS-VALOR .
<VIM_TOTAL_STRUC> = ZREL_V001.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZREL_V001 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZREL_V001.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZREL_V001-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZZT_CA_CONSTANTS WHERE
  ID = ZREL_V001-ID AND
  CONSTANTE = ZREL_V001-CONSTANTE .
    IF SY-SUBRC = 0.
    DELETE ZZT_CA_CONSTANTS .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZZT_CA_CONSTANTS WHERE
  ID = ZREL_V001-ID AND
  CONSTANTE = ZREL_V001-CONSTANTE .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZZT_CA_CONSTANTS.
    ENDIF.
ZZT_CA_CONSTANTS-MANDT =
ZREL_V001-MANDT .
ZZT_CA_CONSTANTS-ID =
ZREL_V001-ID .
ZZT_CA_CONSTANTS-CONSTANTE =
ZREL_V001-CONSTANTE .
ZZT_CA_CONSTANTS-VALOR =
ZREL_V001-VALOR .
    IF SY-SUBRC = 0.
    UPDATE ZZT_CA_CONSTANTS ##WARN_OK.
    ELSE.
    INSERT ZZT_CA_CONSTANTS .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZREL_V001-UPD_FLAG,
STATUS_ZREL_V001-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZREL_V001.
  SELECT SINGLE * FROM ZZT_CA_CONSTANTS WHERE
ID = ZREL_V001-ID AND
CONSTANTE = ZREL_V001-CONSTANTE .
ZREL_V001-MANDT =
ZZT_CA_CONSTANTS-MANDT .
ZREL_V001-ID =
ZZT_CA_CONSTANTS-ID .
ZREL_V001-CONSTANTE =
ZZT_CA_CONSTANTS-CONSTANTE .
ZREL_V001-VALOR =
ZZT_CA_CONSTANTS-VALOR .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZREL_V001 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZREL_V001-ID TO
ZZT_CA_CONSTANTS-ID .
MOVE ZREL_V001-CONSTANTE TO
ZZT_CA_CONSTANTS-CONSTANTE .
MOVE ZREL_V001-MANDT TO
ZZT_CA_CONSTANTS-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZZT_CA_CONSTANTS'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZZT_CA_CONSTANTS TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZZT_CA_CONSTANTS'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*...processing: ZREL_V002.......................................*
FORM GET_DATA_ZREL_V002.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZREL_T003 WHERE
(VIM_WHERETAB) .
    CLEAR ZREL_V002 .
ZREL_V002-MANDT =
ZREL_T003-MANDT .
ZREL_V002-DEPT_SUBS =
ZREL_T003-DEPT_SUBS .
ZREL_V002-EKGRP =
ZREL_T003-EKGRP .
    SELECT SINGLE * FROM T024 WHERE
EKGRP = ZREL_T003-EKGRP .
    IF SY-SUBRC EQ 0.
ZREL_V002-EKNAM =
T024-EKNAM .
    ENDIF.
    SELECT SINGLE * FROM ZREL_T002 WHERE
DEPT_SUBS = ZREL_T003-DEPT_SUBS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T002T WHERE
DEPT_SUBS = ZREL_T002-DEPT_SUBS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V002-DESCRIPTION =
ZREL_T002T-DESCRIPTION .
      ENDIF.
    ENDIF.
<VIM_TOTAL_STRUC> = ZREL_V002.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZREL_V002 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZREL_V002.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZREL_V002-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZREL_T003 WHERE
  DEPT_SUBS = ZREL_V002-DEPT_SUBS AND
  EKGRP = ZREL_V002-EKGRP .
    IF SY-SUBRC = 0.
    DELETE ZREL_T003 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZREL_T003 WHERE
  DEPT_SUBS = ZREL_V002-DEPT_SUBS AND
  EKGRP = ZREL_V002-EKGRP .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZREL_T003.
    ENDIF.
ZREL_T003-MANDT =
ZREL_V002-MANDT .
ZREL_T003-DEPT_SUBS =
ZREL_V002-DEPT_SUBS .
ZREL_T003-EKGRP =
ZREL_V002-EKGRP .
    IF SY-SUBRC = 0.
    UPDATE ZREL_T003 ##WARN_OK.
    ELSE.
    INSERT ZREL_T003 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZREL_V002-UPD_FLAG,
STATUS_ZREL_V002-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZREL_V002.
  SELECT SINGLE * FROM ZREL_T003 WHERE
DEPT_SUBS = ZREL_V002-DEPT_SUBS AND
EKGRP = ZREL_V002-EKGRP .
ZREL_V002-MANDT =
ZREL_T003-MANDT .
ZREL_V002-DEPT_SUBS =
ZREL_T003-DEPT_SUBS .
ZREL_V002-EKGRP =
ZREL_T003-EKGRP .
    SELECT SINGLE * FROM T024 WHERE
EKGRP = ZREL_T003-EKGRP .
    IF SY-SUBRC EQ 0.
ZREL_V002-EKNAM =
T024-EKNAM .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V002-EKNAM .
    ENDIF.
    SELECT SINGLE * FROM ZREL_T002 WHERE
DEPT_SUBS = ZREL_T003-DEPT_SUBS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T002T WHERE
DEPT_SUBS = ZREL_T002-DEPT_SUBS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V002-DESCRIPTION =
ZREL_T002T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZREL_V002-DESCRIPTION .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V002-DESCRIPTION .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZREL_V002 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZREL_V002-DEPT_SUBS TO
ZREL_T003-DEPT_SUBS .
MOVE ZREL_V002-EKGRP TO
ZREL_T003-EKGRP .
MOVE ZREL_V002-MANDT TO
ZREL_T003-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZREL_T003'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZREL_T003 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZREL_T003'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZREL_V002 USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZREL_T003-MANDT =
ZREL_V002-MANDT .
ZREL_T003-DEPT_SUBS =
ZREL_V002-DEPT_SUBS .
ZREL_T003-EKGRP =
ZREL_V002-EKGRP .
    SELECT SINGLE * FROM T024 WHERE
EKGRP = ZREL_T003-EKGRP .
    IF SY-SUBRC EQ 0.
ZREL_V002-EKNAM =
T024-EKNAM .
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V002-EKNAM .
    ENDIF.
    SELECT SINGLE * FROM ZREL_T002 WHERE
DEPT_SUBS = ZREL_T003-DEPT_SUBS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T002T WHERE
DEPT_SUBS = ZREL_T002-DEPT_SUBS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V002-DESCRIPTION =
ZREL_T002T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZREL_V002-DESCRIPTION .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V002-DESCRIPTION .
    ENDIF.
ENDFORM.
*...processing: ZREL_V003.......................................*
FORM GET_DATA_ZREL_V003.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZREL_T005 WHERE
(VIM_WHERETAB) .
    CLEAR ZREL_V003 .
ZREL_V003-MANDT =
ZREL_T005-MANDT .
ZREL_V003-ROL =
ZREL_T005-ROL .
ZREL_V003-USERNAME =
ZREL_T005-USERNAME .
ZREL_V003-DEPT_SUBS =
ZREL_T005-DEPT_SUBS .
    SELECT SINGLE * FROM ZREL_T002 WHERE
DEPT_SUBS = ZREL_T005-DEPT_SUBS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T002T WHERE
DEPT_SUBS = ZREL_T002-DEPT_SUBS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V003-DEPT_DESCRIPTION =
ZREL_T002T-DESCRIPTION .
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM ZREL_T008 WHERE
ROL = ZREL_T005-ROL .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T008T WHERE
ROL = ZREL_T008-ROL AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V003-ROL_DESCRIPTION =
ZREL_T008T-DESCRIPTION .
      ENDIF.
    ENDIF.
<VIM_TOTAL_STRUC> = ZREL_V003.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZREL_V003 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZREL_V003.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZREL_V003-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZREL_T005 WHERE
  USERNAME = ZREL_V003-USERNAME AND
  DEPT_SUBS = ZREL_V003-DEPT_SUBS AND
  ROL = ZREL_V003-ROL .
    IF SY-SUBRC = 0.
    DELETE ZREL_T005 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZREL_T005 WHERE
  USERNAME = ZREL_V003-USERNAME AND
  DEPT_SUBS = ZREL_V003-DEPT_SUBS AND
  ROL = ZREL_V003-ROL .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZREL_T005.
    ENDIF.
ZREL_T005-MANDT =
ZREL_V003-MANDT .
ZREL_T005-ROL =
ZREL_V003-ROL .
ZREL_T005-USERNAME =
ZREL_V003-USERNAME .
ZREL_T005-DEPT_SUBS =
ZREL_V003-DEPT_SUBS .
    IF SY-SUBRC = 0.
    UPDATE ZREL_T005 ##WARN_OK.
    ELSE.
    INSERT ZREL_T005 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZREL_V003-UPD_FLAG,
STATUS_ZREL_V003-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZREL_V003.
  SELECT SINGLE * FROM ZREL_T005 WHERE
USERNAME = ZREL_V003-USERNAME AND
DEPT_SUBS = ZREL_V003-DEPT_SUBS AND
ROL = ZREL_V003-ROL .
ZREL_V003-MANDT =
ZREL_T005-MANDT .
ZREL_V003-ROL =
ZREL_T005-ROL .
ZREL_V003-USERNAME =
ZREL_T005-USERNAME .
ZREL_V003-DEPT_SUBS =
ZREL_T005-DEPT_SUBS .
    SELECT SINGLE * FROM ZREL_T002 WHERE
DEPT_SUBS = ZREL_T005-DEPT_SUBS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T002T WHERE
DEPT_SUBS = ZREL_T002-DEPT_SUBS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V003-DEPT_DESCRIPTION =
ZREL_T002T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZREL_V003-DEPT_DESCRIPTION .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V003-DEPT_DESCRIPTION .
    ENDIF.
    SELECT SINGLE * FROM ZREL_T008 WHERE
ROL = ZREL_T005-ROL .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T008T WHERE
ROL = ZREL_T008-ROL AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V003-ROL_DESCRIPTION =
ZREL_T008T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZREL_V003-ROL_DESCRIPTION .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V003-ROL_DESCRIPTION .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZREL_V003 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZREL_V003-USERNAME TO
ZREL_T005-USERNAME .
MOVE ZREL_V003-DEPT_SUBS TO
ZREL_T005-DEPT_SUBS .
MOVE ZREL_V003-ROL TO
ZREL_T005-ROL .
MOVE ZREL_V003-MANDT TO
ZREL_T005-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZREL_T005'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZREL_T005 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZREL_T005'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZREL_V003 USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZREL_T005-MANDT =
ZREL_V003-MANDT .
ZREL_T005-ROL =
ZREL_V003-ROL .
ZREL_T005-USERNAME =
ZREL_V003-USERNAME .
ZREL_T005-DEPT_SUBS =
ZREL_V003-DEPT_SUBS .
    SELECT SINGLE * FROM ZREL_T002 WHERE
DEPT_SUBS = ZREL_T005-DEPT_SUBS .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T002T WHERE
DEPT_SUBS = ZREL_T002-DEPT_SUBS AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V003-DEPT_DESCRIPTION =
ZREL_T002T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZREL_V003-DEPT_DESCRIPTION .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V003-DEPT_DESCRIPTION .
    ENDIF.
    SELECT SINGLE * FROM ZREL_T008 WHERE
ROL = ZREL_T005-ROL .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T008T WHERE
ROL = ZREL_T008-ROL AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V003-ROL_DESCRIPTION =
ZREL_T008T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZREL_V003-ROL_DESCRIPTION .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V003-ROL_DESCRIPTION .
    ENDIF.
ENDFORM.
*...processing: ZREL_V004.......................................*
FORM GET_DATA_ZREL_V004.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZREL_T006 WHERE
(VIM_WHERETAB) .
    CLEAR ZREL_V004 .
ZREL_V004-MANDT =
ZREL_T006-MANDT .
ZREL_V004-BACKUP_USER =
ZREL_T006-BACKUP_USER .
ZREL_V004-BACKUP_USER_DESC =
ZREL_T006-BACKUP_USER_DESC .
ZREL_V004-USERNAME =
ZREL_T006-USERNAME .
ZREL_V004-USERNAME_DESC =
ZREL_T006-USERNAME_DESC .
ZREL_V004-DATE_INI =
ZREL_T006-DATE_INI .
ZREL_V004-DATE_FIN =
ZREL_T006-DATE_FIN .
<VIM_TOTAL_STRUC> = ZREL_V004.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZREL_V004 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZREL_V004.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZREL_V004-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZREL_T006 WHERE
  BACKUP_USER = ZREL_V004-BACKUP_USER AND
  USERNAME = ZREL_V004-USERNAME .
    IF SY-SUBRC = 0.
    DELETE ZREL_T006 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZREL_T006 WHERE
  BACKUP_USER = ZREL_V004-BACKUP_USER AND
  USERNAME = ZREL_V004-USERNAME .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZREL_T006.
    ENDIF.
ZREL_T006-MANDT =
ZREL_V004-MANDT .
ZREL_T006-BACKUP_USER =
ZREL_V004-BACKUP_USER .
ZREL_T006-BACKUP_USER_DESC =
ZREL_V004-BACKUP_USER_DESC .
ZREL_T006-USERNAME =
ZREL_V004-USERNAME .
ZREL_T006-USERNAME_DESC =
ZREL_V004-USERNAME_DESC .
ZREL_T006-DATE_INI =
ZREL_V004-DATE_INI .
ZREL_T006-DATE_FIN =
ZREL_V004-DATE_FIN .
    IF SY-SUBRC = 0.
    UPDATE ZREL_T006 ##WARN_OK.
    ELSE.
    INSERT ZREL_T006 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZREL_V004-UPD_FLAG,
STATUS_ZREL_V004-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZREL_V004.
  SELECT SINGLE * FROM ZREL_T006 WHERE
BACKUP_USER = ZREL_V004-BACKUP_USER AND
USERNAME = ZREL_V004-USERNAME .
ZREL_V004-MANDT =
ZREL_T006-MANDT .
ZREL_V004-BACKUP_USER =
ZREL_T006-BACKUP_USER .
ZREL_V004-BACKUP_USER_DESC =
ZREL_T006-BACKUP_USER_DESC .
ZREL_V004-USERNAME =
ZREL_T006-USERNAME .
ZREL_V004-USERNAME_DESC =
ZREL_T006-USERNAME_DESC .
ZREL_V004-DATE_INI =
ZREL_T006-DATE_INI .
ZREL_V004-DATE_FIN =
ZREL_T006-DATE_FIN .
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZREL_V004 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZREL_V004-BACKUP_USER TO
ZREL_T006-BACKUP_USER .
MOVE ZREL_V004-USERNAME TO
ZREL_T006-USERNAME .
MOVE ZREL_V004-MANDT TO
ZREL_T006-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZREL_T006'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZREL_T006 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZREL_T006'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
*...processing: ZREL_V005.......................................*
FORM GET_DATA_ZREL_V005.
  PERFORM VIM_FILL_WHERETAB.
*.read data from database.............................................*
  REFRESH TOTAL.
  CLEAR   TOTAL.
  SELECT * FROM ZREL_T009 WHERE
(VIM_WHERETAB) .
    CLEAR ZREL_V005 .
ZREL_V005-MANDT =
ZREL_T009-MANDT .
ZREL_V005-ROL =
ZREL_T009-ROL .
ZREL_V005-AUTH =
ZREL_T009-AUTH .
    SELECT SINGLE * FROM ZREL_T007 WHERE
AUTH = ZREL_T009-AUTH .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T007T WHERE
AUTH = ZREL_T007-AUTH AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V005-DESCRIPTION_AUTH =
ZREL_T007T-DESCRIPTION .
      ENDIF.
    ENDIF.
    SELECT SINGLE * FROM ZREL_T008 WHERE
ROL = ZREL_T009-ROL .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T008T WHERE
ROL = ZREL_T008-ROL AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V005-DESCRIPTION_ROL =
ZREL_T008T-DESCRIPTION .
      ENDIF.
    ENDIF.
<VIM_TOTAL_STRUC> = ZREL_V005.
    APPEND TOTAL.
  ENDSELECT.
  SORT TOTAL BY <VIM_XTOTAL_KEY>.
  <STATUS>-ALR_SORTED = 'R'.
*.check dynamic selectoptions (not in DDIC)...........................*
  IF X_HEADER-SELECTION NE SPACE.
    PERFORM CHECK_DYNAMIC_SELECT_OPTIONS.
  ELSEIF X_HEADER-DELMDTFLAG NE SPACE.
    PERFORM BUILD_MAINKEY_TAB.
  ENDIF.
  REFRESH EXTRACT.
ENDFORM.
*---------------------------------------------------------------------*
FORM DB_UPD_ZREL_V005 .
*.process data base updates/inserts/deletes.........................*
LOOP AT TOTAL.
  CHECK <ACTION> NE ORIGINAL.
MOVE <VIM_TOTAL_STRUC> TO ZREL_V005.
  IF <ACTION> = UPDATE_GELOESCHT.
    <ACTION> = GELOESCHT.
  ENDIF.
  CASE <ACTION>.
   WHEN NEUER_GELOESCHT.
IF STATUS_ZREL_V005-ST_DELETE EQ GELOESCHT.
     READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
     IF SY-SUBRC EQ 0.
       DELETE EXTRACT INDEX SY-TABIX.
     ENDIF.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN GELOESCHT.
  SELECT SINGLE FOR UPDATE * FROM ZREL_T009 WHERE
  ROL = ZREL_V005-ROL AND
  AUTH = ZREL_V005-AUTH .
    IF SY-SUBRC = 0.
    DELETE ZREL_T009 .
    ENDIF.
    IF STATUS-DELETE EQ GELOESCHT.
      READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY> BINARY SEARCH.
      DELETE EXTRACT INDEX SY-TABIX.
    ENDIF.
    DELETE TOTAL.
    IF X_HEADER-DELMDTFLAG NE SPACE.
      PERFORM DELETE_FROM_MAINKEY_TAB.
    ENDIF.
   WHEN OTHERS.
  SELECT SINGLE FOR UPDATE * FROM ZREL_T009 WHERE
  ROL = ZREL_V005-ROL AND
  AUTH = ZREL_V005-AUTH .
    IF SY-SUBRC <> 0.   "insert preprocessing: init WA
      CLEAR ZREL_T009.
    ENDIF.
ZREL_T009-MANDT =
ZREL_V005-MANDT .
ZREL_T009-ROL =
ZREL_V005-ROL .
ZREL_T009-AUTH =
ZREL_V005-AUTH .
    IF SY-SUBRC = 0.
    UPDATE ZREL_T009 ##WARN_OK.
    ELSE.
    INSERT ZREL_T009 .
    ENDIF.
    READ TABLE EXTRACT WITH KEY <VIM_XTOTAL_KEY>.
    IF SY-SUBRC EQ 0.
      <XACT> = ORIGINAL.
      MODIFY EXTRACT INDEX SY-TABIX.
    ENDIF.
    <ACTION> = ORIGINAL.
    MODIFY TOTAL.
  ENDCASE.
ENDLOOP.
CLEAR: STATUS_ZREL_V005-UPD_FLAG,
STATUS_ZREL_V005-UPD_CHECKD.
MESSAGE S018(SV).
ENDFORM.
*---------------------------------------------------------------------*
FORM READ_SINGLE_ENTRY_ZREL_V005.
  SELECT SINGLE * FROM ZREL_T009 WHERE
ROL = ZREL_V005-ROL AND
AUTH = ZREL_V005-AUTH .
ZREL_V005-MANDT =
ZREL_T009-MANDT .
ZREL_V005-ROL =
ZREL_T009-ROL .
ZREL_V005-AUTH =
ZREL_T009-AUTH .
    SELECT SINGLE * FROM ZREL_T007 WHERE
AUTH = ZREL_T009-AUTH .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T007T WHERE
AUTH = ZREL_T007-AUTH AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V005-DESCRIPTION_AUTH =
ZREL_T007T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZREL_V005-DESCRIPTION_AUTH .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V005-DESCRIPTION_AUTH .
    ENDIF.
    SELECT SINGLE * FROM ZREL_T008 WHERE
ROL = ZREL_T009-ROL .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T008T WHERE
ROL = ZREL_T008-ROL AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V005-DESCRIPTION_ROL =
ZREL_T008T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZREL_V005-DESCRIPTION_ROL .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V005-DESCRIPTION_ROL .
    ENDIF.
ENDFORM.
*---------------------------------------------------------------------*
FORM CORR_MAINT_ZREL_V005 USING VALUE(CM_ACTION) RC.
  DATA: RETCODE LIKE SY-SUBRC, COUNT TYPE I, TRSP_KEYLEN TYPE SYFLENG.
  FIELD-SYMBOLS: <TAB_KEY_X> TYPE X.
  CLEAR RC.
MOVE ZREL_V005-ROL TO
ZREL_T009-ROL .
MOVE ZREL_V005-AUTH TO
ZREL_T009-AUTH .
MOVE ZREL_V005-MANDT TO
ZREL_T009-MANDT .
  CORR_KEYTAB             =  E071K.
  CORR_KEYTAB-OBJNAME     = 'ZREL_T009'.
  IF NOT <vim_corr_keyx> IS ASSIGNED.
    ASSIGN CORR_KEYTAB-TABKEY TO <vim_corr_keyx> CASTING.
  ENDIF.
  ASSIGN ZREL_T009 TO <TAB_KEY_X> CASTING.
  PERFORM VIM_GET_TRSPKEYLEN
    USING 'ZREL_T009'
    CHANGING TRSP_KEYLEN.
  <VIM_CORR_KEYX>(TRSP_KEYLEN) = <TAB_KEY_X>(TRSP_KEYLEN).
  PERFORM UPDATE_CORR_KEYTAB USING CM_ACTION RETCODE.
  ADD: RETCODE TO RC, 1 TO COUNT.
  IF RC LT COUNT AND CM_ACTION NE PRUEFEN.
    CLEAR RC.
  ENDIF.

ENDFORM.
*---------------------------------------------------------------------*
FORM COMPL_ZREL_V005 USING WORKAREA.
*      provides (read-only) fields from secondary tables related
*      to primary tables by foreignkey relationships
ZREL_T009-MANDT =
ZREL_V005-MANDT .
ZREL_T009-ROL =
ZREL_V005-ROL .
ZREL_T009-AUTH =
ZREL_V005-AUTH .
    SELECT SINGLE * FROM ZREL_T007 WHERE
AUTH = ZREL_T009-AUTH .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T007T WHERE
AUTH = ZREL_T007-AUTH AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V005-DESCRIPTION_AUTH =
ZREL_T007T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZREL_V005-DESCRIPTION_AUTH .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V005-DESCRIPTION_AUTH .
    ENDIF.
    SELECT SINGLE * FROM ZREL_T008 WHERE
ROL = ZREL_T009-ROL .
    IF SY-SUBRC EQ 0.
      SELECT SINGLE * FROM ZREL_T008T WHERE
ROL = ZREL_T008-ROL AND
SPRAS = SY-LANGU .
      IF SY-SUBRC EQ 0.
ZREL_V005-DESCRIPTION_ROL =
ZREL_T008T-DESCRIPTION .
      ELSE.
        CLEAR SY-SUBRC.
        CLEAR ZREL_V005-DESCRIPTION_ROL .
      ENDIF.
    ELSE.
      CLEAR SY-SUBRC.
      CLEAR ZREL_V005-DESCRIPTION_ROL .
    ENDIF.
ENDFORM.

* base table related FORM-routines.............
INCLUDE LSVIMFTX .