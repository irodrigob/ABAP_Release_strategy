CLASS zcl_rel_general_master_data DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_rfc_system,
             sysid     TYPE syst_sysid,
             rfc       TYPE rfcdest,
             sys_type  TYPE zrel_e_sys_type,
             sys_level TYPE zrel_e_level,
           END OF ts_rfc_system.
    TYPES: tt_rfc_system TYPE STANDARD TABLE OF ts_rfc_system WITH DEFAULT KEY.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING
        iv_langu TYPE sylangu DEFAULT sy-langu.
    "! <p class="shorttext synchronized">Devuelve los usuarios de SAP</p>
    "! Estos usuarios se usarán para escoger compradores o aprobadores de estrategias.
    "! @parameter it_r_users | <p class="shorttext synchronized">Rango de usuarios</p>
    "! @parameter et_users | <p class="shorttext synchronized">Usuarios</p>
    METHODS get_system_users
      IMPORTING
                it_r_users TYPE zif_rel_data=>tt_r_username OPTIONAL
      EXPORTING et_users   TYPE zif_rel_data=>tt_users.
    "! <p class="shorttext synchronized">Devuelvo las RFC para las conexiones </p>
    "! @parameter iv_sys_type | <p class="shorttext synchronized">Tipo de sistema</p>
    "! @parameter rt_values | <p class="shorttext synchronized">Valores</p>
    METHODS get_rfc_system
      IMPORTING iv_sys_type      TYPE zrel_e_sys_type OPTIONAL
      RETURNING VALUE(rt_values) TYPE tt_rfc_system.

  PROTECTED SECTION.
    DATA mv_langu TYPE sylangu.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_general_master_data IMPLEMENTATION.
  METHOD constructor.
    mv_langu = sy-langu.
  ENDMETHOD.

  METHOD get_system_users.
    CLEAR: et_users.

    DATA(lt_r_users) = it_r_users.

    " Si no hay usuarios informados se añade al ranges los usuarios que se tienen que excluir
    IF lt_r_users IS INITIAL.
      SELECT 'E' AS sign, 'EQ' AS option, username AS low, username AS high
             INTO TABLE @lt_r_users
             FROM zrel_t016.
    ENDIF.


*    SELECT a~bname AS username d~name_text AS username_desc INTO TABLE et_users
*           FROM ( usr02 AS a INNER JOIN usr21 AS b ON
*                b~bname = a~bname ) LEFT OUTER JOIN adrp AS d ON
*                                    d~persnumber = b~persnumber
*           WHERE a~bname IN lt_r_users
*                 AND a~ustyp = zif_rel_data=>cs_general_master_data-users-dialog_user
*                 AND ( a~gltgb >= sy-datum OR a~gltgb = '00000000' ).
    SELECT username, username_desc
           FROM zrel_i_systems_user
           WHERE username IN @lt_r_users
                 AND ( gltgb >= @sy-datum OR gltgb = '00000000' )
                 INTO TABLE @et_users.

    LOOP AT et_users ASSIGNING FIELD-SYMBOL(<ls_users>).
      <ls_users>-username_desc_search = |{ <ls_users>-username_desc CASE = UPPER }|.
    ENDLOOP.
  ENDMETHOD.

  METHOD get_rfc_system.
    DATA lt_r_sys_type TYPE RANGE OF zrel_e_sys_type.

    IF iv_sys_type IS NOT INITIAL.
      lt_r_sys_type = VALUE #( ( sign = 'I' option = 'EQ' low = iv_sys_type ) ).
    ENDIF.

    SELECT sysid rfc sys_type sys_level INTO TABLE rt_values
           FROM zrel_t026
           WHERE sys_type IN lt_r_sys_type
           ORDER BY sys_level.

  ENDMETHOD.



ENDCLASS.
