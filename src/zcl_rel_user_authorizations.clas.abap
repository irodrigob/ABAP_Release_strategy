CLASS zcl_rel_user_authorizations DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: tt_r_rol TYPE RANGE OF zrel_e_rol.
    CLASS-DATA mv_rol_admin TYPE zrel_e_rol READ-ONLY .
    CLASS-DATA mv_rol_view TYPE zrel_e_rol READ-ONLY .
    CLASS-DATA mv_rol_approver TYPE zrel_e_rol READ-ONLY .


    "! <p class="shorttext synchronized">Constructor de clase</p>
    "! @parameter user | <p class="shorttext synchronized">Usuarios</p>
    CLASS-METHODS class_constructor .
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    METHODS constructor
      IMPORTING
        iv_langu TYPE sylangu DEFAULT sy-langu
        iv_user  TYPE syuname DEFAULT sy-uname.
    "! <p class="shorttext synchronized">Verificación autorización según actividad</p>
    "! @parameter iv_dept_subs | <p class="shorttext synchronized">Idioma</p>
    "! @parameter rv_auth | <p class="shorttext synchronized">Tiene autorización</p>
    METHODS authority_check
      IMPORTING iv_dept_subs   TYPE zrel_t005-dept_subs OPTIONAL
                iv_actvt       TYPE zrel_t008-actvt DEFAULT zif_rel_auth_data=>cs_actvt_auth_check-view
      RETURNING VALUE(rv_auth) TYPE sap_bool.
    "! <p class="shorttext synchronized">El usuario es administrador</p>
    "! @parameter iv_dept_subs | <p class="shorttext synchronized">Deparamento/filial</p>
    "! @parameter rv_is | <p class="shorttext synchronized">Lo es</p>
    METHODS is_admin
      IMPORTING iv_dept_subs TYPE zrel_t005-dept_subs OPTIONAL
      RETURNING VALUE(rv_is) TYPE sap_bool.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_user_rol,
             dept_subs TYPE zrel_t005-dept_subs,
             rol       TYPE zrel_t005-rol,
           END OF ts_user_rol.
    TYPES: tt_user_rol TYPE STANDARD TABLE OF ts_user_rol WITH EMPTY KEY.
    TYPES: BEGIN OF ts_rol_actvt,
             rol   TYPE zrel_t008-rol,
             actvt TYPE zrel_t008-actvt,
           END OF ts_rol_actvt.
    TYPES: tt_rol_actvt TYPE STANDARD TABLE OF ts_rol_actvt WITH EMPTY KEY.
    CONSTANTS: BEGIN OF cs_internal_auth,
                 all_actvt TYPE activ_auth VALUE '*',
               END OF cs_internal_auth.
    DATA mv_user TYPE syuname.
    DATA mv_langu TYPE sylangu.
    DATA mt_user_rol TYPE tt_user_rol.
    DATA mt_rol_actvt TYPE tt_rol_actvt.
    "! <p class="shorttext synchronized">Lectuta de los roles del usuario</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    METHODS load_user_rol
      IMPORTING
        iv_user TYPE syuname.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_user_authorizations IMPLEMENTATION.
  METHOD class_constructor.
    " Lectura de los roles segun la tabla de constantes
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'AUTH_ROL_ADMIN'
                                           IMPORTING ev_value    = mv_rol_admin ).
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'AUTH_ROL_VIEW'
                                               IMPORTING ev_value    = mv_rol_view ).
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'AUTH_ROL_APPROVER'
                                               IMPORTING ev_value    = mv_rol_approver ).
  ENDMETHOD.
  METHOD constructor.
    mv_langu = iv_langu.
    mv_user = iv_user.

    " lectura de los roles del usuario
    load_user_rol( EXPORTING iv_user = iv_user ).

  ENDMETHOD.

  METHOD authority_check.

    rv_auth = abap_false. " Sin autorización

    " Leemos que roles del usuario tiene la actividad pasada por parámetro y los roles donde la actividad sea '*', acceso a todo.
    DATA(lt_r_roles) = VALUE tt_r_rol( FOR <wa> IN mt_rol_actvt WHERE ( actvt = iv_actvt )
                                                         ( sign = 'I' option = 'EQ' low = <wa>-rol ) ).
    IF lt_r_roles IS NOT INITIAL.

      " Ahora se ha de evaluar el departamento pasado, si no se ha informado el departamento se asume que tiene autorización.
      IF iv_dept_subs IS SUPPLIED.

        " Miramos si algun de los roles con la actividad pasada esta asociado al departamento pasado
        LOOP AT mt_user_rol TRANSPORTING NO FIELDS WHERE dept_subs = iv_dept_subs
                                                         AND rol IN lt_r_roles.
          EXIT.
        ENDLOOP.
        IF sy-subrc = 0.
          rv_auth = abap_true.
        ELSE.
          " Si no tiene rol a nivel de departamento miramos si lo tiene a nivel general.
          LOOP AT mt_user_rol TRANSPORTING NO FIELDS WHERE dept_subs = space
                                                           AND rol IN lt_r_roles.
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            rv_auth = abap_true.
          ENDIF.
        ENDIF.

      ELSE.
        rv_auth = abap_true.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD load_user_rol.
    CLEAR: mt_rol_actvt, mt_user_rol.

    " Leo los roles asignados al usuario
    SELECT dept_subs rol INTO TABLE mt_user_rol
           FROM zrel_t005
           WHERE username = mv_user.
    IF sy-subrc = 0.
      " Las actividades de los roles del usuario
      SELECT rol actvt INTO TABLE mt_rol_actvt
            FROM zrel_t008
            FOR ALL ENTRIES IN mt_user_rol
            WHERE rol = mt_user_rol-rol.

      IF mt_rol_actvt IS NOT INITIAL.

        " Aquellos roles donde tenga un '*' voy a splitearlos por las actividades configuradas
        LOOP AT mt_rol_actvt ASSIGNING FIELD-SYMBOL(<ls_rol_actvt>) WHERE actvt = cs_internal_auth-all_actvt.
          DATA(lv_tabix) = sy-tabix.
          DATA(ls_rol_actvt) = <ls_rol_actvt>.
          DO.

            ASSIGN COMPONENT sy-index OF STRUCTURE zif_rel_auth_data=>cs_actvt_auth_check TO FIELD-SYMBOL(<value>).
            IF sy-subrc = 0.
              ls_rol_actvt-actvt = <value>.
              INSERT ls_rol_actvt INTO TABLE mt_rol_actvt.
            ELSE.
              EXIT.
            ENDIF.
          ENDDO.
          " Borro el astericos
          DELETE mt_rol_actvt INDEX lv_tabix.
        ENDLOOP.
      ENDIF.
    ENDIF.

  ENDMETHOD.
  METHOD is_admin.

    " Si el departamento/filial esta informado busco si tiene el rol de administrador para ese departamento.
    " Si no lo tiene se mira si a nivel general lo tiene
    IF iv_dept_subs IS NOT INITIAL.
      rv_is = COND #( WHEN line_exists( mt_user_rol[ dept_subs = iv_dept_subs
                                                     rol = mv_rol_admin ] )
                      THEN abap_true
                      ELSE COND #( WHEN line_exists( mt_user_rol[ dept_subs = space
                                                     rol = mv_rol_admin ] )
                                   THEN abap_true ELSE abap_false ) ) .

    ELSE. " Si no tiene se mira a nivel general

      rv_is = COND #( WHEN line_exists( mt_user_rol[ dept_subs = space
                                                           rol = mv_rol_admin ] )
                                         THEN abap_true ELSE abap_false ).
    ENDIF.
  ENDMETHOD.

ENDCLASS.
