CLASS zcl_rel_authorizations DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_users,
             username TYPE syuname,
           END OF ts_users.
    TYPES: tt_users TYPE STANDARD TABLE OF ts_users WITH DEFAULT KEY.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING iv_langu TYPE sylangu DEFAULT sy-langu.

    "! <p class="shorttext synchronized">Get user approvers</p>
    "! @parameter iv_dept_subs | <p class="shorttext synchronized">Department/subsidiary</p>
    "! @parameter et_users | <p class="shorttext synchronized">User approvers</p>
    METHODS get_user_approvers
      IMPORTING iv_dept_subs TYPE zrel_t005-dept_subs OPTIONAL
      EXPORTING et_users     TYPE zcl_rel_authorizations=>tt_users.

  PROTECTED SECTION.
    DATA mv_langu TYPE sylangu.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_authorizations IMPLEMENTATION.
  METHOD constructor.
    mv_langu = iv_langu.

  ENDMETHOD.

  METHOD get_user_approvers.

    CLEAR et_users.

    " Aquí tengo dos estrategias para obtenerlo:
    " 1) Mirar los roles con actividad aprobador o superio usuario y mirar que usuario lo tienen y teniendo en
    " cuenta el departamento/filial pasado
    " 2) Leer los usuario por el departamento/filial pasado por parámetro e ir lanzando el authority check,
    " Lo practico es el 1, pero voy hacer el 2 porque así como lo haría SAP (que no signifique que sea lo mejor) y porque me
    " va a permitir que si hago cambios en el modelo de autorizaciones no me afecte. Ya que será un proceso centralizado y no disperso
    " en varios sitios.

    " Primero sacamos los usuarios asignados directamente al departamento.
    SELECT username INTO TABLE @DATA(lt_users)
                 FROM zrel_t005
                 WHERE dept_subs = @iv_dept_subs.

    " Y ahora saco los generales, es decir, los que ven todo.
    SELECT username APPENDING TABLE lt_users
                   FROM zrel_t005
                   WHERE dept_subs = space.

    LOOP AT lt_users ASSIGNING FIELD-SYMBOL(<ls_users>).
      DATA(lo_auth) = NEW zcl_rel_user_authorizations( iv_user = <ls_users>-username ).

      IF lo_auth->authority_check( iv_actvt = zif_rel_auth_data=>cs_actvt_auth_check-approver iv_dept_subs = iv_dept_subs ).
        INSERT VALUE #( username = <ls_users>-username ) INTO TABLE et_users.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

ENDCLASS.
