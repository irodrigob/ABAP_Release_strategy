CLASS zcl_rel_user DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_user_detail,
             fullname TYPE string,
             email    TYPE string,
           END OF ts_user_detail.
    TYPES: BEGIN OF ts_user_info.
        INCLUDE TYPE ts_user_detail.
    TYPES: decimals TYPE val_text,
           date     TYPE val_text,
           END OF ts_user_info.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING
        !iv_user  TYPE syuname DEFAULT sy-uname
        !iv_langu TYPE sylangu DEFAULT sy-langu .
    "! <p class="shorttext synchronized">Devuelve información del usuario</p>
    "! @parameter es_user_info | <p class="shorttext synchronized">Información</p>
    METHODS get_user_info
      EXPORTING
                es_user_info TYPE ts_user_info
      RAISING   zcx_rel.
    "! <p class="shorttext synchronized">Detalle del usuario</p>
    "! @parameter es_user_info | <p class="shorttext synchronized">Información</p>
    METHODS get_user_detail
      EXPORTING
                es_user_detail TYPE ts_user_detail
      RAISING   zcx_rel.
    "! <p class="shorttext synchronized">Devuelve los formato de separación de la USR01</p>
    "! @parameter ev_dflag | <p class="shorttext synchronized">Separamos decimal</p>
    "! @parameter ev_tflag | <p class="shorttext synchronized">Separamos miles</p>
    METHODS get_usr01_format
      EXPORTING
        ev_dflag TYPE syst_batch
        ev_tflag TYPE syst_batch.
  PROTECTED SECTION.
    DATA mv_langu TYPE sylangu.
    DATA mv_user TYPE syuname.
    "! <p class="shorttext synchronized">Register user</p>
    METHODS register_user.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_user IMPLEMENTATION.
  METHOD get_user_info.


    " Información al detalle de usuario
    get_user_detail(
      IMPORTING
        es_user_detail = DATA(ls_detail) ).

    es_user_info = CORRESPONDING #( ls_detail ).

* establecemos la configuración de fechas y decimales
    zcl_ca_utilidades=>get_user_date_en( EXPORTING user = mv_user
                                         IMPORTING date = es_user_info-date ).
    zcl_ca_utilidades=>get_user_decimals_en( EXPORTING user = mv_user
                                             IMPORTING decimals = es_user_info-decimals ).

    " Se registra el usuario en la tabla para saber cuando es la ultima vez que ha entrado
    register_user( ).
  ENDMETHOD.

  METHOD get_user_detail.

    CLEAR: es_user_detail.

    SELECT b~smtp_addr, c~langu, d~name_text INTO @DATA(ls_user_info) UP TO 1 ROWS
         FROM usr21 AS a
         LEFT OUTER JOIN adr6 AS b ON
             b~addrnumber = a~addrnumber
             AND b~persnumber = a~persnumber
        LEFT OUTER JOIN adrc AS c ON
             c~addrnumber = a~addrnumber
      LEFT OUTER JOIN adrp AS d ON
          d~persnumber = a~persnumber
         WHERE a~bname = @mv_user.
    ENDSELECT.
    IF sy-subrc = 0.
      es_user_detail-email = ls_user_info-smtp_addr.
      es_user_detail-fullname = ls_user_info-name_text.
    ELSE.
      RAISE EXCEPTION TYPE zcx_rel
        EXPORTING
          textid   = zcx_rel=>user_not_exist
          mv_msgv1 = CONV #( mv_user ).
    ENDIF.

  ENDMETHOD.

  METHOD constructor.
    mv_user      = iv_user.
    mv_langu     = iv_langu.
  ENDMETHOD.

  METHOD register_user.
    SELECT SINGLE * INTO @DATA(ls_user)
              FROM zrel_t001
              WHERE username = @mv_user.

    IF sy-subrc NE 0.
      ls_user = VALUE zapd_t007( username = mv_user
                                       aedat = sy-datum
                                       aezet = sy-uzeit ).


    ELSE.
      ls_user-date_last_conn = sy-datum.
      ls_user-time_last_conn = sy-uzeit.
    ENDIF.

    MODIFY zrel_t001 FROM ls_user.
    COMMIT WORK AND WAIT.
  ENDMETHOD.

  METHOD get_usr01_format.
    CALL FUNCTION 'CLSE_SELECT_USR01'
      EXPORTING
        iv_delete_buffer = 'X'
        username         = mv_user
      IMPORTING
        decimal_sign     = ev_dflag
        separator        = ev_tflag
      EXCEPTIONS
        OTHERS           = 0.
  ENDMETHOD.

ENDCLASS.
