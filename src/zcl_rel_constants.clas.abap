CLASS zcl_rel_constants DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

*"* public components of class ZCL_REL_CONSTANTS
*"* do not include other source files here!!!
    CLASS-METHODS obtener_constantes
      IMPORTING
        !iv_constant TYPE brf_constant
      EXPORTING
        !ev_value    TYPE any .
    CLASS-METHODS obtener_constantes_programa
      IMPORTING
        !iv_pattern TYPE brf_constant OPTIONAL
      EXPORTING
        !et_values  TYPE table .
    CLASS-METHODS obtener_constantes_en_ranges
      IMPORTING
        !iv_pattern TYPE brf_constant
        !iv_option  TYPE any DEFAULT 'EQ'
        !iv_sign    TYPE any DEFAULT 'I'
      CHANGING
        !ct_ranges  TYPE STANDARD TABLE .
    CLASS-METHODS get_constante_en_tabla
      IMPORTING
        !iv_pattern TYPE brf_constant
      CHANGING
        !ct_table   TYPE ANY TABLE .
  PROTECTED SECTION.
*"* protected components of class ZCL_REL_CONSTANTS
*"* do not include other source files here!!!
  PRIVATE SECTION.
*"* private components of class ZCL_REL_CONSTANTS
*"* do not include other source files here!!!
ENDCLASS.



CLASS zcl_rel_constants IMPLEMENTATION.


  METHOD get_constante_en_tabla.

*  zcl_ca_constants=>get_constante_en_tabla(
*    EXPORTING
*      i_id     = zif_rel_data=>cv_id_constant
*      i_patron = iv_pattern
*    CHANGING
*      c_tabla  = ct_table ).

    SELECT value FROM zrel_t000 INTO TABLE ct_table
           WHERE constant LIKE iv_pattern
           ORDER BY PRIMARY KEY.

  ENDMETHOD.


  METHOD obtener_constantes.

*    zcl_ca_constants=>obtener_constantes(
*      EXPORTING
*        i_id        = zif_rel_data=>cv_id_constant
*        i_constante = iv_constant
*      IMPORTING
*        c_valor     = ev_value ).

    DATA ld_valor TYPE zze_valor_cte.

    CLEAR ev_value.

    SELECT SINGLE value FROM zrel_t000 INTO @DATA(lv_value)
           WHERE constant = @iv_constant.
    IF sy-subrc = 0.
      ev_value = lv_value.
    ENDIF.

  ENDMETHOD.


  METHOD obtener_constantes_en_ranges.

*    zcl_ca_constants=>obtener_constantes_en_ranges(
*      EXPORTING
*        i_id     = zif_rel_data=>cv_id_constant
*        i_patron = iv_pattern
*        i_option = iv_option
*        i_sign   = iv_sign
*      CHANGING
*        t_ranges = ct_ranges ).

* Uso: Rellena la tabla tipo ranges T_RANGES con los valores en la tabla ZZT_CA0401
* que cumplan ID = i_id y CONSTANTE = i_patron, donde i_patron puede ser una expresión
* del tipo 'BUKRS%', por ejemplo.


* Uso: Rellena la tabla tipo ranges T_RANGES con los valores en la tabla ZZT_CA0401
* que cumplan ID = i_id y CONSTANTE = i_patron, donde i_patron puede ser una expresión
* del tipo 'BUKRS%', por ejemplo.
    DATA it_valores        TYPE TABLE OF zze_valor_cte.
    DATA d_valor           TYPE zze_valor_cte.
    DATA: dref             TYPE REF TO data.
    FIELD-SYMBOLS: <wa>    TYPE any,
                   <campo> TYPE any.

* Obtener los valores de la tabla de constantes
    SELECT value FROM zrel_t000 INTO TABLE it_valores
           WHERE constant LIKE iv_pattern.

    IF sy-subrc = 0.
*   Crear una Work area del mismo tipo que una linea del ranges pasado por parámetro
      CREATE DATA dref LIKE LINE OF ct_ranges.
      ASSIGN dref->* TO <wa>.

*   Asignamos y llenamos campo Sign
      ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <wa> TO <campo>.
      IF sy-subrc = 0.
        <campo> = iv_sign.
      ENDIF.

*   Asignamos y llenamos campo Option
      ASSIGN COMPONENT 'OPTION' OF STRUCTURE <wa> TO <campo>.
      IF sy-subrc = 0.
        <campo> = iv_option.
      ENDIF.

*   Asignamos campo Low
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <wa> TO <campo>.
      IF sy-subrc = 0.

*     Asignar todos los valores y llenar la tabla ranges.
        LOOP AT it_valores INTO d_valor.

          <campo> = d_valor.
          APPEND <wa> TO ct_ranges.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD obtener_constantes_programa.


*    zcl_ca_constants=>obtener_constantes_programa(
*      EXPORTING
*        i_id      = zif_rel_data=>cv_id_constant
*        i_patron  = iv_pattern
*      IMPORTING
*        c_valores = et_values ).

    IF iv_pattern IS NOT SUPPLIED.
      SELECT * FROM zzt_ca_constants INTO TABLE et_values.
    ELSE.
      SELECT * FROM zzt_ca_constants INTO TABLE et_values
            WHERE constante LIKE iv_pattern.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
