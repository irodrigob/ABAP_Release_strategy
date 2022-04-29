class ZCL_REL_CONSTANTS definition
  public
  final
  create public .

public section.

*"* public components of class ZCL_REL_CONSTANTS
*"* do not include other source files here!!!
  class-methods OBTENER_CONSTANTES
    importing
      !IV_CONSTANT type BRF_CONSTANT
    exporting
      !EV_VALUE type ANY .
  class-methods OBTENER_CONSTANTES_PROGRAMA
    importing
      !IV_PATTERN type BRF_CONSTANT optional
    exporting
      !ET_VALUES type TABLE .
  class-methods OBTENER_CONSTANTES_EN_RANGES
    importing
      !IV_PATTERN type BRF_CONSTANT
      !IV_OPTION type ANY default 'EQ'
      !IV_SIGN type ANY default 'I'
    changing
      !CT_RANGES type STANDARD TABLE .
  class-methods GET_CONSTANTE_EN_TABLA
    importing
      !IV_PATTERN type BRF_CONSTANT
    changing
      !CT_TABLE type ANY TABLE .
protected section.
*"* protected components of class ZCL_REL_CONSTANTS
*"* do not include other source files here!!!
private section.
*"* private components of class ZCL_REL_CONSTANTS
*"* do not include other source files here!!!
ENDCLASS.



CLASS ZCL_REL_CONSTANTS IMPLEMENTATION.


METHOD get_constante_en_tabla.

  zcl_ca_constants=>get_constante_en_tabla(
    EXPORTING
      i_id     = zif_rel_data=>cv_id_constant
      i_patron = iv_pattern
    CHANGING
      c_tabla  = ct_table ).

ENDMETHOD.


METHOD obtener_constantes.

  zcl_ca_constants=>obtener_constantes(
    EXPORTING
      i_id        = zif_rel_data=>cv_id_constant
      i_constante = iv_constant
    IMPORTING
      c_valor     = ev_value ).

ENDMETHOD.


METHOD obtener_constantes_en_ranges.

  zcl_ca_constants=>obtener_constantes_en_ranges(
    EXPORTING
      i_id     = zif_rel_data=>cv_id_constant
      i_patron = iv_pattern
      i_option = iv_option
      i_sign   = iv_sign
    CHANGING
      t_ranges = ct_ranges ).

ENDMETHOD.


METHOD obtener_constantes_programa.


  zcl_ca_constants=>obtener_constantes_programa(
    EXPORTING
      i_id      = zif_rel_data=>cv_id_constant
      i_patron  = iv_pattern
    IMPORTING
      c_valores = et_values ).

ENDMETHOD.
ENDCLASS.
