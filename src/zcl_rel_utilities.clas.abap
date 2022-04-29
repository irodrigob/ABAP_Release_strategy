CLASS zcl_rel_utilities DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">Convierte un parámetro de seleccion en ranges</p>
    "! @parameter iv_name | <p class="shorttext synchronized">Nombre</p>
    "! @parameter it_params_sl | <p class="shorttext synchronized">Parámetros de seleccion</p>
    "! @parameter iv_apply_conversion | <p class="shorttext synchronized">Aplica rutina de conversion</p>
    "! @parameter et_r_ranges | <p class="shorttext synchronized">Ranges con los valores</p>
    CLASS-METHODS conv_params_2_ranges
      IMPORTING
        !iv_name             TYPE any
        !it_params_sl        TYPE pivb_rsparamsl_255_t
        !iv_apply_conversion TYPE abap_bool DEFAULT abap_false
      EXPORTING
        !et_r_ranges         TYPE STANDARD TABLE .
    "! <p class="shorttext synchronized">Convierte el valor interno de un carácterística a externo</p>
    "! Este código se basa en la función del mismo nombre del método. No se usa la función porque hace una validación
    "! final que provoca un error aunque se haya formateado bien en los pasos previos.
    "! Eso si, hago ciertas mejoras para simplificar llamadas
    "! @parameter iv_charactname | <p class="shorttext synchronized">Nombre de la característica</p>
    "! @parameter iv_charctinn | <p class="shorttext synchronized">Nombre interno de la característica</p>
    "! @parameter iv_value_from | <p class="shorttext synchronized">Valor desde</p>
    "! @parameter iv_value_to | <p class="shorttext synchronized">Valor hasta</p>
    "! @parameter iv_value_relation | <p class="shorttext synchronized">Código de relación</p>
    "! @parameter ev_value | <p class="shorttext synchronized">Valor formateado</p>
    "! @parameter ev_value_operand | <p class="shorttext synchronized">Operando: mayor o igual, menor, etc.</p>
    "! @parameter ev_value_amount | <p class="shorttext synchronized">Importe desde</p>
    "! @parameter ev_value_amount2 | <p class="shorttext synchronized">Importe hasta</p>
    "! @parameter ev_currency | <p class="shorttext synchronized">Moneda</p>
    CLASS-METHODS ctbp_convert_value_int_to_ext
      IMPORTING
        iv_charactname       TYPE    bapicharactkey-charactname OPTIONAL
        iv_charctinn         TYPE ausp-atinn OPTIONAL
        iv_value_from        TYPE    atflv
        iv_value_to          TYPE    atflb
        iv_value_relation    TYPE    atcod
      EXPORTING
        ev_value_formatted   TYPE atwrt
        ev_value_operand     TYPE zrel_e_strategy_operand
        ev_value_amount_from TYPE zrel_e_strategy_amount
        ev_value_amount_to   TYPE zrel_e_strategy_amount
        ev_currency          TYPE waers.
    "! <p class="shorttext synchronized">Convierte el valor externo de un carácterística a interno</p>
    "! Este código se basa en la función del mismo nombre del método. Aunque internamente se usa
    "! la función estándar la encapsula para adaptarla mejor a mi proceso
    "! @parameter iv_charactname | <p class="shorttext synchronized">Nombre de la característica</p>
    "! @parameter iv_charctinn | <p class="shorttext synchronized">Nombre interno de la característica</p>
    "! @parameter iv_amount | <p class="shorttext synchronized">Importe desde</p>
    "! @parameter iv_amount2 | <p class="shorttext synchronized">Importe hasta</p>
    "! @parameter iv_operand | <p class="shorttext synchronized">Operando</p>
    "! @parameter iv_currency | <p class="shorttext synchronized">Moneda</p>
    "! @parameter iv_value_formatted | <p class="shorttext synchronized">Valor formateado</p>
    "! @parameter ev_value_from | <p class="shorttext synchronized">Valor desde convertido</p>
    "! @parameter ev_value_to | <p class="shorttext synchronized">Valor hasta convertido</p>
    "! @parameter ev_value_relation | <p class="shorttext synchronized">Código de relación</p>
    CLASS-METHODS ctbp_convert_value_ext_to_int
      IMPORTING
        iv_charactname     TYPE    bapicharactkey-charactname OPTIONAL
        iv_charctinn       TYPE ausp-atinn OPTIONAL
        iv_amount          TYPE zrel_bo_sc_strategy_amount-amount OPTIONAL
        iv_amount2         TYPE zrel_bo_sc_strategy_amount-amount OPTIONAL
        iv_operand         TYPE zrel_bo_sc_strategy_amount-amount_operand OPTIONAL
        iv_currency        TYPE zrel_bo_sc_strategy_amount-currency OPTIONAL
        iv_value_formatted TYPE atwrt OPTIONAL
      EXPORTING
        ev_value_from      TYPE    atflv
        ev_value_to        TYPE    atflb
        ev_value_relation  TYPE    atcod.

    "! <p class="shorttext synchronized">Transfiere la solicitud de strategia al modelo de estrategia</p>
    "! Este metodo transfiere el modelo de estrategias de una solicitud de cambio al modelo de salida
    "! de estrategias
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter request_data | <p class="shorttext synchronized">Datos</p>
    "! @parameter return | <p class="shorttext synchronized">Retorno del proceso</p>
    "! @parameter request_id | <p class="shorttext synchronized">ID de petición</p>
    CLASS-METHODS transfer_req_data_2_strag_data
      IMPORTING
        is_request_data  TYPE zcl_rel_strategy_chnge_request=>ts_pgroup_request_data
      CHANGING
        cs_strategy_data TYPE zif_rel_data=>ts_pgroup_all_data.
    "! <p class="shorttext synchronized">Formate los importes de la clasificación a char</p>
    "! @parameter iv_amount | <p class="shorttext synchronized">Importe desde</p>
    "! @parameter iv_amount2 | <p class="shorttext synchronized">Importe hasta</p>
    "! @parameter iv_operand | <p class="shorttext synchronized">Operando</p>
    "! @parameter iv_currency | <p class="shorttext synchronized">Moneda</p>
    "! @parameter rv_amount_char | <p class="shorttext synchronized">Importe formateado</p>
    CLASS-METHODS format_classif_amount_char
      IMPORTING
        iv_amount             TYPE zrel_bo_sc_strategy_amount-amount
        iv_amount2            TYPE zrel_bo_sc_strategy_amount-amount
        iv_operand            TYPE zrel_bo_sc_strategy_amount-amount_operand
        iv_currency           TYPE zrel_bo_sc_strategy_amount-currency
      RETURNING
        VALUE(rv_amount_char) TYPE zrel_bo_sc_strategy_amount-amount_char.
    "! <p class="shorttext synchronized">Convierte el codigo interno de la característica al externo</p>
    "! @parameter iv_atinn | <p class="shorttext synchronized">Código interno</p>
    "! @parameter rv_atnam | <p class="shorttext synchronized">Código externo</p>
    CLASS-METHODS conv_atinn_2_atnam
      IMPORTING iv_atinn        TYPE atinn
      RETURNING VALUE(rv_atnam) TYPE atnam.
  PROTECTED SECTION.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_utilities IMPLEMENTATION.
  METHOD conv_params_2_ranges.
    FIELD-SYMBOLS <ls_params> TYPE LINE OF pivb_rsparamsl_255_t.
    FIELD-SYMBOLS <campo> TYPE any.
    FIELD-SYMBOLS <ls_ranges> TYPE any.
    DATA: ls_ranges TYPE REF TO data.

    CLEAR: et_r_ranges.

*   Crear una Work area del mismo tipo que una linea del ranges pasado por parámetro
    CREATE DATA ls_ranges LIKE LINE OF et_r_ranges.
    ASSIGN ls_ranges->* TO <ls_ranges>.

*   buscamos la mascara de edición adecuada:
    IF iv_apply_conversion EQ abap_true.
      ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_ranges> TO <campo>.
      IF sy-subrc EQ 0.
        DATA(lo_elem_descr) = CAST cl_abap_elemdescr( cl_abap_datadescr=>describe_by_data( <campo> ) ).
        IF lo_elem_descr->edit_mask IS NOT INITIAL.
          DATA(lv_fm_name) = |CONVERSION_EXIT_{ lo_elem_descr->edit_mask+2 }_INPUT|.
        ENDIF.
      ENDIF.
      UNASSIGN <campo>.
    ENDIF.

    CLEAR et_r_ranges.
    LOOP AT it_params_sl ASSIGNING <ls_params> WHERE selname = iv_name
                                                  AND ( low IS NOT INITIAL OR
                                                        option IS NOT INITIAL ).

      CASE <ls_params>-kind.
        WHEN 'S'.

          IF <ls_params>-sign IS NOT INITIAL AND <ls_params>-option IS NOT INITIAL.
*   Asignamos campo Low. Si esta en blanco, no proceso el registro.
            ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_ranges> TO <campo>.
            IF sy-subrc = 0.

              IF iv_apply_conversion EQ abap_true AND lv_fm_name IS NOT INITIAL.
                CALL FUNCTION lv_fm_name
                  EXPORTING
                    input  = <ls_params>-low
                  IMPORTING
                    output = <campo>
                  EXCEPTIONS
                    OTHERS = 1.
              ELSE.
                <campo> = <ls_params>-low.
              ENDIF.

*   Asignamos y llenamos campo Sign
              ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <ls_ranges> TO <campo>.
              IF sy-subrc = 0.
                <campo> = <ls_params>-sign.
              ENDIF.

*   Asignamos y llenamos campo Option
              ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_ranges> TO <campo>.
              IF sy-subrc = 0.
                <campo> = <ls_params>-option.
              ENDIF.

*   Asignamos campo high
              ASSIGN COMPONENT 'HIGH' OF STRUCTURE <ls_ranges> TO <campo>.
              IF sy-subrc = 0.
                IF <ls_params>-high IS NOT INITIAL.
                  IF iv_apply_conversion EQ abap_true AND lv_fm_name IS NOT INITIAL.
                    CALL FUNCTION lv_fm_name
                      EXPORTING
                        input  = <ls_params>-low
                      IMPORTING
                        output = <campo>
                      EXCEPTIONS
                        OTHERS = 1.
                  ELSE.
                    <campo> = <ls_params>-high.
                  ENDIF.
                ENDIF.
              ENDIF.

              APPEND <ls_ranges> TO et_r_ranges.

            ENDIF.

          ENDIF.
        WHEN 'P'.
*   Asignamos y llenamos campo Sign
          ASSIGN COMPONENT 'SIGN'   OF STRUCTURE <ls_ranges> TO <campo>.
          IF sy-subrc = 0.
            <campo> = 'I'.
          ENDIF.
*   Asignamos y llenamos campo Option
          ASSIGN COMPONENT 'OPTION' OF STRUCTURE <ls_ranges> TO <campo>.
          IF sy-subrc = 0.
            <campo> = 'EQ'.
          ENDIF.
*   Asignamos y llenamos campo Option
          ASSIGN COMPONENT 'LOW' OF STRUCTURE <ls_ranges> TO <campo>.
          IF sy-subrc = 0.
            IF iv_apply_conversion EQ abap_true AND lv_fm_name IS NOT INITIAL.
              CALL FUNCTION lv_fm_name
                EXPORTING
                  input  = <ls_params>-low
                IMPORTING
                  output = <campo>
                EXCEPTIONS
                  OTHERS = 1.
            ELSE.
              <campo> = <ls_params>-low.
            ENDIF.
          ENDIF.
          APPEND <ls_ranges> TO et_r_ranges.
      ENDCASE.

    ENDLOOP.
  ENDMETHOD.

  METHOD ctbp_convert_value_int_to_ext.

    DATA lv_atnam TYPE atnam.
    DATA lt_cabn TYPE tt_cabn.
    DATA lv_amount_char TYPE c LENGTH 30.

    CLEAR: ev_value_formatted,ev_value_amount_from, ev_value_operand,ev_currency.
    CLEAR: ev_value_amount_to.

    " El nombre de la característica se puede pasar el nombre externo o el interno.
    IF iv_charactname IS NOT INITIAL.
      lv_atnam = iv_charactname.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
        EXPORTING
          input  = iv_charctinn
        IMPORTING
          output = lv_atnam.
    ENDIF.

    " Leo el formato de la característica
    CALL FUNCTION 'CLSE_SELECT_CABN_VIA_NAME'
      EXPORTING
        characteristic = lv_atnam
        key_date       = sy-datum    "#EC DOM_EQUAL
      TABLES
        t_cabn         = lt_cabn
      EXCEPTIONS
        no_entry_found = 1
        OTHERS         = 2.

    IF sy-subrc = 0.
      " Leo el primer registro donde esta los valores. Solo habrá uno.
      READ TABLE lt_cabn INDEX 1 INTO DATA(ls_cabn).

      IF     ls_cabn-atfor EQ 'NUM'
          OR ls_cabn-atfor EQ 'CURR'.
        CALL FUNCTION 'CTCV_TRANSLATE_TO_USER_MASK'
          EXPORTING
            iv_db_mask   = ls_cabn-atsch
          IMPORTING
            ev_user_mask = ls_cabn-atsch.
      ENDIF.

      " Valores a convertir
      DATA(ls_cawn) = VALUE cawn( atflv = iv_value_from
                                  atflb = iv_value_to
                                  atcod = iv_value_relation ).

      " Se convierte el valor
      CALL FUNCTION 'CTCV_PREPARE_VALUES_TO_DISPLAY'
        EXPORTING
*         ALIGN                = 'NO'
*         CONDENSE             = 'NO'
          decimalpoint         = '.' " El importe en formato interno
*         SHIFT                = 'LEFT'
*         SINGLE               = 'NO'
*         STRING_WITHOUT_OPERAND = 'NO'
*         STRING_WITHOUT_UNIT  = 'NO'
          string_with_baseunit = 'YES'
          structure_cabn       = ls_cabn
          structure_cawn       = ls_cawn
*         WITHOUT_EDIT_MASK    = ' '
*         LANGUAGE             = SY-LANGU
*         CLASSTYPE            = ' '
        IMPORTING
          operand1             = ev_value_operand
*         operand2             = lv_operand2
          string               = ev_value_formatted
          string1              = ev_value_amount_from
          string2              = ev_value_amount_to
*         UNIT2                =
*         STRING_LONG          =
*         STRING_TOO_LONG      =
        EXCEPTIONS
          overflow             = 1
          exp_overflow         = 2
          unit_not_found       = 3
          OTHERS               = 4.

      " Ahora hay que mirar el tipo de relacion porque hay una, la de intervalos, que hay que procesarla de distinta manera.
      IF iv_value_relation = zif_rel_data=>cs_strategy-classification-value_relation-between.
        " El operando en este caso no lo devuelve la función así que le pongo el "-" para identificarlo
        SPLIT ev_value_formatted AT space INTO: DATA(lv_amount1) ev_value_operand DATA(lv_amount2) ev_currency.

        WRITE ev_value_amount_from CURRENCY ev_currency TO lv_amount_char LEFT-JUSTIFIED.
        ev_value_formatted = |{ lv_amount_char } { ev_currency } { ev_value_operand }|.
        WRITE ev_value_amount_to CURRENCY ev_currency TO lv_amount_char LEFT-JUSTIFIED.
        ev_value_formatted = |{ ev_value_formatted } { lv_amount_char } { ev_currency }|.

      ELSE.
        " El valor formateado lo troceo en tres partes porque lo que voy hacer es formatear el importe con separador de miles y decimales
        " para que el usuario lo veo en su formato configurado.
        SPLIT ev_value_formatted AT space INTO: DATA(lv_operand) DATA(lv_amount) ev_currency.

        WRITE ev_value_amount_from CURRENCY ev_currency TO lv_amount_char LEFT-JUSTIFIED.
        ev_value_formatted = |{ lv_operand } { lv_amount_char } { ev_currency }|.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD transfer_req_data_2_strag_data.
    " Copiamos los datos base pero ignoramos los campos que son estructuras. El motivo es que
    " en ambos sitios se llaman igual pero en los datos de strategia tienen que ir a un campo distinto
    zcl_ca_utilities=>clone_structure_values( EXPORTING is_source = is_request_data
                                                        iv_ignore_itab = abap_true
                                              CHANGING cs_destiny = cs_strategy_data ).

    " Clonamos el campo de compradores
    zcl_ca_utilities=>clone_table_values( EXPORTING it_source = is_request_data-buyers
                                              CHANGING ct_destiny = cs_strategy_data-buyers_requested ).

    " Clonamos los campos de estrategias. En ese caso si que nos interesa que se clonan los campos que son
    " tablas internas (los aprobadores) porque son propios de la petición de cambio
    zcl_ca_utilities=>clone_table_values( EXPORTING it_source = is_request_data-strategies
                                              CHANGING ct_destiny = cs_strategy_data-strategies_requested ).

    " Los datos propios del maestro de grupo de compras, actualmente el texto se informan a mano.
    cs_strategy_data-purchase_group_requested_desc = is_request_data-pgroup-purchase_group_desc.
    cs_strategy_data-cdchngind = is_request_data-pgroup-cdchngind.
  ENDMETHOD.


  METHOD format_classif_amount_char.
    DATA lv_amount_char TYPE c LENGTH 30.

    IF iv_operand = '-'. " Intervalo.

      WRITE iv_amount CURRENCY iv_currency TO lv_amount_char LEFT-JUSTIFIED.
      rv_amount_char = |{ lv_amount_char } { iv_currency } { iv_operand }|.
      WRITE iv_amount2 CURRENCY iv_currency TO lv_amount_char LEFT-JUSTIFIED.
      rv_amount_char = |{ rv_amount_char } { lv_amount_char } { iv_currency }|.

    ELSE.
      " El amount char es la concatenación del operador, importe (formato char) y moneda
      WRITE: iv_amount CURRENCY iv_currency TO lv_amount_char LEFT-JUSTIFIED.
      rv_amount_char = |{ iv_operand } { lv_amount_char } { iv_currency }|.

    ENDIF.
  ENDMETHOD.

  METHOD ctbp_convert_value_ext_to_int.
    FIELD-SYMBOLS: <p_dflag> TYPE syst_batch .
    FIELD-SYMBOLS: <p_tflag> TYPE syst_batch .

    DATA lv_atnam TYPE atnam.

    CLEAR: ev_value_from, ev_value_relation, ev_value_to.

    " El nombre de la característica se puede pasar el nombre externo o el interno.
    IF iv_charactname IS NOT INITIAL.
      lv_atnam = iv_charactname.
    ELSE.
      CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
        EXPORTING
          input  = iv_charctinn
        IMPORTING
          output = lv_atnam.
    ENDIF.

    " Si el valor formateado no se ha pasado lo creo a partir de los importes, operador y moneda.
    IF iv_value_formatted IS NOT INITIAL.
      DATA(lv_value_external) = iv_value_formatted.
    ELSE.


      lv_value_external = format_classif_amount_char( iv_amount   = iv_amount
                                                      iv_amount2  = iv_amount2
                                                      iv_operand  = iv_operand
                                                      iv_currency = iv_currency ).

    ENDIF.


    CALL FUNCTION 'CTBP_CONVERT_VALUE_EXT_TO_INT'
      EXPORTING
        charactname           = lv_atnam
        value_external        = lv_value_external
        iv_no_authority_check = abap_true
      IMPORTING
        value_from            = ev_value_from
        value_to              = ev_value_to
        value_relation        = ev_value_relation
      EXCEPTIONS
        no_authority          = 1                " Missing Authorization
        charact_not_found     = 2                " Characteristic Does Not Exist Or Is Not Valid
        wrong_data_type       = 3                " Characteristic Has Character Format
        wrong_value           = 4                " Incorrect Value Format
        wrong_input           = 5                " CHARACTNAME or CHARCTDETAIL must be filled
        OTHERS                = 6.

  ENDMETHOD.

  METHOD conv_atinn_2_atnam.
clear: rv_atnam.

CALL FUNCTION 'CONVERSION_EXIT_ATINN_OUTPUT'
        EXPORTING
          input  = iv_atinn
        IMPORTING
          output = rv_atnam.
  ENDMETHOD.

ENDCLASS.
