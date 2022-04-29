class ZCL_REL_D_STRATEGY_AMOUNT definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCL_SIMPLE
  final
  create public .

public section.

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
  PROTECTED SECTION.
    METHODS amount_char
      IMPORTING
        is_ctx    TYPE /bobf/s_frw_ctx_det
        it_key    TYPE /bobf/t_frw_key
        io_read   TYPE REF TO /bobf/if_frw_read
        io_modify TYPE REF TO /bobf/if_frw_modify.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_REL_D_STRATEGY_AMOUNT IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    CASE is_ctx-det_key.
      WHEN zif_rel_bo_strategy_c=>sc_determination-amount-amount_char.
        amount_char(
          EXPORTING
            is_ctx        = is_ctx
            it_key        = it_key
            io_read       = io_read
            io_modify     = io_modify ).



    ENDCASE.
  ENDMETHOD.


  METHOD amount_char.
    DATA lt_data TYPE zrel_bo_i_strategy_amount.
    DATA lv_amount_char TYPE c LENGTH 20.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                               it_key  = it_key
                     IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.

      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        lo_data->amount_char = zcl_rel_utilities=>format_classif_amount_char( iv_amount   = lo_data->amount
                                                                              iv_amount2  = lo_data->amount2
                                                                              iv_operand  = lo_data->amount_operand
                                                                              iv_currency = lo_data->currency ).


        io_modify->update( iv_node = is_ctx-node_key
                           iv_key  = lo_data->key
                           is_data = lo_data ).
      ENDLOOP.


    ENDIF.

  ENDMETHOD.
ENDCLASS.
