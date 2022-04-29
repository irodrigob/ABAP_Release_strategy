class ZCL_REL_D_STRATEGY_BUYERS definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCL_SIMPLE
  final
  create public .

public section.

  methods CONSTRUCTOR .

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
  PROTECTED SECTION.
    DATA mo_strategy_md TYPE REF TO zcl_rel_strategy_md_query.

    METHODS descriptions
      IMPORTING
        is_ctx    TYPE /bobf/s_frw_ctx_det
        it_key    TYPE /bobf/t_frw_key
        io_read   TYPE REF TO /bobf/if_frw_read
        io_modify TYPE REF TO /bobf/if_frw_modify.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_REL_D_STRATEGY_BUYERS IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    CASE is_ctx-det_key.
      WHEN zif_rel_bo_strategy_c=>sc_determination-buyers-descriptions.
        descriptions(
          EXPORTING
            is_ctx        = is_ctx
            it_key        = it_key
            io_read       = io_read
            io_modify     = io_modify ).



    ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    super->constructor( ).

    mo_strategy_md = NEW zcl_rel_strategy_md_query( ).

  ENDMETHOD.


  METHOD descriptions.
    DATA lt_data TYPE zrel_bo_i_strategy_buyers.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                               it_key  = it_key
                     IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.

      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        IF lo_data->username IS NOT INITIAL.
          lo_data->username_desc = mo_strategy_md->get_username_desc( iv_username = lo_data->username ).
          lo_data->username_desc = COND #( WHEN lo_data->username_desc IS NOT INITIAL THEN lo_data->username_desc ELSE lo_data->username ).
        ENDIF.

        io_modify->update( iv_node = is_ctx-node_key
                           iv_key  = lo_data->key
                           is_data = lo_data ).
      ENDLOOP.


    ENDIF.

  ENDMETHOD.
ENDCLASS.
