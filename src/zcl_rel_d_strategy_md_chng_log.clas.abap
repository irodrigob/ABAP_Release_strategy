class ZCL_REL_D_STRATEGY_MD_CHNG_LOG definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCL_SIMPLE
  final
  create public .

public section.

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
  PROTECTED SECTION.
    METHODS build_message
      IMPORTING
        is_ctx    TYPE /bobf/s_frw_ctx_det
        it_key    TYPE /bobf/t_frw_key
        io_read   TYPE REF TO /bobf/if_frw_read
        io_modify TYPE REF TO /bobf/if_frw_modify.
    METHODS new_log
      IMPORTING
        is_ctx    TYPE /bobf/s_frw_ctx_det
        it_key    TYPE /bobf/t_frw_key
        io_read   TYPE REF TO /bobf/if_frw_read
        io_modify TYPE REF TO /bobf/if_frw_modify.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_REL_D_STRATEGY_MD_CHNG_LOG IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    CASE is_ctx-det_key.
      WHEN zif_rel_bo_strategy_c=>sc_determination-md_change_log-new_log.
        new_log(
          EXPORTING
            is_ctx        = is_ctx
            it_key        = it_key
            io_read       = io_read
            io_modify     = io_modify ).
      WHEN zif_rel_bo_strategy_c=>sc_determination-md_change_log-build_message.
        build_message(
          EXPORTING
            is_ctx        = is_ctx
            it_key        = it_key
            io_read       = io_read
            io_modify     = io_modify ).
    ENDCASE.
  ENDMETHOD.


  METHOD build_message.
    DATA lt_data TYPE zrel_bo_i_strategy_md_log.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.


      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        lo_data->message = zcl_ca_utilities=>fill_return( iv_type = lo_data->type
                                                          iv_id = lo_data->id
                                                          iv_number = lo_data->number
                                                          iv_message_v1 = lo_data->message_v1
                                                          iv_message_v2 = lo_data->message_v2
                                                          iv_message_v3 = lo_data->message_v3
                                                          iv_message_v4 = lo_data->message_v4 )-message.

        io_modify->update( iv_node = is_ctx-node_key
                           iv_key  = lo_data->key
                           is_data = lo_data
                            it_changed_fields = VALUE #( ( zif_rel_bo_strategy_c=>sc_node_attribute-md_change_log-message ) ) ).

      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD new_log.
    DATA lt_data TYPE zrel_bo_i_strategy_md_log.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.


      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        lo_data->erdat = sy-datum.
        lo_data->ernam = sy-uname.
        lo_data->erzet = sy-uzeit.

        io_modify->update( iv_node = is_ctx-node_key
                           iv_key  = lo_data->key
                           is_data = lo_data
                            it_changed_fields = VALUE #( ( zif_rel_bo_strategy_c=>sc_node_attribute-md_change_log-erdat )
                                                        ( zif_rel_bo_strategy_c=>sc_node_attribute-md_change_log-ernam )
                                                        ( zif_rel_bo_strategy_c=>sc_node_attribute-md_change_log-erzet ) ) ).

      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
