class ZCL_REL_D_STRATEGY_MD_CHNG_ST definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCL_SIMPLE
  final
  create public .

public section.

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
  PROTECTED SECTION.
    METHODS descriptions
      IMPORTING
        is_ctx    TYPE /bobf/s_frw_ctx_det
        it_key    TYPE /bobf/t_frw_key
        io_read   TYPE REF TO /bobf/if_frw_read
        io_modify TYPE REF TO /bobf/if_frw_modify.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_REL_D_STRATEGY_MD_CHNG_ST IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    CASE is_ctx-det_key.
      WHEN zif_rel_bo_strategy_c=>sc_determination-md_change_status-descriptions.
        descriptions(
          EXPORTING
            is_ctx        = is_ctx
            it_key        = it_key
            io_read       = io_read
            io_modify     = io_modify ).

    ENDCASE.
  ENDMETHOD.


  METHOD descriptions.
    DATA lt_data TYPE zrel_bo_i_strategy_md_chng_st.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.

      " Leo las descripciones de los status de cambio del maestro de datos
      SELECT domvalue_l, ddtext INTO TABLE @DATA(lt_status_md_desc)
                  FROM dd07t
                  WHERE domname = @zif_rel_data=>cs_strategy-master_data-change_md_status-domain
                        AND ddlanguage = @sy-langu.

      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        " Status del cambio en el maestro de datos
        IF lo_data->md_status IS NOT INITIAL.
          READ TABLE lt_status_md_desc ASSIGNING FIELD-SYMBOL(<ls_status_md_desc>) WITH KEY domvalue_l = lo_data->md_status.
          IF sy-subrc = 0.
            lo_data->md_status_desc = <ls_status_md_desc>-ddtext.
          ENDIF.
        ENDIF.

        io_modify->update( iv_node = is_ctx-node_key
                           iv_key  = lo_data->key
                           is_data = lo_data ).

      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
