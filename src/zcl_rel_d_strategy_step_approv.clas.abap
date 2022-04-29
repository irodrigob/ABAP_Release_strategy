class ZCL_REL_D_STRATEGY_STEP_APPROV definition
  public
  inheriting from /BOBF/CL_LIB_D_SUPERCL_SIMPLE
  final
  create public .

public section.

  methods CONSTRUCTOR .
  methods DESCRIPTIONS
    importing
      !IS_CTX type /BOBF/S_FRW_CTX_DET
      !IT_KEY type /BOBF/T_FRW_KEY
      !IO_READ type ref to /BOBF/IF_FRW_READ
      !IO_MODIFY type ref to /BOBF/IF_FRW_MODIFY .

  methods /BOBF/IF_FRW_DETERMINATION~EXECUTE
    redefinition .
  PROTECTED SECTION.
    DATA mo_strategy_md TYPE REF TO zcl_rel_strategy_md_query.

  PRIVATE SECTION.
ENDCLASS.



CLASS ZCL_REL_D_STRATEGY_STEP_APPROV IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    CASE is_ctx-det_key.
      WHEN zif_rel_bo_strategy_c=>sc_determination-step_approvers-descriptions.
        descriptions(
          EXPORTING
            is_ctx        = is_ctx
            it_key        = it_key
            io_read       = io_read
            io_modify     = io_modify ).
    ENDCASE.
  ENDMETHOD.


  METHOD constructor.
    super->constructor(  ).

    mo_strategy_md = NEW zcl_rel_strategy_md_query( ).

  ENDMETHOD.


  METHOD descriptions.
    DATA lt_data TYPE zrel_bo_i_strategy_step_aprov.
    DATA lt_r_dept_subs TYPE zif_rel_data=>tt_r_dept_subs.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.
      " Leo las descripciones de los status de la peticion.
      SELECT domvalue_l, ddtext INTO TABLE @DATA(lt_status_desc)
                  FROM dd07t
                  WHERE domname = @zif_rel_data=>cs_strategy-change_request-approvals-request_status-domain
                        AND ddlanguage = @sy-langu.


      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        " Status de la petición
        IF lo_data->request_status IS NOT INITIAL.
          READ TABLE lt_status_desc ASSIGNING FIELD-SYMBOL(<ls_status_desc>) WITH KEY domvalue_l = lo_data->request_status.
          IF sy-subrc = 0.
            lo_data->request_status_desc = <ls_status_desc>-ddtext.
          ENDIF.
        ENDIF.


        io_modify->update( iv_node = is_ctx-node_key
                           iv_key  = lo_data->key
                           is_data = lo_data ).
      ENDLOOP.
    ENDIF.
  ENDMETHOD.
ENDCLASS.
