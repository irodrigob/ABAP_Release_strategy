class ZCL_REL_D_STRATEGY_APPRVRS_SAP definition
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



CLASS ZCL_REL_D_STRATEGY_APPRVRS_SAP IMPLEMENTATION.


  METHOD /BOBF/IF_FRW_DETERMINATION~EXECUTE.
    CASE is_ctx-det_key.
      WHEN zif_rel_bo_strategy_c=>sc_determination-approvers_sap-descriptions.
        descriptions(
          EXPORTING
            is_ctx        = is_ctx
            it_key        = it_key
            io_read       = io_read
            io_modify     = io_modify ).



    ENDCASE.
  ENDMETHOD.


  METHOD CONSTRUCTOR.
    super->constructor( ).

    mo_strategy_md = NEW zcl_rel_strategy_md_query( ).

  ENDMETHOD.


  METHOD DESCRIPTIONS.
    DATA lt_data TYPE zrel_bo_i_strategy_apprv_sap.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                               it_key  = it_key
                     IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.

      " Nota Iván: En el método que recupera los nombres de los codigos de liberación, en la clase que recupera
      " los datos de las estrategias, hay una lectura extra que es cuando no existe el usuario en SAP se lee de la denominación.
      " Aquí eso no se puede hacer, porque no se sabe si los datos que se leen son actuales o un histórico. Ante eso,
      " y para evitar denominaciones que no son correctos, se opta por buscar directamente el nombre del usuario.
      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        " El aprobador solicitado se mira del usuario
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
