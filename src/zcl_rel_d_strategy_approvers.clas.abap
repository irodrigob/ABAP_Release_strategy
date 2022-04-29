CLASS zcl_rel_d_strategy_approvers DEFINITION
  PUBLIC
  INHERITING FROM /bobf/cl_lib_d_supercl_simple
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS constructor .

    METHODS /bobf/if_frw_determination~execute
        REDEFINITION .
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



CLASS zcl_rel_d_strategy_approvers IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    CASE is_ctx-det_key.
      WHEN zif_rel_bo_strategy_c=>sc_determination-approvers-descriptions.
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
    DATA lt_data TYPE zrel_bo_i_strategy_approvers.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                               it_key  = it_key
                     IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.

      " Leo las descripciones de los motivos del cambio
      SELECT domvalue_l, ddtext INTO TABLE @DATA(lt_type_change_desc)
                  FROM dd07t
                  WHERE domname = @zif_rel_data=>cs_strategy-change_request-type_change_approver-domain
                        AND ddlanguage = @sy-langu.

      " Busco la descripción del código de liberación para el caso que no lo encuentre como usuario SAP. Este caso
      " solo sirve para los entornos que no son producción. Ya que en producción el usuario SAP debe de existir
      SELECT * INTO TABLE @DATA(lt_t16fd)
             FROM t16fd
             FOR ALL ENTRIES IN @lt_data
             WHERE frggr = @lt_data-group
                   AND frgco = @lt_data-code.

      " Nota Iván: En el método que recupera los nombres de los codigos de liberación, en la clase que recupera
      " los datos de las estrategias, hay una lectura extra que es cuando no existe el usuario en SAP se lee de la denominación.
      " Aquí eso no se puede hacer, porque no se sabe si los datos que se leen son actuales o un histórico. Ante eso,
      " y para evitar denominaciones que no son correctos, se opta por buscar directamente el nombre del usuario.
      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        " El aprobador solicitado se mira del usuario
        IF lo_data->username IS NOT INITIAL.
          lo_data->username_desc = mo_strategy_md->get_username_desc( iv_username = lo_data->username ).
          " Si no hay descripcion la saco del codigo
          IF lo_data->username_desc IS INITIAL.
            " Primero en idioma de logon, en caso de no existir en el primer idioma que encuentre
            READ TABLE lt_t16fd ASSIGNING FIELD-SYMBOL(<ls_t16fd>)
                                WITH KEY frggr = lo_data->group
                                         frgco = lo_data->code
                                         spras = sy-langu.
            IF sy-subrc = 0.
              lo_data->username_desc = <ls_t16fd>-frgct.
            ELSE.
              READ TABLE lt_t16fd ASSIGNING <ls_t16fd>
                                  WITH KEY frggr = lo_data->group
                                           frgco = lo_data->code.
              IF sy-subrc = 0.
                lo_data->username_desc = <ls_t16fd>-frgct.
              ENDIF.
            ENDIF.

          ENDIF.
          lo_data->username_desc = COND #( WHEN lo_data->username_desc IS NOT INITIAL THEN lo_data->username_desc ELSE lo_data->username ).

        ENDIF.


        " Tipo del motivo del cambio del aprobador
        IF lo_data->type_change IS NOT INITIAL.
          READ TABLE lt_type_change_desc ASSIGNING FIELD-SYMBOL(<ls_type_change_desc>) WITH KEY domvalue_l = lo_data->type_change.
          IF sy-subrc = 0.
            lo_data->type_change_desc = <ls_type_change_desc>-ddtext.
          ENDIF.
        ENDIF.

        io_modify->update( iv_node = is_ctx-node_key
                           iv_key  = lo_data->key
                           is_data = lo_data ).
      ENDLOOP.


    ENDIF.

  ENDMETHOD.
ENDCLASS.
