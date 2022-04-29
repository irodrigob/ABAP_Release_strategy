CLASS zcl_rel_d_strategy_header DEFINITION
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
    METHODS new_request
      IMPORTING
        is_ctx    TYPE /bobf/s_frw_ctx_det
        it_key    TYPE /bobf/t_frw_key
        io_read   TYPE REF TO /bobf/if_frw_read
        io_modify TYPE REF TO /bobf/if_frw_modify.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_d_strategy_header IMPLEMENTATION.


  METHOD /bobf/if_frw_determination~execute.
    CASE is_ctx-det_key.
      WHEN zif_rel_bo_strategy_c=>sc_determination-root-descriptions.
        descriptions(
          EXPORTING
            is_ctx        = is_ctx
            it_key        = it_key
            io_read       = io_read
            io_modify     = io_modify ).
      WHEN zif_rel_bo_strategy_c=>sc_determination-root-new_request.
        new_request(
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
    DATA lt_data TYPE zrel_bo_i_strategy_header.
    DATA lt_r_dept_subs TYPE zif_rel_data=>tt_r_dept_subs.
    DATA lt_r_bukrs TYPE zif_rel_data=>tt_r_dept_subs.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.
      " Leo las descripciones de los status de la peticion.
      SELECT domvalue_l, ddtext INTO TABLE @DATA(lt_status_desc)
                  FROM dd07t
                  WHERE domname = @zif_rel_data=>cs_strategy-change_request-approvals-request_status-domain
                        AND ddlanguage = @sy-langu.

      " Leo las descripciones de los status de cambio del maestro de datos
      SELECT domvalue_l, ddtext INTO TABLE @DATA(lt_status_md_desc)
                  FROM dd07t
                  WHERE domname = @zif_rel_data=>cs_strategy-master_data-change_md_status-domain
                        AND ddlanguage = @sy-langu.

      " Hago un primer recorrido para llenar los ranges para optimziar las búsquedas
      LOOP AT lt_data REFERENCE INTO DATA(lo_data).
        IF lo_data->dept_subs IS NOT INITIAL.
          INSERT VALUE #( sign = 'I' option = 'EQ' low = lo_data->dept_subs ) INTO TABLE lt_r_dept_subs.
        ENDIF.
        IF lo_data->company IS NOT INITIAL.
          INSERT VALUE #( sign = 'I' option = 'EQ' low = lo_data->company ) INTO TABLE lt_r_bukrs.
        ENDIF.
      ENDLOOP.

      IF lt_r_dept_subs IS NOT INITIAL.
        SELECT dept_subs, description INTO TABLE @DATA(lt_dept_subs_desc)
               FROM zrel_t002t
               WHERE dept_subs IN @lt_r_dept_subs
                     AND spras = @sy-langu.
      ENDIF.
      IF lt_r_bukrs IS NOT INITIAL.
        SELECT bukrs, butxt INTO TABLE @DATA(lt_t001)
               FROM t001
               WHERE bukrs IN @lt_r_bukrs.
      ENDIF.


      LOOP AT lt_data REFERENCE INTO lo_data.

        " Quien ha realizado la aprobacion
        IF lo_data->approved_by IS NOT INITIAL.
          lo_data->approved_by_desc = mo_strategy_md->get_username_desc( iv_username = lo_data->approved_by ).
          lo_data->approved_by_desc = COND #( WHEN lo_data->approved_by_desc IS NOT INITIAL THEN lo_data->approved_by_desc ELSE lo_data->approved_by ).
        ENDIF.

        " Quien ha realizado la solicitud
        IF lo_data->request_by IS NOT INITIAL.
          lo_data->request_by_desc = mo_strategy_md->get_username_desc( iv_username = lo_data->request_by ).
          lo_data->request_by_desc = COND #( WHEN lo_data->request_by_desc IS NOT INITIAL THEN lo_data->request_by_desc ELSE lo_data->request_by ).
        ENDIF.

        " Status de la petición
        IF lo_data->request_status IS NOT INITIAL.
          READ TABLE lt_status_desc ASSIGNING FIELD-SYMBOL(<ls_status_desc>) WITH KEY domvalue_l = lo_data->request_status.
          IF sy-subrc = 0.
            lo_data->request_status_desc = <ls_status_desc>-ddtext.
          ENDIF.
        ENDIF.

        " Status del cambio en el maestro de datos
        IF lo_data->change_md_status IS NOT INITIAL.
          READ TABLE lt_status_md_desc ASSIGNING FIELD-SYMBOL(<ls_status_md_desc>) WITH KEY domvalue_l = lo_data->change_md_status.
          IF sy-subrc = 0.
            lo_data->change_md_status_desc = <ls_status_md_desc>-ddtext.
          ENDIF.
        ENDIF.

        IF lo_data->dept_subs IS NOT INITIAL.
          READ TABLE lt_dept_subs_desc ASSIGNING FIELD-SYMBOL(<ls_dept_subs_desc>) WITH KEY dept_subs = lo_data->dept_subs.
          IF sy-subrc = 0.
            lo_data->dept_subs_desc = <ls_dept_subs_desc>-description.
          ENDIF.
        ENDIF.

        IF lo_data->company IS NOT INITIAL.
          READ TABLE lt_t001 ASSIGNING FIELD-SYMBOL(<ls_t001>) WITH KEY bukrs = lo_data->company.
          IF sy-subrc = 0.
            lo_data->company_desc = <ls_t001>-butxt.
          ENDIF.
        ENDIF.

        io_modify->update( iv_node = is_ctx-node_key
                           iv_key  = lo_data->key
                           is_data = lo_data ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.


  METHOD new_request.
    DATA lt_data TYPE zrel_bo_i_strategy_header.

    io_read->retrieve( EXPORTING iv_node = is_ctx-node_key
                                 it_key  = it_key
                       IMPORTING et_data = lt_data ).

    IF lt_data IS NOT INITIAL.
      LOOP AT lt_data REFERENCE INTO DATA(lo_data).

        lo_data->request_by = sy-uname.
        lo_data->request_date = sy-datum.
        lo_data->request_time = sy-uzeit.
        lo_data->request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending.

        io_modify->update( iv_node = is_ctx-node_key
                           iv_key  = lo_data->key
                           is_data = lo_data ).
      ENDLOOP.

    ENDIF.
  ENDMETHOD.
ENDCLASS.
