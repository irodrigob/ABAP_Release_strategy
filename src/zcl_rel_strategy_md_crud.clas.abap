CLASS zcl_rel_strategy_md_crud DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING
        iv_langu TYPE sylangu DEFAULT sy-langu.

    "! <p class="shorttext synchronized">Gestiona el proceso de actualización de datos maestros</p>
    "! @parameter iv_key | <p class="shorttext synchronized">Clave del registro</p>
    "! @parameter iv_request_id | <p class="shorttext synchronized">Acción</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    "! @parameter ev_master_data_updated | <p class="shorttext synchronized">Datos maestros actualizados completamente</p>
    METHODS dispatch_update_master_data
      IMPORTING
        iv_key                 TYPE zrel_bo_sc_strategy_header-key OPTIONAL
        iv_request_id          TYPE zrel_bo_sc_strategy_header-request_id OPTIONAL
      EXPORTING
        et_return              TYPE bapiret2_t
        ev_master_data_updated TYPE sap_bool.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_process_buyers.
             INCLUDE TYPE zrel_bo_sp_strategy_buyers.
           TYPES: END OF ts_process_buyers.
    TYPES: tt_process_buyers TYPE STANDARD TABLE OF ts_process_buyers WITH EMPTY KEY.
    TYPES: BEGIN OF ts_process_amount.
             INCLUDE TYPE zrel_bo_sp_strategy_amount.
           TYPES: END OF ts_process_amount.
    TYPES: tt_process_amount TYPE STANDARD TABLE OF ts_process_amount WITH EMPTY KEY.
    TYPES: BEGIN OF ts_global_options,
             commit TYPE sap_bool,
           END OF ts_global_options.
    TYPES: BEGIN OF ts_strag_code_new_created,
             group            TYPE zrel_bo_sc_strategy_amount-group,
             strategy         TYPE zrel_bo_sc_strategy_amount-strategy,
             group_created    TYPE zrel_bo_sc_strategy_amount-group,
             strategy_created TYPE zrel_bo_sc_strategy_amount-strategy,
             strategy_reused  TYPE sap_bool,
             level            TYPE zrel_bo_sc_strategy_amount-level,
           END OF ts_strag_code_new_created.
    TYPES: tt_strag_code_new_created TYPE STANDARD TABLE OF ts_strag_code_new_created WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_free_blocks_strategies,
             strategy TYPE zrel_bo_sc_strategy_amount-strategy,
             reused   TYPE sap_bool,
           END OF ts_free_blocks_strategies.
    TYPES: tt_free_blocks_strategies TYPE STANDARD TABLE OF ts_free_blocks_strategies WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_comb_group_strat,
             group    TYPE zrel_bo_sc_strategy_amount-group,
             strategy TYPE zrel_bo_sc_strategy_amount-strategy,
           END OF ts_comb_group_strat.
    TYPES: tt_comb_group_strat TYPE STANDARD TABLE OF ts_comb_group_strat WITH DEFAULT KEY.
    DATA mv_langu TYPE sy-langu.
    DATA mt_header TYPE zrel_bo_i_strategy_header.
    DATA ms_header TYPE zrel_bo_sc_strategy_header.
    DATA mt_buyers TYPE zrel_bo_i_strategy_buyers.
    DATA mt_amounts TYPE zrel_bo_i_strategy_amount.
    DATA mt_amounts_sap TYPE zrel_bo_i_strategy_amount_sap.
    DATA mt_approvers TYPE zrel_bo_i_strategy_approvers.
    DATA mt_approvers_sap TYPE zrel_bo_i_strategy_apprv_sap.
    DATA mt_pgroup TYPE zrel_bo_i_strategy_pgroup.
    DATA ms_pgroup TYPE zrel_bo_sc_strategy_pgroup.
    DATA mt_md_change_status TYPE zrel_bo_i_strategy_md_chng_st.
    DATA ms_global_options TYPE ts_global_options.
    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_conf_mngr TYPE REF TO /bobf/if_frw_configuration.
    DATA mo_bopf_util TYPE REF TO zcl_ca_bopf_util.
    DATA mo_bopf_helper TYPE REF TO zcl_rel_helper_bo.
    DATA mo_md_query TYPE REF TO zcl_rel_strategy_md_query.
    DATA mo_general_md TYPE REF TO zcl_rel_general_master_data.
    DATA mt_strag_code_new_created TYPE tt_strag_code_new_created.
    DATA mt_t16fs TYPE zcl_rel_strategy_md_query=>tt_strategies_from_group.
    DATA mt_t16fd TYPE zrel_i_t16fd.
    DATA mt_t16fs_update TYPE zrel_i_t16fs.
    DATA mt_t16ft_update TYPE zrel_i_t16ft.
    DATA mt_t16fk_update TYPE zrel_i_t16fk.
    DATA mt_t16fv_update TYPE zrel_i_t16fv.
    DATA mt_t16fk_delete TYPE zrel_i_t16fk.
    DATA mt_t16fv_delete TYPE zrel_i_t16fv.
    DATA mt_t16fw_update TYPE zrel_i_t16fw.
    DATA mt_t16fw_delete TYPE zrel_i_t16fw.
    DATA mt_t16fd_update TYPE zrel_i_t16fd.
    DATA mt_t16fc_update TYPE zrel_i_t16fc.
    DATA mt_generator_comb_strategies TYPE zcl_ca_combinations_generator=>tt_combinations.
    DATA mv_klart_obtab TYPE tabelle.
    DATA mt_lib_group_of_user TYPE zcl_rel_strategy_md_query=>tt_user_lib_group .
    DATA mt_users_from_lib_group TYPE zcl_rel_strategy_md_query=>tt_user_lib_group .
    DATA mv_langu_texto_baja TYPE sylangu.
    DATA mv_texto_baja TYPE frgxt.

    "! <p class="shorttext synchronized">Proceso de actualización de compradores</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS process_buyers
      EXPORTING
        et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Proceso de actualización del grupo de compras</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS process_pgroup
      EXPORTING
        et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Proceso de actualización de importes</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS process_strategy
      EXPORTING et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Proceso de actualización de importes</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS process_amounts
      EXPORTING et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Actualiza un importe</p>
    "! @parameter is_amount | <p class="shorttext synchronized">Importe</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS update_amount
      IMPORTING
        is_amount TYPE zrel_bo_sc_strategy_amount
      EXPORTING
        et_return TYPE bapiret2_t.

    "! <p class="shorttext synchronized">Inicialización variables para el BOPF</p>
    METHODS init_bopf.
    "! <p class="shorttext synchronized">lectura de datos del BOPF</p>
    "! @parameter iv_key | <p class="shorttext synchronized">Clave del registro</p>
    "! @parameter iv_request_id | <p class="shorttext synchronized">Acción</p>
    METHODS get_values_from_bopf
      IMPORTING
        iv_key        TYPE zrel_bo_sc_strategy_header-key OPTIONAL
        iv_request_id TYPE zrel_bo_sc_strategy_header-request_id OPTIONAL.
    "! <p class="shorttext synchronized">Actualiza los valores en el BOPF</p>
    METHODS update_bopf_values.
    "! <p class="shorttext synchronized">Actualiza los status de modificación de los datos maestros</p>
    "! @parameter it_md_change_status | <p class="shorttext synchronized">Datos de los status de los bloques</p>
    "! @parameter it_md_change_log | <p class="shorttext synchronized">Datos de los logs de los bloques</p>
    "! @parameter it_return | <p class="shorttext synchronized">Mensajes a actualizar en el log del bloque</p>
    METHODS update_bopf_md_status
      IMPORTING it_md_change_status TYPE zrel_bo_i_strategy_md_chng_st
                it_md_change_log    TYPE zrel_bo_i_strategy_md_log OPTIONAL
                it_return           TYPE bapiret2_t OPTIONAL.

    "! <p class="shorttext synchronized">Actualiza los datos de cabecera</p>
    METHODS update_bopf_header.
    "! <p class="shorttext synchronized">Actualiza el nodo de importes</p>
    METHODS update_bopf_node_amounts.
    "! <p class="shorttext synchronized">Actualiza el nodo de aprobadores</p>
    METHODS update_bopf_node_approvers.
    "! <p class="shorttext synchronized">Añade registro a la tabla de log de bloques</p>
    "! @parameter iv_block | <p class="shorttext synchronized">Bloque</p>
    "! @parameter it_return | <p class="shorttext synchronized">Tipo</p>
    "! @parameter ev_key | <p class="shorttext synchronized">Clave generada</p>
    METHODS add_md_change_log
      IMPORTING
        iv_block      TYPE zrel_bo_sc_strategy_md_chng_st-md_block
        it_return     TYPE bapiret2_t
      EXPORTING
        ev_key        TYPE zrel_bo_sc_strategy_md_chng_st-key_last_log
        et_change_log TYPE zrel_bo_i_strategy_md_log .
    "! <p class="shorttext synchronized">Inserta un importe</p>
    "! @parameter cs_amount | <p class="shorttext synchronized">Importe</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS insert_amount
      IMPORTING
        is_amount TYPE zrel_bo_sc_strategy_amount
      EXPORTING
        et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Se determina nueva estrategia</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Grupo</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS determine_new_strategy
      IMPORTING
        iv_group  TYPE frggr
      EXPORTING
        et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Rellena el texto para la tabla T16FT</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Grupo libreación</p>
    "! @parameter iv_strategy | <p class="shorttext synchronized">Estrategia liberación</p>
    "! @parameter iv_level | <p class="shorttext synchronized">Nivel</p>
    "! @parameter iv_text | <p class="shorttext synchronized">Texto</p>
    "! @parameter et_values | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS build_text_t16ft
      IMPORTING
                iv_group         TYPE zrel_bo_sc_strategy_amount-group
                iv_strategy      TYPE zrel_bo_sc_strategy_amount-strategy
                iv_level         TYPE zrel_e_level OPTIONAL
                iv_text          TYPE frgxt  OPTIONAL
      RETURNING VALUE(rt_values) TYPE zrel_i_t16ft.
    "! <p class="shorttext synchronized">Determina el grupo y estrategia que se creará</p>
    "! @parameter et_return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS process_update_custo_sap
      EXPORTING
        et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Construye los datos base de la T16FS</p>
    "! @parameter et_return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS build_custo_base_t16fs
      EXPORTING
        et_return TYPE bapiret2_t.

    "! <p class="shorttext synchronized">Búsqueda de bloques consecutivos libres para strategieas</p>
    "! @parameter iv_from_tabix | <p class="shorttext synchronized">Registro inicial de la combinación</p>
    "! @parameter iv_size_block | <p class="shorttext synchronized">Número de strategias</p>
    "! @parameter iv_block_contiguos | <p class="shorttext synchronized">Las estrategias tienen que ser contiguos</p>
    "! @parameter et_blocks | <p class="shorttext synchronized">Bloque con las estrategias</p>
    METHODS search_free_block_strategies
      IMPORTING
                iv_from_tabix      TYPE i
                iv_size_block      TYPE i
                iv_block_contiguos TYPE sap_bool DEFAULT abap_true
      EXPORTING et_blocks          TYPE tt_free_blocks_strategies.
    "! <p class="shorttext synchronized">Graba la parametrización en SAP y BOPF</p>
    "! También graba en el BOPF los datos de importe y cantidad para actualzar el grupo
    "! y estrategia de liberación. De esta manera los datos van a la par modificados
    "! el grupo y estrategia de liberación en el caso de creación.
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS save_custo_sap_bo
      EXPORTING
        et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Obtiene la tabla del sistema de configuración</p>
    "! @parameter rv_table | <p class="shorttext synchronized">Tabla</p>
    METHODS get_klart_table
      RETURNING
        VALUE(rv_table) TYPE tabelle.
    "! <p class="shorttext synchronized">Construye la parámetrización para los aprobadores</p>
    "! @parameter et_return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS build_custo_approvers
      EXPORTING
        et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Se determina el codigo para el nuevo aprobador</p>
    "! El código se añade a la parametrización de las tablas de custo ha actualizar.
    "! @parameter is_approver | <p class="shorttext synchronized">Aprobador</p>
    METHODS det_new_approver_code
      IMPORTING
                is_approver TYPE zrel_bo_sc_strategy_approvers
      EXPORTING et_return   TYPE bapiret2_t
                ev_code     TYPE frgco.
    "! <p class="shorttext synchronized">Devuelve un codigo del usuario para el grupo del parametro</p>
    "! Busca si el usuario ya tiene codigo en el grupo pasado por parámetro. Si no hay devovera
    "! el codigo donde este asignado el usuario en otro grupo y se pueda usar en el grupo pasado por
    "! parámetro
    "! @parameter iv_username | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_group_of_code | <p class="shorttext synchronized">Grupo donde estará el código</p>
    METHODS get_code_user_to_group
      IMPORTING
        iv_username      TYPE zrel_bo_sc_strategy_approvers-username
        iv_group_of_code TYPE zrel_bo_sc_strategy_approvers-group
      RETURNING
        VALUE(rv_code)   TYPE zrel_bo_sc_strategy_approvers-code.
    "! <p class="shorttext synchronized">Reseteo de aprobadores de la T16FS</p>
    "! @parameter cs_t16fs | <p class="shorttext synchronized">Estructura T16FS</p>
    METHODS reset_approvers_t16fs
      CHANGING
        cs_t16fs TYPE t16fs.
    "! <p class="shorttext synchronized">Rellena el texto para la tabla T16FD</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Grupo libreación</p>
    "! @parameter iv_code | <p class="shorttext synchronized">Código liberación</p>
    "! @parameter iv_text | <p class="shorttext synchronized">Texto</p>
    "! @parameter et_values | <p class="shorttext synchronized">Valores</p>
    METHODS build_text_t16fd
      IMPORTING
                iv_group         TYPE zrel_bo_sc_strategy_approvers-group
                iv_code          TYPE zrel_bo_sc_strategy_approvers-code
                iv_text          TYPE zrel_bo_sc_strategy_approvers-username_desc
      RETURNING VALUE(rt_values) TYPE zrel_i_t16fd.
    "! <p class="shorttext synchronized">Rellena las IT del maestro para actualizar los aprobadores</p>
    "! @parameter iv_code | <p class="shorttext synchronized">Código liberación</p>
    "! @parameter is_approver | <p class="shorttext synchronized">Aprobación</p>
    "! @parameter iv_fill_t16fw | <p class="shorttext synchronized">Datos para la T16FW</p>
    "! @parameter iv_fill_t16fc | <p class="shorttext synchronized">Datos para la T16FC</p>
    "! @parameter iv_fill_t16fs | <p class="shorttext synchronized">Datos para la T16FS</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS fill_it_upd_md_approvers
      IMPORTING
        iv_code       TYPE t16fw-frgco
        is_approver   TYPE zrel_bo_sc_strategy_approvers
        iv_fill_t16fw TYPE sap_bool DEFAULT abap_true
        iv_fill_t16fc TYPE sap_bool DEFAULT abap_true
        iv_fill_t16fs TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_return     TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Rellena las IT del maestro para borrar los aprobadores</p>
    "! @parameter is_approver | <p class="shorttext synchronized">Aprobación</p>
    "! @parameter iv_fill_t16fw | <p class="shorttext synchronized">Datos para la T16FW</p>
    METHODS fill_it_dele_md_approvers
      IMPORTING
        is_approver TYPE zrel_bo_sc_strategy_approvers.
    "! <p class="shorttext synchronized">Determina el código de aprobación para la actualización</p>
    "! En este proceso también se actualiza las tablas internas para actualizar el customizing
    "! @parameter is_approvers | <p class="shorttext synchronized">Aprobador</p>
    "! @parameter ev_code | <p class="shorttext synchronized">Código</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS det_update_approver_code
      IMPORTING
        is_approver TYPE zrel_bo_sc_strategy_approvers
      EXPORTING
        ev_code     TYPE frgco
        et_return   TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Rellena el borrado del usuario en todos sus grupos</p>
    "! @parameter is_old_approver | <p class="shorttext synchronized">Datos de aprobación</p>
    METHODS fill_del_user_groups
      IMPORTING
        is_old_approver TYPE zrel_bo_sc_strategy_apprv_sap .
    "! <p class="shorttext synchronized">Rellena el reemplazo de un usuario de todos los grupos</p>
    "! @parameter is_old_approver | <p class="shorttext synchronized">Antiguo aprobador</p>
    "! @parameter iv_new_approver | <p class="shorttext synchronized">Nuevo aprobador</p>
    METHODS fill_repl_user_groups
      IMPORTING
        is_old_approver TYPE zrel_bo_sc_strategy_approvers
        is_new_approver TYPE zrel_bo_sc_strategy_approvers.
    "! <p class="shorttext synchronized">Construye los estados de liberación</p>
    METHODS build_liberation_statuses.
    "! <p class="shorttext synchronized">Ajuste T16FS para eliminar los aprobadores borrados</p>
    "! @parameter cs_t16fs | <p class="shorttext synchronized">Estructura T16FS</p>
    METHODS delete_approvers_t16fs
      CHANGING
        cs_t16fs TYPE t16fs.
    "! <p class="shorttext synchronized">Borra un importe</p>
    "! @parameter cs_amount | <p class="shorttext synchronized">Importe</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS delete_amount
      IMPORTING
        is_amount TYPE zrel_bo_sc_strategy_amount
      EXPORTING
        et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Lectura de constantes</p>
    METHODS read_constants.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_strategy_md_crud IMPLEMENTATION.
  METHOD constructor.
    mv_langu = iv_langu.

    " Consulta de datos de maestros
    mo_md_query = NEW zcl_rel_strategy_md_query( iv_langu = mv_langu ).
    mo_general_md = NEW zcl_rel_general_master_data( iv_langu = mv_langu ).

    " Inicialización de variables del BOPF
    init_bopf(  ).

    read_constants(  ).

  ENDMETHOD.


  METHOD process_buyers.
    DATA lt_db TYPE STANDARD TABLE OF zm014.

    CLEAR et_return.

    DATA(lt_buyers) = VALUE zrel_bo_i_strategy_buyers( FOR <wa1> IN mt_buyers WHERE ( cdchngind IS NOT INITIAL )
                                                       ( CORRESPONDING #( <wa1> ) ) ).

    IF lt_buyers IS NOT INITIAL.

      " Leo si tengo entrada en el bloque de actualización de status
      READ TABLE mt_md_change_status ASSIGNING FIELD-SYMBOL(<ls_md_st>)
                                     WITH KEY md_block = zif_rel_data=>cs_strategy-master_data-blocks-buyers.
      IF sy-subrc NE 0.
        INSERT VALUE #( md_block = zif_rel_data=>cs_strategy-master_data-blocks-buyers
                        md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-pending )
               INTO TABLE mt_md_change_status ASSIGNING <ls_md_st>.
      ENDIF.

      " Si no esta completado se actualiza
      IF <ls_md_st>-md_status NE zif_rel_data=>cs_strategy-master_data-change_md_status-done.

        " El proceso de actualizar compradores es simple. Es añadir o quitar registros de la tabla ZM014
        LOOP AT lt_buyers ASSIGNING FIELD-SYMBOL(<ls_buyers>).
          CASE <ls_buyers>-cdchngind.
            WHEN zif_rel_data=>cs_strategy-change_request-change_indicator-insert.
              INSERT VALUE #( ekgrp = ms_header-purchase_group
                              zusuario = <ls_buyers>-username
                              zfirmante = <ls_buyers>-username_desc ) INTO TABLE lt_db.
            WHEN zif_rel_data=>cs_strategy-change_request-change_indicator-delete.
              DELETE FROM zm014 WHERE ekgrp = ms_header-purchase_group
                                      AND zusuario = <ls_buyers>-username.
          ENDCASE.
        ENDLOOP.

        IF lt_db IS NOT INITIAL.
          MODIFY zm014 FROM TABLE lt_db.
        ENDIF.



        " Aquí no se espera errores por lo tanto se devuelve siempre un mensaje que ha ido bien
        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_success
                                                    i_id         = zif_rel_data=>cs_msg-id
                                                    i_number     = '015'
                                                    i_langu      = mv_langu
                                                    i_message_v1 = ms_header-purchase_group ) INTO TABLE et_return.

        " A nivel de bloque se indica que se ha realizado el proceso.
        <ls_md_st>-md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-done.


        " Grabo los datos del log
        update_bopf_md_status(
          EXPORTING
            it_md_change_status = VALUE #( ( <ls_md_st> ) )
            it_return = et_return ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD process_pgroup.

    DATA(lv_error) = abap_false.

    CLEAR et_return.

    IF ms_pgroup-cdchngind IS NOT INITIAL.

      " Leo si tengo entrada en el bloque de actualización de status
      READ TABLE mt_md_change_status ASSIGNING FIELD-SYMBOL(<ls_md_st>)
                                     WITH KEY md_block = zif_rel_data=>cs_strategy-master_data-blocks-pgroup.
      IF sy-subrc NE 0.
        INSERT VALUE #( md_block = zif_rel_data=>cs_strategy-master_data-blocks-pgroup
                        md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-pending )
               INTO TABLE mt_md_change_status ASSIGNING <ls_md_st>.
      ENDIF.

      " Si no esta completado se actualiza
      IF <ls_md_st>-md_status NE zif_rel_data=>cs_strategy-master_data-change_md_status-done.

        " Si viene un update es simple, solo se actualiza el texto
        IF ms_pgroup-cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-update.
          UPDATE t024 SET eknam = ms_pgroup-purchase_group_desc
                 WHERE ekgrp = ms_header-purchase_group.
          IF sy-subrc = 0.
            INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_success
                                                      i_id         = zif_rel_data=>cs_msg-id
                                                      i_number     = '020'
                                                      i_langu      = mv_langu
                                                      i_message_v1 = ms_header-purchase_group ) INTO TABLE et_return.

          ELSE.
            lv_error = abap_true.
            INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                                                    i_id         = zif_rel_data=>cs_msg-id
                                                    i_number     = '021'
                                                    i_langu      = mv_langu
                                                    i_message_v1 = ms_header-purchase_group ) INTO TABLE et_return.
          ENDIF.
        ENDIF.


        IF lv_error = abap_false.
          <ls_md_st>-md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-done.
          IF ms_global_options-commit = abap_true.
            COMMIT WORK AND WAIT.
          ENDIF.
        ELSE.
          <ls_md_st>-md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-error.
        ENDIF.

        " Grabo los datos del log
        update_bopf_md_status(
          EXPORTING
            it_md_change_status = VALUE #( ( <ls_md_st> ) )
            it_return = et_return ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD process_amounts.

    CLEAR: et_return.

    DATA(lt_amounts) = VALUE zrel_bo_i_strategy_amount( FOR <wa1> IN mt_amounts WHERE ( cdchngind IS NOT INITIAL )
                                                        ( CORRESPONDING #( <wa1> ) ) ).

    IF lt_amounts IS NOT INITIAL.
      " Leo si tengo entrada en el bloque de actualización de status
      READ TABLE mt_md_change_status ASSIGNING FIELD-SYMBOL(<ls_md_st>)
                                     WITH KEY md_block = zif_rel_data=>cs_strategy-master_data-blocks-amount.
      IF sy-subrc NE 0.
        INSERT VALUE #( md_block = zif_rel_data=>cs_strategy-master_data-blocks-amount
                        md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-pending )
               INTO TABLE mt_md_change_status ASSIGNING <ls_md_st>.
      ENDIF.

      " Si no esta completado se actualiza
      IF <ls_md_st>-md_status NE zif_rel_data=>cs_strategy-master_data-change_md_status-done.

        LOOP AT lt_amounts ASSIGNING FIELD-SYMBOL(<ls_amount>).

          " Como es normal los procesos de creación, actualización o borrado son distintos por lo que se llama
          " a métodos distintos.
          IF <ls_amount>-cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-update.

            update_amount( EXPORTING is_amount = <ls_amount>
                           IMPORTING et_return = DATA(lt_return_update) ).
            INSERT LINES OF lt_return_update  INTO TABLE et_return.


          ELSEIF <ls_amount>-cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-insert.

            insert_amount(  EXPORTING is_amount = <ls_amount>
                            IMPORTING et_return = DATA(lt_return_insert) ).

            INSERT LINES OF lt_return_insert  INTO TABLE et_return.

          ELSEIF <ls_amount>-cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-delete.
            delete_amount(  EXPORTING is_amount = <ls_amount>
                                       IMPORTING et_return = DATA(lt_return_delete) ).

            INSERT LINES OF lt_return_delete  INTO TABLE et_return.
          ENDIF.

        ENDLOOP.

        " Si hay algun error se marca el bloque como erroneo y se hará rollback. Si no hay ningún error entonces
        " se hace un commit
        READ TABLE et_return TRANSPORTING NO FIELDS WITH KEY type = zif_rel_data=>cs_msg-type_error.
        IF sy-subrc = 0.
          <ls_md_st>-md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-error.

          CALL FUNCTION 'BAPI_TRANSACTION_ROLLBACK'.
        ELSE.
          <ls_md_st>-md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-done.
          CALL FUNCTION 'BAPI_TRANSACTION_COMMIT'
            EXPORTING
              wait = 'X'.
        ENDIF.

        " Grabo los datos del log
        update_bopf_md_status(
          EXPORTING
            it_md_change_status = VALUE #( ( <ls_md_st> ) )
            it_return = et_return ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD update_amount.
    DATA: lt_allocvaluesnum  TYPE STANDARD TABLE OF bapi1003_alloc_values_num WITH EMPTY KEY.
    DATA: lt_allocvalueschar TYPE STANDARD TABLE OF bapi1003_alloc_values_char WITH EMPTY KEY.
    DATA: lt_allocvaluescurr TYPE STANDARD TABLE OF bapi1003_alloc_values_curr WITH EMPTY KEY.
    DATA: lt_return TYPE STANDARD TABLE OF bapiret2 WITH EMPTY KEY.
    DATA: lv_status        TYPE clstatus.
    DATA: lv_standardclass TYPE stdclass.
    DATA lt_bsart TYPE STANDARD TABLE OF atwrt.

    CLEAR: et_return.

    " Primero hemos de sacar los datos actuales que tiene el sistema de clasificación
    " para el código y grupo de liberación

    " Sacamos la tabla
    DATA(lv_obtab) = get_klart_table( ).

    DATA(lv_objectkey) = CONV objnum( |{ is_amount-group }{ is_amount-strategy }| ).
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objectkey
        objecttable     = lv_obtab
        classnum        = zif_rel_data=>cs_strategy-classification-klasse
        classtype       = zif_rel_data=>cs_strategy-classification-klart
      IMPORTING
        status          = lv_status
        standardclass   = lv_standardclass
      TABLES
        allocvaluesnum  = lt_allocvaluesnum[]
        allocvalueschar = lt_allocvalueschar[]
        allocvaluescurr = lt_allocvaluescurr[]
        return          = lt_return[].

    READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return_bapi>) WITH KEY type = zif_rel_data=>cs_msg-type_error.
    IF sy-subrc NE 0.

      CLEAR lt_return.

      " El proceso de actualización puede llamarse desde la creación porque se este reaprovechando la estrategia. En ese caso
      " tengo que verificar si tengo el grupo de compras en los datos leídos y en caso de no tenerlo añadirlo.
      DATA(lv_charact_ekgrp) = zcl_rel_utilities=>conv_atinn_2_atnam( zif_rel_data=>cs_strategy-classification-fields_charac-ekgrp ).
      IF NOT line_exists( lt_allocvalueschar[ charact = lv_charact_ekgrp
                                             value_char = ms_header-purchase_group ] ).
        " Para desactivar el sistema de clasificación lo que hacen es quitar el grupo de compra a la característica. Por ello miro si hay
        " algun registro en blanco para actualizarlo. " Si no lo encuentro lo añado.
        ASSIGN lt_allocvalueschar[ charact = lv_charact_ekgrp
                                   value_char = '' ] TO FIELD-SYMBOL(<ls_values_char>).
        IF sy-subrc = 0.
          <ls_values_char>-value_char = ms_header-purchase_group.
        ELSE.
          INSERT VALUE #( charact = lv_charact_ekgrp
                          value_char = ms_header-purchase_group )
         INTO TABLE lt_allocvalueschar.
        ENDIF.
      ENDIF.

      " Refrescamos las clases de documento.
      DATA(lv_charact_bsart) = zcl_rel_utilities=>conv_atinn_2_atnam( zif_rel_data=>cs_strategy-classification-fields_charac-bsart ).
      DELETE lt_allocvalueschar WHERE charact = lv_charact_bsart.
      zcl_rel_constants=>get_constante_en_tabla( EXPORTING iv_pattern = 'CHARACT_BSART_%'
                                                 CHANGING ct_table   = lt_bsart ).
      lt_allocvalueschar = VALUE #( BASE lt_allocvalueschar FOR <wa> IN lt_bsart ( charact = lv_charact_bsart
                                                                                  value_char = <wa> ) ).


      " NOTA IVAN: Cuando se llama a este método dentro del proceso de aprobación vía HTTP esta provocando errores de formato con los importes tanto
      " en conversión (y aunque lo solución me da error en la bapi de actualizacion).
      " Creo que es debido al valor de esta variable (SAPLCTCV)P_DFLAG, separador decimal, que se determina en algun momento y provoca errores.
      " Si el proceso de actualización se llama fuera del contexto de aprobación, da igual si es online o web, entonces funciona bien las conversiones.
      " Por ello se lanza en una función en proceso de fondo cuando se aprueba la solicitud.

      " Necesitamos convertir los valores externos (campos importe) en el formato interno del sistema de clasificación.
      " El resultado se guarda en el primer registro (solo habrá uno) del sistema de clasificación.
      zcl_rel_utilities=>ctbp_convert_value_ext_to_int(
        EXPORTING
          iv_charctinn       = zif_rel_data=>cs_strategy-classification-fields_charac-gnetw
          iv_amount          = is_amount-amount
          iv_amount2         = is_amount-amount2
          iv_operand         = is_amount-amount_operand
          iv_currency        = is_amount-currency
        IMPORTING
          ev_value_from      = lt_allocvaluescurr[ 1 ]-value_from
          ev_value_to        = lt_allocvaluescurr[ 1 ]-value_to
          ev_value_relation  = lt_allocvaluescurr[ 1 ]-value_relation ).

      CALL FUNCTION 'BAPI_OBJCL_CHANGE'
        EXPORTING
          objectkey          = lv_objectkey
          objecttable        = lv_obtab
          classnum           = zif_rel_data=>cs_strategy-classification-klasse
          classtype          = zif_rel_data=>cs_strategy-classification-klart
        TABLES
          allocvaluesnumnew  = lt_allocvaluesnum[]
          allocvaluescharnew = lt_allocvalueschar[]
          allocvaluescurrnew = lt_allocvaluescurr[]
          return             = lt_return[].

      READ TABLE lt_return ASSIGNING <ls_return_bapi> WITH KEY type = zif_rel_data=>cs_msg-type_error.
      IF sy-subrc = 0.
        INSERT <ls_return_bapi> INTO TABLE et_return.
      ELSE.
        INSERT zcl_ca_utilities=>fill_return( iv_id = zif_rel_data=>cs_msg-id
                                                iv_type = zif_rel_data=>cs_msg-type_success
                                                iv_number = '022'
                                                iv_message_v1 = is_amount-group
                                                iv_message_v2 = is_amount-strategy ) INTO TABLE et_return.
      ENDIF.

    ELSE.
      INSERT <ls_return_bapi> INTO TABLE et_return.
    ENDIF.

  ENDMETHOD.



  METHOD init_bopf.
    " Instancia las variables para los BOPF
    TRY.
        " Inicialización del gestor transaccional actualizaciones, bloqueos, etc..
        mo_txn_mngr = /bobf/cl_tra_trans_mgr_factory=>get_transaction_manager( ).

        " Creación del el gestor de servicios del BOPF. Permite realizar las llamadas al BOPF para ejecutar validaciones, acciones, añadir, etc..
        " Es la clase más importante ya que toda la gestión CRUD se realiza en esta clase
        mo_svc_mngr = /bobf/cl_tra_serv_mgr_factory=>get_service_manager( zif_rel_bo_strategy_c=>sc_bo_key ).

        " Creación de la configuración del BOPF, permite obtener los metadas del BOPF
        mo_conf_mngr = /bobf/cl_frw_factory=>get_configuration( zif_rel_bo_strategy_c=>sc_bo_key ).

        " Clase con utilidades de BOPF
        mo_bopf_util = NEW zcl_ca_bopf_util( zif_rel_bo_strategy_c=>sc_bo_key ).

        " Helper del BOPF
        mo_bopf_helper = NEW zcl_rel_helper_bo( iv_langu = mv_langu ).


      CATCH /bobf/cx_frw.
        "TODO: Error handling...
    ENDTRY.
  ENDMETHOD.

  METHOD dispatch_update_master_data.

    CLEAR: et_return.
    ev_master_data_updated = abap_false.

    get_values_from_bopf( EXPORTING iv_key = iv_key iv_request_id = iv_request_id ).

    IF mt_header IS NOT INITIAL.

      " Proceso del grupo de compras
      process_pgroup( IMPORTING et_return = DATA(lt_return_pgroup) ).
      INSERT LINES OF lt_return_pgroup INTO TABLE et_return.


      " Proceso de compradores
      process_buyers( IMPORTING et_return = DATA(lt_return_buyers) ).
      INSERT LINES OF lt_return_buyers INTO TABLE et_return.

      " Proceso de las estrategias
      process_strategy( IMPORTING et_return = DATA(lt_return_strategy) ).
      INSERT LINES OF lt_return_strategy INTO TABLE et_return.


      " Se actualizan los datos del BOPF
      update_bopf_values(  ).

      " Devuelve si los datos maestros están completamente actualizados
      ev_master_data_updated = COND #( WHEN ms_header-change_md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-done THEN abap_true ELSE abap_false ).


    ELSE.
      INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                                             i_id         = zif_rel_data=>cs_msg-id
                                             i_number     = '007'
                                             i_langu      = mv_langu
                                             i_message_v1 = |{ iv_request_id ALPHA = OUT }| ) INTO TABLE et_return.
    ENDIF.

  ENDMETHOD.


  METHOD get_values_from_bopf.
    mt_header = mo_bopf_helper->get_header_from_key_fields( EXPORTING iv_key = iv_key iv_request_id = iv_request_id ).

    IF mt_header IS NOT INITIAL.

      " Aprovechamos el método existente para recuperar toda la info
      mo_bopf_helper->query_all_data_from_header( EXPORTING it_params = VALUE /bobf/t_frw_query_selparam( ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-key
                                                                                                            sign = 'I'
                                                                                                            option = 'EQ'
                                                                                                            low = mt_header[ 1 ]-key ) )
                                                   IMPORTING et_buyers = mt_buyers
                                                             et_amounts = mt_amounts
                                                             et_amounts_sap = mt_amounts_sap
                                                             et_approvers = mt_approvers
                                                             et_approvers_sap = mt_approvers_sap
                                                             et_pgroup = mt_pgroup
                                                             et_md_change_status = mt_md_change_status ).

      " Paso la cabecera y datos de grupo de compras a una estructura porque solo habrá un solo registro y es más comodo procesarlo,
      " aparte que en determinadas sentencias no funciona el "[ 1 ]-"
      ms_header = mt_header[ 1 ].
      ms_pgroup = mt_pgroup[ 1 ].

    ENDIF.
  ENDMETHOD.


  METHOD update_bopf_values.

    " Datos de cabecera
    update_bopf_header(  ).

    " Se graban los datos
    mo_bopf_helper->save_data_bopf(  ).

  ENDMETHOD.


  METHOD update_bopf_md_status.
    DATA lt_mod TYPE /bobf/t_frw_modification .
    DATA lt_change_log TYPE zrel_bo_i_strategy_md_log .


    DATA(lt_change_status) = it_md_change_status.

    " Los datos del log pueden venir de dos maneras: ya formateados o la tabla con el retorno.
    IF it_md_change_log IS NOT INITIAL.
      lt_change_log = it_md_change_log.
    ELSEIF it_return IS NOT INITIAL.
      " Añade el resultado al log. El bloque tomo el primero
      add_md_change_log( EXPORTING iv_block = it_md_change_status[ 1 ]-md_block
                                   it_return = it_return
                        IMPORTING  ev_key = DATA(lv_key)
                                   et_change_log = lt_change_log ).

    ENDIF.

    LOOP AT lt_change_status REFERENCE INTO DATA(lo_md_change_status).
      " Actualizo la clave del último log si el log viene en formato RETURN.
      IF it_return IS NOT INITIAL.
        lo_md_change_status->key_last_log = lv_key.
      ENDIF.

      IF lo_md_change_status->key IS NOT INITIAL. " Existe clave tengo que actualizar.
        INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-md_change_status
             change_mode = /bobf/if_frw_c=>sc_modify_update
             key = lo_md_change_status->key
             source_node = zif_rel_bo_strategy_c=>sc_node-root
             association = zif_rel_bo_strategy_c=>sc_association-root-md_change_status
             source_key = lo_md_change_status->root_key
             data = lo_md_change_status ) INTO TABLE lt_mod.
      ELSE. " Hay que insertar
        lo_md_change_status->key = /bobf/cl_frw_factory=>get_new_key( ).
        INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-md_change_status
               change_mode = /bobf/if_frw_c=>sc_modify_create
               key = lo_md_change_status->key
               source_node = zif_rel_bo_strategy_c=>sc_node-root
               association = zif_rel_bo_strategy_c=>sc_association-root-md_change_status
               source_key =  ms_header-key
               data = lo_md_change_status ) INTO TABLE lt_mod.
      ENDIF.

      " Añadimos el registro de log
      LOOP AT lt_change_log REFERENCE INTO DATA(lo_md_change_log) WHERE md_block = lo_md_change_status->md_block.
        INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-md_change_log
                       change_mode = /bobf/if_frw_c=>sc_modify_create
                       key = lo_md_change_log->key
                       source_node = zif_rel_bo_strategy_c=>sc_node-md_change_status
                       association = zif_rel_bo_strategy_c=>sc_association-md_change_status-md_change_log
                       source_key =  lo_md_change_status->key
                       root_key = ms_header-key
                       data = lo_md_change_log ) INTO TABLE lt_mod.
      ENDLOOP.
    ENDLOOP.

    mo_bopf_helper->save_data_bopf( EXPORTING it_mod = lt_mod ).

  ENDMETHOD.


  METHOD update_bopf_header.
    DATA lt_mod TYPE /bobf/t_frw_modification .

    GET REFERENCE OF ms_header INTO DATA(lo_header).

    " Si hay algun blorque erróneo entonces el estado global será erroneo
    READ TABLE mt_md_change_status TRANSPORTING NO FIELDS WITH KEY md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-error.
    IF sy-subrc = 0.
      lo_header->change_md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-error.
    ELSE.
      lo_header->change_md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-done.
    ENDIF.

    INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-root
            change_mode = /bobf/if_frw_c=>sc_modify_update
            key = lo_header->key
            data = lo_header
            changed_fields = VALUE #( ( zif_rel_bo_strategy_c=>sc_node_attribute-root-change_md_status ) ) )
         INTO TABLE lt_mod.


    " Solo actualizamos el BOPF la grabación real se hace fuera
    mo_bopf_helper->modify_data_bopf( EXPORTING it_mod = lt_mod ).


  ENDMETHOD.


  METHOD update_bopf_node_amounts.
    DATA lt_mod TYPE /bobf/t_frw_modification .

    " Solo actualizo los registros que se añaden, que es donde se ha determinado el grupo y strategia.
    LOOP AT mt_amounts REFERENCE INTO DATA(lo_amounts) WHERE cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-insert.
      INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-amount
                      change_mode = /bobf/if_frw_c=>sc_modify_update
                      key = lo_amounts->key
                      data = lo_amounts
                      source_node = zif_rel_bo_strategy_c=>sc_node-root
                      association = zif_rel_bo_strategy_c=>sc_association-root-amount
                      source_key =  ms_header-key
                      changed_fields = VALUE #( ( zif_rel_bo_strategy_c=>sc_node_attribute-amount-group )
                                                ( zif_rel_bo_strategy_c=>sc_node_attribute-amount-strategy ) ) )
                   INTO TABLE lt_mod.
    ENDLOOP.
    IF sy-subrc = 0.
      mo_bopf_helper->modify_data_bopf( EXPORTING it_mod = lt_mod ).
    ENDIF.

  ENDMETHOD.

  METHOD update_bopf_node_approvers.
    DATA lt_mod TYPE /bobf/t_frw_modification .

    " Solo actualizo los registros que se añaden o actualizan.Al añadir se determina grupo, strategia y código. En la actualización solo el código.
    LOOP AT mt_approvers REFERENCE INTO DATA(lo_approvers) WHERE cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-insert
                                                                 OR cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-update.
      INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-approvers
                      change_mode = /bobf/if_frw_c=>sc_modify_update
                      key = lo_approvers->key
                      data = lo_approvers
                      source_node = zif_rel_bo_strategy_c=>sc_node-root
                      association = zif_rel_bo_strategy_c=>sc_association-root-approvers
                      source_key =  ms_header-key
                      changed_fields = VALUE #( ( zif_rel_bo_strategy_c=>sc_node_attribute-approvers-group )
                                                ( zif_rel_bo_strategy_c=>sc_node_attribute-approvers-strategy )
                                                ( zif_rel_bo_strategy_c=>sc_node_attribute-approvers-code ) ) )
                   INTO TABLE lt_mod.
    ENDLOOP.
    IF sy-subrc = 0.
      mo_bopf_helper->modify_data_bopf( EXPORTING it_mod = lt_mod ).
    ENDIF.

  ENDMETHOD.


  METHOD add_md_change_log.

    CLEAR: et_change_log.

    LOOP AT it_return ASSIGNING FIELD-SYMBOL(<ls_return>).
      " Genero aquí la clave y no en el momento de grabar para devoverla y que se devuelva para que se guarde en el registro del status
      " de modificación de datos maestros
      ev_key = /bobf/cl_frw_factory=>get_new_key( ).

      INSERT VALUE #( key = ev_key
                      md_block = iv_block
                      type = <ls_return>-type
                      id = <ls_return>-id
                      number = <ls_return>-number
                      message_v1 = <ls_return>-message_v1
                      message_v2 = <ls_return>-message_v2
                      message_v3 = <ls_return>-message_v3
                      message_v4 = <ls_return>-message_v4 ) INTO TABLE et_change_log.

    ENDLOOP.

  ENDMETHOD.


  METHOD insert_amount.
    DATA: lt_allocvaluesnum  TYPE STANDARD TABLE OF bapi1003_alloc_values_num WITH EMPTY KEY.
    DATA: lt_allocvalueschar TYPE STANDARD TABLE OF bapi1003_alloc_values_char WITH EMPTY KEY.
    DATA: lt_allocvaluescurr TYPE STANDARD TABLE OF bapi1003_alloc_values_curr WITH EMPTY KEY.
    DATA: lt_return TYPE STANDARD TABLE OF bapiret2 WITH EMPTY KEY.
    DATA lt_bsart TYPE STANDARD TABLE OF atwrt.

    CLEAR: et_return.

    " Insertar una cantidad no es insertar directamente al sistema de clasificación. El motivo
    " es que la estrategia puede ser reaprovechada por eso hay que verificar si existe de antemano.
    " A ver, si todo fuese bien podría saberlo porque en la tabla global "mt_strag_code_new_created"
    " sabemos si se reaprovecha o no. Pero como puede habe fallos y hay reprocesaos esa info la pierdo
    " porque el custo y códigos de estrategias es lo primero que se graba y los códigos nuevos ya han sido
    " establecidos.
    IF mo_md_query->check_exist_strateg_classif( iv_group = is_amount-group
                                              iv_strategy = is_amount-strategy  ).

      " Si existe llamamos al proceso de actualización
      update_amount( EXPORTING is_amount = is_amount
                     IMPORTING et_return = et_return ).

    ELSE.


      " Obtiene la tabla asociada a la categoria de la clasificación
      DATA(lv_obtab) = get_klart_table( ).

      DATA(lv_objectkey) = CONV objnum( |{ is_amount-group }{ is_amount-strategy }| ).

      " Informamos del importe
      INSERT VALUE #( charact = zcl_rel_utilities=>conv_atinn_2_atnam( zif_rel_data=>cs_strategy-classification-fields_charac-gnetw )
                      currency_from = is_amount-currency
                      currency_to = COND #( WHEN is_amount-amount2 IS NOT INITIAL THEN is_amount-currency ) )
             INTO TABLE lt_allocvaluescurr ASSIGNING FIELD-SYMBOL(<ls_valuescurr>).

      " Convertimos el importe
      zcl_rel_utilities=>ctbp_convert_value_ext_to_int( EXPORTING iv_charactname       = <ls_valuescurr>-charact
                                                                  iv_amount          = is_amount-amount
                                                                  iv_amount2         = is_amount-amount2
                                                                  iv_operand         = is_amount-amount_operand
                                                                  iv_currency        = is_amount-currency
                                                        IMPORTING ev_value_from      = <ls_valuescurr>-value_from
                                                                  ev_value_to        = <ls_valuescurr>-value_to
                                                                  ev_value_relation  = <ls_valuescurr>-value_relation ).

      " Ahora el grupo de compras
      INSERT VALUE #( charact = zcl_rel_utilities=>conv_atinn_2_atnam( zif_rel_data=>cs_strategy-classification-fields_charac-ekgrp )
                      value_char = ms_header-purchase_group )
                   INTO TABLE lt_allocvalueschar.

      " Clases de documento
      DATA(lv_charact_bsart) = zcl_rel_utilities=>conv_atinn_2_atnam( zif_rel_data=>cs_strategy-classification-fields_charac-bsart ).
      zcl_rel_constants=>get_constante_en_tabla( EXPORTING iv_pattern = 'CHARACT_BSART_%'
                                                 CHANGING ct_table   = lt_bsart ).
      lt_allocvalueschar = VALUE #( BASE lt_allocvalueschar FOR <wa> IN lt_bsart ( charact = lv_charact_bsart
                                                                                  value_char = <wa> ) ).

      CALL FUNCTION 'BAPI_OBJCL_CREATE'
        EXPORTING
          objectkeynew      = lv_objectkey
          objecttablenew    = lv_obtab
          classnumnew       = zif_rel_data=>cs_strategy-classification-klasse
          classtypenew      = zif_rel_data=>cs_strategy-classification-klart
          status            = '1'
          no_default_values = abap_true
        TABLES
          allocvaluesnum    = lt_allocvaluesnum
          allocvalueschar   = lt_allocvalueschar
          allocvaluescurr   = lt_allocvaluescurr
          return            = lt_return.

      READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return_bapi>) WITH KEY type = zif_rel_data=>cs_msg-type_error.
      IF sy-subrc = 0.
        INSERT <ls_return_bapi> INTO TABLE et_return.

      ELSE.

        INSERT zcl_ca_utilities=>fill_return( iv_id = zif_rel_data=>cs_msg-id
                                                      iv_type = zif_rel_data=>cs_msg-type_success
                                                      iv_number = '022'
                                                      iv_message_v1 = is_amount-group
                                                      iv_message_v2 = is_amount-strategy ) INTO TABLE et_return.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD determine_new_strategy.

    CLEAR: et_return.

    " Aunque todo el proceso de solicitud se graba en el orden que viene de frontend, que supuestamente esta ordenado, prefiero ordenar por nivel
    " para evitar problemas.
    SORT mt_strag_code_new_created BY level.

    " El objetivo del proceso es que las estrategias se creen de manera consecutiva lo máxima posible. Por ello hay varios frentes iniciales
    " 1) Que se este añadiendo un nivel nuevo a un grupo de compras. En ese caso hay que mirar la última estrategia
    " 2) Que este añadiendo un nivel de cero porque no hay nada anteriormente.
    " En ambos casos hay que intentar que sean consecutivos todos los niveles nuevos.

    " Generamos las combinaciones, para ajustarlo al custo actual solo se van a generar con mayúsculas y que comiencen por numeros. Esto nos servirá
    " para ir mirando las libres y que sean consecutivas.
    mt_generator_comb_strategies = NEW zcl_ca_combinations_generator( iv_only_upper_case = abap_true
                                                        iv_start_comb_with = zcl_ca_combinations_generator=>cs_start_combination_with-number )->generate_combinations( iv_comb_number = 2 ).

    " Primero vamos a mirar desde donde vamos a empezar a buscar. Para saber el grupo y strategia del nivel anterior me voy a los datos de la foto que hay en SAP ya que hay se guarda
    " toda la info y no como en la petición que pueden estar, o no, todas las estrategias.
    DATA(lv_tabix_comb_ini) = 0.
    ASSIGN mt_amounts_sap[ level = ( mt_strag_code_new_created[ 1 ]-level - 1 ) ] TO FIELD-SYMBOL(<ls_amounts_sap>).
    IF sy-subrc = 0.
      READ TABLE mt_generator_comb_strategies ASSIGNING FIELD-SYMBOL(<ls_comb>) WITH KEY value = <ls_amounts_sap>-strategy.
      IF sy-subrc = 0.
        lv_tabix_comb_ini = sy-tabix.
      ENDIF.
    ENDIF.

    " Buscamos los bloques libres. Este proceso comienza, un registro más, de la combinación encontrada y va buscando bloques. Si no encuentra
    " se vuelve a llamar a si mismo pero incremento en uno el registro a partir del cual buscar. Hasta encontrar un hueco libre.
    " Este proceso si solo insertamos un solo nivel buscará
    search_free_block_strategies( EXPORTING iv_from_tabix = lv_tabix_comb_ini
                                            iv_size_block = lines( mt_strag_code_new_created )
                                  IMPORTING et_blocks = DATA(lt_blocks) ).


    " Si no encuentro bloques consecutivos a partir de la estrategia que estoy creando. Tengo dos posibilidades:
    " 1) Si he buscado a partir de una combinación anterior, busco consecutivos desde el inicio de las combinaciones.
    " 2) Si estoy buscando desde el inicio, porque es un proceso nuevo entonces busco las estrategias libres aunque no sean consecutivas.
    IF lt_blocks IS INITIAL.
      IF lv_tabix_comb_ini NE 0.
        search_free_block_strategies( EXPORTING iv_from_tabix = 0
                                                    iv_size_block = lines( mt_strag_code_new_created )
                                          IMPORTING et_blocks = lt_blocks ).
        IF lt_blocks IS INITIAL.
          " Si desde el inicio he encontrado bloques consecutivos entonces busco desde el inicio y lo que encuentre libre
          search_free_block_strategies( EXPORTING iv_from_tabix = 0
                                                  iv_size_block = lines( mt_strag_code_new_created )
                                                  iv_block_contiguos = abap_false
                                            IMPORTING et_blocks = lt_blocks ).
        ENDIF.
      ELSE.
        search_free_block_strategies( EXPORTING iv_from_tabix = 0
                                                iv_size_block = lines( mt_strag_code_new_created )
                                                iv_block_contiguos = abap_false
                                          IMPORTING et_blocks = lt_blocks ).
      ENDIF.
    ENDIF.

    " Si finalmente tengo bloques actualizo la tabla de equivalencias
    IF lt_blocks IS NOT INITIAL.
      DATA(lv_tabix) = 1.
      LOOP AT mt_strag_code_new_created ASSIGNING FIELD-SYMBOL(<ls_strag_code_new>).
        READ TABLE lt_blocks ASSIGNING FIELD-SYMBOL(<ls_blocks>) INDEX lv_tabix.
        <ls_strag_code_new>-strategy_created = <ls_blocks>-strategy.
        <ls_strag_code_new>-strategy_reused = <ls_blocks>-reused.
        lv_tabix = lv_tabix + 1.
      ENDLOOP.

    ELSE.
      INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                            iv_id         = zif_rel_data=>cs_msg-id
                                            iv_number     = '030'
                                            iv_message_v1 = iv_group
                                            iv_langu      = mv_langu ) INTO TABLE et_return.
    ENDIF.


  ENDMETHOD.




  METHOD build_text_t16ft.
    DATA lt_langu TYPE STANDARD TABLE OF sylangu.
    DATA lt_t16ft TYPE STANDARD TABLE OF t16ft.

    CLEAR: rt_values.

    " Saco los idioma donde tengo que poner el texto
    zcl_rel_constants=>get_constante_en_tabla( EXPORTING iv_pattern = 'LANGU_DESC_CUSTO_%'
                                               CHANGING ct_table   = lt_langu ).
    IF lt_langu IS INITIAL. " Si no idioma, que no debería de ocurrir, relleno los idiomas por defecto
      INSERT 'S' INTO TABLE lt_langu.
      INSERT 'E' INTO TABLE lt_langu.
      INSERT 'D' INTO TABLE lt_langu.
      INSERT 'F' INTO TABLE lt_langu.
    ENDIF.

    " Construyo el texto
    DATA(lv_text) = COND #( WHEN iv_text IS INITIAL
                     THEN |{ mt_pgroup[ 1 ]-purchase_group_desc  } { iv_level }|
                     ELSE iv_text ).
    rt_values = VALUE #( FOR <wa> IN lt_langu ( frggr = iv_group
                                    frgsx = iv_strategy
                                    spras = <wa>
                                    frgxt = lv_text )  ).

  ENDMETHOD.

  METHOD process_strategy.

    CLEAR: mt_strag_code_new_created, mt_t16fs, mt_generator_comb_strategies.
    CLEAR: mt_users_from_lib_group, mt_lib_group_of_user, mt_t16fd.
    CLEAR: mt_t16ft_update, mt_t16fk_update, mt_t16fv_update, mt_t16fw_update, mt_t16fd_update.
    CLEAR: mt_t16fc_update, mt_t16fs_update.

    CLEAR: et_return.

    " Este es el proceso más complejo porque requiere de tocar en el customizing en varios sistemas y crear roles, etc..
    " Por ello vamos a dividirlo por partes:
    " 1) Vamos a tocar el customizing de SAP. Esto se tiene que hacer en los tres sistemas. Si va bien entonces hacemos los siguientes puntos:
    " 2) Actualizar el sistema de clasificación si hay cambios en los importes.
    " 3) Actualizar roles, usuarios, etc..

    " El proceso del custo de SAP solo se toca si hay cambios de aprobadores.
    IF line_exists( mt_approvers[ cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-insert ] )
       OR line_exists( mt_approvers[ cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-update ] )
       OR line_exists( mt_approvers[ cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-delete ] ).

      process_update_custo_sap( IMPORTING et_return = et_return ).
    ENDIF.

    " Si no hay error en el proceso de customizing tanto en el entorno de desarrollo como en el sistema donde estoy, y hay cambios de importes
    " entonces actualizo los importes.
    IF NOT line_exists( et_return[ type = zif_rel_data=>cs_msg-type_error field = sy-sysid ] )
       AND NOT line_exists( et_return[ type = zif_rel_data=>cs_msg-type_error field = zif_rel_data=>cs_connectivity-r3_systems-development ] )
       AND ( line_exists( mt_amounts[ cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-insert ] )
             OR line_exists( mt_amounts[ cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-update ] )
             OR line_exists( mt_amounts[ cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-delete ] ) ).

      process_amounts( IMPORTING et_return = DATA(lt_return_amount) ).
      INSERT LINES OF lt_return_amount INTO TABLE et_return.

    ENDIF.





  ENDMETHOD.


  METHOD process_update_custo_sap.
    CLEAR: et_return.


    " Leo si tengo entrada en el bloque de actualización de status
    READ TABLE mt_md_change_status ASSIGNING FIELD-SYMBOL(<ls_md_st>)
                                   WITH KEY md_block = zif_rel_data=>cs_strategy-master_data-blocks-custo_sap.
    IF sy-subrc NE 0.
      INSERT VALUE #( md_block = zif_rel_data=>cs_strategy-master_data-blocks-custo_sap
                      md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-pending )
             INTO TABLE mt_md_change_status ASSIGNING <ls_md_st>.
    ENDIF.

    " Si no esta completado se actualiza
    IF <ls_md_st>-md_status NE zif_rel_data=>cs_strategy-master_data-change_md_status-done.


      " Vamos a construir los datos que se guadarán los grupos y estrategias con los aprobadores. En este primer paso
      " no vamos a tener en cuento los aprobadores esto se realizará en un paso posterior.
      build_custo_base_t16fs( IMPORTING et_return = DATA(lt_return_custo) ).

      " Si esta vacia es que no hay errores y se continua el proceso
      IF lt_return_custo IS INITIAL.

        " Si no hay errores es momento de montar el proceso de los aprobadores.
        build_custo_approvers( IMPORTING et_return = lt_return_custo ).


        " Proceso de grabación del custo de SAP y de los nodos asociados en el BOFP: importes y aprobadores. De esta manera
        " garantizo la integridad tanto del BOPF como del custo de SAP.
        save_custo_sap_bo( IMPORTING et_return = et_return ).


        " Si hay algun error en el proceso el bloque se marcará como erroneo.
        IF line_exists( et_return[ type = zif_rel_data=>cs_msg-type_error ] ).
          <ls_md_st>-md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-error.
        ELSE.
          <ls_md_st>-md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-done.
        ENDIF.

        " Grabo los datos del log
        update_bopf_md_status(
          EXPORTING
            it_md_change_status = VALUE #( ( <ls_md_st> ) )
            it_return = et_return ).

      ELSE.
        INSERT LINES OF lt_return_custo INTO TABLE et_return.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD build_custo_base_t16fs.

    CLEAR: et_return.

    " El grupo de liberación es el mismo para todos los importes, por lo tanto leo el primer registro de importes
    " que no este en blanco o el primer digito sea el de creación.
    LOOP AT mt_approvers ASSIGNING FIELD-SYMBOL(<ls_approvers>) WHERE group IS NOT INITIAL
                                                                  AND group(1) NE zif_rel_data=>cs_strategy-change_request-strategy_ini_char_new.
      DATA(lv_group) = <ls_approvers>-group.
      EXIT.
    ENDLOOP.

    " Si por cualquier motivo no obtengo el grupo lo sacaré del grupo de compras. Aunque este método también se le pasa la sociedad,
    " a día de hoy 02/02/2022 no se usa.
    IF lv_group IS INITIAL.
      " Obtengo el grupo de liberación, básico para determinar el resto de valores.
      lv_group = mo_md_query->get_lib_group_from_pgroup( iv_purchase_group = ms_header-purchase_group  ).
    ENDIF.

    " Si con todo no hay grupo lanzo un error.
    IF lv_group IS NOT INITIAL.

      " Saco las combinaciones de grupo y estrategia donde se han modificado aprobadores
      DATA(lt_comb_group) = VALUE tt_comb_group_strat( FOR <wa1> IN mt_approvers WHERE ( cdchngind IS NOT INITIAL ) ( group = <wa1>-group
                                                                                                                      strategy = <wa1>-strategy )  ).
      SORT lt_comb_group.
      DELETE ADJACENT DUPLICATES FROM lt_comb_group COMPARING ALL FIELDS.

      LOOP AT lt_comb_group ASSIGNING FIELD-SYMBOL(<ls_comb_group>).
        " Me posiciono en el nivel de importe porque es donde tengo datos que me interesa, como por ejemplo el nivel de la estrategia en el grupo de compras.
        ASSIGN mt_amounts[ group = <ls_comb_group>-group
                          strategy = <ls_comb_group>-strategy ] TO FIELD-SYMBOL(<ls_amounts>).

        " Primero vamos a leer las estrategias asociadas al grupo
        IF mt_t16fs IS INITIAL.
          mt_t16fs = mo_md_query->get_strategys_from_group( EXPORTING iv_group = lv_group
                                                                      iv_langu = mv_langu_texto_baja ).
        ENDIF.

        " Como el customizing se actualiza en varios sistema puede llegar a ocurrir que en el sistema que se lanza, ejemplo APT,
        " el proceso vaya bien pero en el resto de sistema(GPD y GPI) vaya mal. Esto hará que el custo y el modelo interno en APT
        " sea corecto pero como ha fallado en otros sistema el bloque lo doy como erroneo.
        " Cuando haga el reproceso en MT_AMOUNTS/MT_APPROVERS no tendré el $x(donde x es un contador) sino el valor definitivo que
        " existirá en la T16FS. En ese caso no determinaré uno nuevo sino que usaré el determinado. Si, actualizará algo que ya esta actualizado
        " pero es más simple que controlar en que sistema ha fallado.
        READ TABLE mt_t16fs ASSIGNING FIELD-SYMBOL(<ls_t16fs>) WITH KEY frggr = <ls_comb_group>-group
                                                                        frgsx = <ls_comb_group>-strategy.
        IF sy-subrc = 0.

          INSERT CORRESPONDING #( <ls_t16fs> ) INTO TABLE mt_t16fs_update ASSIGNING FIELD-SYMBOL(<ls_t16fs_update>).

          " Si el importe es nuevo voy a resetear los aprobadores porque pueden ser que el codigo se reutilice porque estaba de baja.
          " Aquí de nuevo lo hacemos por el tema de reprocesos, Ya que puede ser que la actualización del custo haya fallado y no se haya grabado.
          " Por ello mejor el reset de aprobadores y tenga que volver a reprocesar.
          IF <ls_amounts>-cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-insert.
            reset_approvers_t16fs( CHANGING cs_t16fs = <ls_t16fs_update> ).

            " Miro si a nivel de aprobador hay alguno borrado, ya que si existe hay que ajustar los códigos de liberacion
          ELSEIF line_exists( mt_approvers[ group = <ls_comb_group>-group
                                            strategy = <ls_comb_group>-strategy
                                            cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-delete ] ).
            delete_approvers_t16fs( CHANGING cs_t16fs = <ls_t16fs_update> ).

            INSERT LINES OF build_text_t16ft(
            EXPORTING
              iv_group    = <ls_comb_group>-group
              iv_strategy = <ls_comb_group>-strategy
              iv_text = mv_texto_baja ) INTO TABLE mt_t16ft_update.

          ENDIF.

        ELSE.
          " Si no existe lo añado a la tabla donde tendrá la equivalencia de estrategias a crear y su codigo definitivo.
          " Añado el grupo y estrategia temporal y el grupo creado pongo el que se ha determinado
          INSERT VALUE #( group = <ls_comb_group>-group
                          strategy = <ls_comb_group>-strategy
                          group_created = lv_group
                          level = <ls_amounts>-level ) INTO TABLE mt_strag_code_new_created.
        ENDIF.
      ENDLOOP.

      " Miro la tabla donde tengo las estrategias a crear y equivalencia final para si tengo que crear o determinar nuevas estrategias
      IF mt_strag_code_new_created IS NOT INITIAL.
        determine_new_strategy( EXPORTING iv_group = lv_group
                                IMPORTING et_return = DATA(lt_return_new_strag) ).

        IF lt_return_new_strag IS INITIAL.
          " Recorro la tabla con las estrategias para:
          " 1) Añadir los registros en la tabla que actualizará la T16FS
          " 2) Añadir si la estrategia es reaprovechada la actualización de los textos en la tabla T16FT
          " 3) Actualizar el modelo interno para poner los nuevos valores.
          LOOP AT mt_strag_code_new_created ASSIGNING FIELD-SYMBOL(<ls_strag_code_new>).
            INSERT VALUE #( frggr = <ls_strag_code_new>-group_created
                            frgsx = <ls_strag_code_new>-strategy_created ) INTO TABLE mt_t16fs_update.

            " Añado los textos que deberá de tener la estrategía de liberación. Para los estrategias que se reutilicen se
            " sobreescribirá el que ya tenga.
            INSERT LINES OF build_text_t16ft(
              EXPORTING
                iv_group    = <ls_strag_code_new>-group_created
                iv_strategy = <ls_strag_code_new>-strategy_created
                iv_level    = <ls_strag_code_new>-level ) INTO TABLE mt_t16ft_update.

            " Los importes
            ASSIGN mt_amounts[ group = <ls_strag_code_new>-group strategy = <ls_strag_code_new>-strategy ] TO <ls_amounts>.
            IF sy-subrc = 0.
              <ls_amounts>-group = <ls_strag_code_new>-group_created.
              <ls_amounts>-strategy = <ls_strag_code_new>-strategy_created.
            ENDIF.

            " Los aprobadores
            LOOP AT mt_approvers ASSIGNING <ls_approvers> WHERE group = <ls_strag_code_new>-group
                                                                AND strategy = <ls_strag_code_new>-strategy.
              <ls_approvers>-group = <ls_strag_code_new>-group_created.
              <ls_approvers>-strategy = <ls_strag_code_new>-strategy_created.
            ENDLOOP.

          ENDLOOP.
        ENDIF.

        INSERT LINES OF lt_return_new_strag INTO TABLE et_return.
      ENDIF.


    ELSE.
      INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                            iv_id         = zif_rel_data=>cs_msg-id
                                            iv_number     = '031'
                                            iv_langu      = mv_langu ) INTO TABLE et_return.
    ENDIF.
  ENDMETHOD.





  METHOD search_free_block_strategies.


    CLEAR: et_blocks.

    " Vamos a sacar de la contaste el texto que se pone a la estrategía que se dan de baja y se puede reaprovechar.


    " El valor a partir del cual se buscará se pasa una variable para ir poder incrementado en uno
    " su valor.
    DATA(lv_next_tabix) = iv_from_tabix.
    DO.
      lv_next_tabix = lv_next_tabix + 1.
      READ TABLE mt_generator_comb_strategies ASSIGNING FIELD-SYMBOL(<ls_comb>) INDEX lv_next_tabix.
      IF sy-subrc = 0.
        " Miro si el bloque en el custo existe o si existe esta dado de baja. No filtro por grupo
        " porque la tabla ya tiene directamente un grupo.
        READ TABLE mt_t16fs ASSIGNING FIELD-SYMBOL(<ls_t16fs>)
                            WITH KEY frgsx = <ls_comb>-value.
        IF sy-subrc NE 0.
          INSERT VALUE #( strategy = <ls_comb>-value
                          reused = abap_false ) INTO TABLE et_blocks.
        ELSE.
          IF <ls_t16fs>-frgxt = mv_texto_baja.
            INSERT VALUE #( strategy = <ls_comb>-value
                            reused = abap_true ) INTO TABLE et_blocks.
          ELSE.
            " Si se quieren los bloques contiguos se sale porque ya lo voy a encontrar.
            " Si no se quiere contiguo se buscará el siguiente libre.
            IF iv_block_contiguos = abap_true.
              EXIT.
            ENDIF.
          ENDIF.
        ENDIF.

        " Si tengo todos los bloques que necesito salgo del proceso
        IF iv_size_block = lines( et_blocks ).
          EXIT.
        ENDIF.

      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

    " Si no encuentro los bloques deseados limpio la salida para no devolver bloques incompletos.
    IF iv_size_block NE lines( et_blocks ).
      CLEAR: et_blocks.

      " Si se quiere contiguos vuelvo a llamar al proceso pero empezando desde la posición donde me he quedado ¿El motivo?
      " Con un ejemplo se explica. Tenemos estos registros
      " iv_from_tabix = 79
      " lv_next_tabix: 80 - 1A - libre
      " lv_next_tabix: 81 - 1B - libre
      " lv_next_tabix: 82 - 1C - en uso
      " lv_next_tabix: 83 - 1D - libre
      " Si busco tres bloques y veo que el 1C esta ocupado. La siguiente llamada debe empezar a partir del tabix 82
      " porque si le pasa la 80 va a volver a fallar porque la linea 82 esta ocupada. Tiene sentido comenzar a partir de la
      " 82 porque el registro siguiente puede estar libre.
      " De esta manera reducimos el número de bucles.
      "
      " Si no queremos contiguos no se vuelve a llamar porque si no se ha encontrado tampoco lo va hacer volviendo a llamar.
      IF iv_block_contiguos = abap_true.
        search_free_block_strategies(
          EXPORTING
            iv_from_tabix = lv_next_tabix
            iv_size_block = iv_size_block
            iv_block_contiguos = iv_block_contiguos
          IMPORTING
            et_blocks     = et_blocks ).
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD save_custo_sap_bo.
    DATA lt_return_rfc TYPE bapiret2_t.
    DATA lt_return_prod TYPE bapiret2_t.
    DATA lt_return_int TYPE bapiret2_t.
    DATA: lv_msgtxt TYPE msgtxt.
    DATA lv_no_update_dev TYPE sap_bool.

    CLEAR: et_return.

    " La grabación del custo se hacen en varios sistemas de R/3, por ello primero obtengo las RFC de R/3 que me voy a conectar
    DATA(lt_rfc) = mo_general_md->get_rfc_system( iv_sys_type = zif_rel_data=>cs_connectivity-system_type-r3 ).

    " Leo el sistema donde estoy porque quiero sea el primero en hacer el proceso y además segun el nivel donde este se hará más o menos sistemas.
    " Es decir. Si estoy en GPI, tiene el nivel 2. Entonces haré los sistemas cuyo nivel sea < 2, que solo será GPD. Si estoy en APT, nivel 3, haré
    " nivel 2, GPI, y 1, GPD. De esta manera nunca actualizaré APT si estoy en producción.
    " Si por cualquier motivo no esta la entrada añado una manualmente y con nivel 1. Para que solo actualize el sistema donde estoy y no actualice otros
    " sistemas y la lie.

    " Nota IRB: Leo un parámetro para que no actualice el sistema de desarrollo. Es para uso de pruebas.
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'CRUD_NO_UPDATE_DEV'
                                           IMPORTING ev_value = lv_no_update_dev ).

    IF lv_no_update_dev = abap_false.
      " Primero voy actualizar el sistema de desarrollo que es donde tiene que estar si o si.
      IF sy-sysid = zif_rel_data=>cs_connectivity-r3_systems-development.
        CALL FUNCTION 'ZREL_UPDATE_SAP_CUSTO'
          EXPORTING
            it_t16fs        = mt_t16fs_update
            it_t16ft        = mt_t16ft_update
            it_t16fk        = mt_t16fk_update
            it_t16fv        = mt_t16fv_update
            it_t16fw        = mt_t16fw_update
            it_t16fd        = mt_t16fd_update
            it_t16fc        = mt_t16fc_update
            it_t16fk_delete = mt_t16fk_delete
            it_t16fv_delete = mt_t16fv_delete
            it_t16fw_delete = mt_t16fw_delete
            iv_commit       = abap_false
            iv_langu        = mv_langu
          IMPORTING
            et_return       = et_return.

      ELSE.
        TRY.
            ASSIGN lt_rfc[ sysid = zif_rel_data=>cs_connectivity-r3_systems-development ] TO FIELD-SYMBOL(<ls_rfc_develop>).
            IF sy-subrc = 0.
              CALL FUNCTION 'ZREL_UPDATE_SAP_CUSTO' DESTINATION <ls_rfc_develop>-rfc
                EXPORTING
                  it_t16fs              = mt_t16fs_update
                  it_t16ft              = mt_t16ft_update
                  it_t16fk              = mt_t16fk_update
                  it_t16fv              = mt_t16fv_update
                  it_t16fw              = mt_t16fw_update
                  it_t16fd              = mt_t16fd_update
                  it_t16fc              = mt_t16fc_update
                  it_t16fk_delete       = mt_t16fk_delete
                  it_t16fv_delete       = mt_t16fv_delete
                  it_t16fw_delete       = mt_t16fw_delete
                  iv_commit             = abap_false
                  iv_langu              = mv_langu
                IMPORTING
                  et_return             = et_return
                EXCEPTIONS
                  communication_failure = 1 MESSAGE lv_msgtxt
                  system_failure        = 2 MESSAGE lv_msgtxt
                  OTHERS                = 99.

              IF sy-subrc NE 0.
                INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                                         iv_id         = zif_rel_data=>cs_msg-id
                                                         iv_number     = '033'
                                                         iv_langu      = mv_langu
                                                         iv_message_v1 = <ls_rfc_develop>-sysid
                                                         iv_message_v2 = lv_msgtxt ) INTO TABLE et_return.
              ENDIF.
            ENDIF.
          CATCH cx_sy_itab_line_not_found.
        ENDTRY.
      ENDIF.
    ENDIF.

    " Si no hay error entonces actualizo el sistema de producción siempre y cuando estemos en producción
    IF NOT line_exists( et_return[ type = zif_rel_data=>cs_msg-type_error ] ).

      IF sy-sysid = zif_rel_data=>cs_connectivity-r3_systems-production.

        CALL FUNCTION 'ZREL_UPDATE_SAP_CUSTO'
          EXPORTING
            it_t16fs        = mt_t16fs_update
            it_t16ft        = mt_t16ft_update
            it_t16fk        = mt_t16fk_update
            it_t16fv        = mt_t16fv_update
            it_t16fw        = mt_t16fw_update
            it_t16fd        = mt_t16fd_update
            it_t16fc        = mt_t16fc_update
            it_t16fk_delete = mt_t16fk_delete
            it_t16fv_delete = mt_t16fv_delete
            it_t16fw_delete = mt_t16fw_delete
            iv_commit       = abap_false
            iv_langu        = mv_langu
          IMPORTING
            et_return       = lt_return_prod.

        INSERT LINES OF lt_return_prod INTO TABLE et_return.
      ENDIF.


      IF line_exists( et_return[ type = zif_rel_data=>cs_msg-type_error ] ).

        ROLLBACK WORK.

      ELSE.

        " Ahora actualizamos integración
        IF sy-sysid = zif_rel_data=>cs_connectivity-r3_systems-integration.
          CALL FUNCTION 'ZREL_UPDATE_SAP_CUSTO'
            EXPORTING
              it_t16fs        = mt_t16fs_update
              it_t16ft        = mt_t16ft_update
              it_t16fk        = mt_t16fk_update
              it_t16fv        = mt_t16fv_update
              it_t16fw        = mt_t16fw_update
              it_t16fd        = mt_t16fd_update
              it_t16fc        = mt_t16fc_update
              it_t16fk_delete = mt_t16fk_delete
              it_t16fv_delete = mt_t16fv_delete
              it_t16fw_delete = mt_t16fw_delete
              iv_commit       = abap_false
              iv_langu        = mv_langu
            IMPORTING
              et_return       = lt_return_int.
        ELSE.
          TRY.
              ASSIGN lt_rfc[ sysid = zif_rel_data=>cs_connectivity-r3_systems-integration ] TO FIELD-SYMBOL(<ls_rfc_integration>).
              IF sy-subrc = 0.
                CALL FUNCTION 'ZREL_UPDATE_SAP_CUSTO' DESTINATION <ls_rfc_integration>-rfc
                  EXPORTING
                    it_t16fs              = mt_t16fs_update
                    it_t16ft              = mt_t16ft_update
                    it_t16fk              = mt_t16fk_update
                    it_t16fv              = mt_t16fv_update
                    it_t16fw              = mt_t16fw_update
                    it_t16fd              = mt_t16fd_update
                    it_t16fc              = mt_t16fc_update
                    it_t16fk_delete       = mt_t16fk_delete
                    it_t16fv_delete       = mt_t16fv_delete
                    it_t16fw_delete       = mt_t16fw_delete
                    iv_commit             = abap_false
                    iv_langu              = mv_langu
                  IMPORTING
                    et_return             = lt_return_int
                  EXCEPTIONS
                    communication_failure = 1 MESSAGE lv_msgtxt
                    system_failure        = 2 MESSAGE lv_msgtxt
                    OTHERS                = 99.

                IF sy-subrc NE 0.
                  INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                                           iv_id         = zif_rel_data=>cs_msg-id
                                                           iv_number     = '033'
                                                           iv_langu      = mv_langu
                                                           iv_message_v1 = <ls_rfc_integration>-sysid
                                                           iv_message_v2 = lv_msgtxt ) INTO TABLE lt_return_int.
                ENDIF.


              ENDIF.
            CATCH cx_sy_itab_line_not_found.
          ENDTRY.

        ENDIF.
        INSERT LINES OF lt_return_int INTO TABLE et_return.

        " Si no hay errores en el sistema donde estoy actualizo los datos del BOPF
        IF NOT line_exists( et_return[ type = zif_rel_data=>cs_msg-type_error field = sy-sysid ] ).
          " Ahora lanzamos el proceso de actualizar el nodo de los importes y aprobadores.
          update_bopf_node_amounts(  ).
          update_bopf_node_approvers(  ).

          " Se graban, se hace el commit, los datos en el BOPF y de paso los datos en el customizing de SAP
          mo_bopf_helper->save_data_bopf(  ).
        ENDIF.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD get_klart_table.

    IF mv_klart_obtab IS INITIAL.
      SELECT SINGLE klart, obtab FROM tcla INTO @DATA(ls_tcla) WHERE klart = @zif_rel_data=>cs_strategy-classification-klart.
      mv_klart_obtab = ls_tcla-obtab.
    ENDIF.

    rv_table = mv_klart_obtab.

  ENDMETHOD.


  METHOD build_custo_approvers.
    DATA lv_code TYPE frgco.

    " Saco los registros que se actualizan: insertar o modificar. Los borrados no se tiene que hacer nada con ellos porque
    " los códigos de los aprobadores se mantienen.
    DATA(lt_approvers) = VALUE zrel_bo_i_strategy_approvers( FOR <wa1> IN mt_approvers WHERE ( cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-insert
                                                                                               OR cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-update )
                                                                 ( CORRESPONDING #( <wa1> ) ) ).
    IF lt_approvers IS NOT INITIAL.

      " En la mayoría de casuísticas hay que mirar si el usuario que se inserta o modifica tiene código de aprobación, por
      " ello lo primero que voy hacer es mirar que codigos de liberación que tienen, en cualquier grupo donde este.
      mo_md_query->get_liberation_code_from_user( EXPORTING it_r_users        = VALUE #( FOR <wa> IN lt_approvers ( sign = 'I' option = 'EQ' low = <wa>-username ) )
                                                  IMPORTING et_user_lib_group = mt_lib_group_of_user ).

      " Y también va ser necesario los aprobadores del grupo de liberación.
      mo_md_query->get_users_from_lib_group_code( EXPORTING iv_group = lt_approvers[ 1 ]-group
                                                  IMPORTING et_user_lib_group = mt_users_from_lib_group ).

      " Se recorre los aprobadores para ir construyendo las tablas segun el tipo de actualizacion
      LOOP AT lt_approvers ASSIGNING FIELD-SYMBOL(<ls_approvers>) .
        " Miro si el código y usuario que estoy leyendo ya lo tiene asignado el usuario en el custo. Si es así, es que lo he añadido y ahora
        " debo estar en un reproceso o es un codigo que ya esta asignado a un usuario. En ese caso no busco código porque ya lo tengo pero repetiré el proceso de actualiazción porque no sé en que
        " sistemas habrá fallado
        IF line_exists( mt_lib_group_of_user[ group = <ls_approvers>-group
                                              code = <ls_approvers>-code
                                              username = <ls_approvers>-username ] ).
          lv_code = <ls_approvers>-code.

          " Actualizo el aprobador en la tabla de custo donde se ponen los códigos.
          fill_it_upd_md_approvers(
            EXPORTING
              iv_code       = <ls_approvers>-code
              is_approver   = <ls_approvers>
              iv_fill_t16fw = abap_false
              iv_fill_t16fc = abap_false
              iv_fill_t16fs = abap_true ).

        ELSE.
          CASE <ls_approvers>-cdchngind.
            WHEN  zif_rel_data=>cs_strategy-change_request-change_indicator-insert.
              det_new_approver_code( EXPORTING is_approver = <ls_approvers>
                                     IMPORTING ev_code = lv_code
                                               et_return = DATA(lt_return_new) ).
              " Si tenemos un error se añade al parámetro de salida y se sale del proceso.
              IF lt_return_new IS NOT INITIAL.
                INSERT LINES OF lt_return_new INTO TABLE et_return.
                EXIT.
              ENDIF.
            WHEN zif_rel_data=>cs_strategy-change_request-change_indicator-update.
              det_update_approver_code( EXPORTING is_approver = <ls_approvers>
                                        IMPORTING ev_code = lv_code ).
          ENDCASE.
        ENDIF.

        IF lv_code IS NOT INITIAL.

          " Actualizo la tabla global con el código determinado
          mt_approvers[ key = <ls_approvers>-key ]-code = lv_code.

        ELSE.
          " Si no hay código es que no se ha podido determinar y por lo tanto genero un mensaje de error y salgo del proceso
          " porque no se puede completar
          INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                            iv_id         = zif_rel_data=>cs_msg-id
                                            iv_number     = '034'
                                            iv_message_v1 = <ls_approvers>-group
                                            iv_langu      = mv_langu ) INTO TABLE et_return.
          EXIT.
        ENDIF.

      ENDLOOP.

    ENDIF.

    " En la actualización o borrado hay que actualizar los estados de liberación.
    IF lt_approvers IS NOT INITIAL
       OR line_exists( mt_approvers[ cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-delete ] ).
      build_liberation_statuses(  ).
    ENDIF.
  ENDMETHOD.


  METHOD det_new_approver_code.

    CLEAR: et_return.

    " Vamos a mirar si el usuario ya tiene codigo de aprobador para el grupo de liberación que estoy
    " procesando o en otro grupo donde pueda estar el usuario.
    ev_code = get_code_user_to_group( iv_username  = is_approver-username
                                            iv_group_of_code = is_approver-group ).

    IF ev_code IS INITIAL.

      " Si no tiene usuario hay que buscarle uno. Primero priorizaremos los códigos dados de baja,
      " para ello hay que leer las descripciones de los códigos para el grupo que estamos procesando.
      " Si tengo la tabla global de textos vacia los busco los textos de los códigos de los grupos que estoy
      " tratando. Nota: Todo el conjunto de cambios siempre tienen un solo grupo de liberacion.
      IF mt_t16fd IS INITIAL.
        " Busco por todos los idiomas porque he visto que a veces el texto "BAJA" no esta en el idioma
        " de conexión, y prefiero leer todos los idioma y buscar
        mt_t16fd = mo_md_query->get_descriptions_release_code( iv_group = is_approver-group ).
      ENDIF.


      " Hago un bucle porque hay que tener en cuenta que el codigo que pueda encontrar ya lo he econtrado y procesado
      " en un aprobador anterior.
      LOOP AT mt_t16fd ASSIGNING FIELD-SYMBOL(<ls_t16fd>)
                       WHERE frggr = is_approver-group
                             AND frgct = mv_texto_baja.
        IF NOT line_exists( mt_t16fw_update[ frggr = <ls_t16fd>-frggr
                                             frgco = <ls_t16fd>-frgco ] ).
          ev_code = <ls_t16fd>-frgco.
          EXIT.
        ENDIF.
      ENDLOOP.

      " Si no encuentro codigo que pueda reaprovechar hay que crear uno.
      IF ev_code IS INITIAL.

        " Si la tabla donde están las combinaciones que se usan tanto en los grupos como aquí en los códigos esta vacia la relleno porque al final
        " el sistema de combinaciones es idéntico para ambos.
        IF mt_generator_comb_strategies IS INITIAL.
          mt_generator_comb_strategies = NEW zcl_ca_combinations_generator( iv_only_upper_case = abap_true
                                                              iv_start_comb_with = zcl_ca_combinations_generator=>cs_start_combination_with-number )->generate_combinations( iv_comb_number = 2 ).
        ENDIF.

        LOOP AT mt_generator_comb_strategies ASSIGNING FIELD-SYMBOL(<ls_generator_comb_strategies>).
          READ TABLE mt_users_from_lib_group ASSIGNING FIELD-SYMBOL(<ls_lib_group>)
                                             WITH KEY group = is_approver-group
                                                      code = <ls_generator_comb_strategies>-value.
          IF sy-subrc NE 0.
            " Ahora miso si el nuevo código no lo he añadido en otro proveedor.
            IF NOT line_exists( mt_t16fw_update[ frggr = is_approver-group
                                                 frgco = <ls_generator_comb_strategies>-value ] ).
              ev_code = <ls_generator_comb_strategies>-value.
              EXIT.
            ENDIF.
          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.


    fill_it_upd_md_approvers( EXPORTING iv_code = ev_code
                                    is_approver = is_approver
                          IMPORTING et_return = et_return ).

  ENDMETHOD.


  METHOD get_code_user_to_group.


    CLEAR: rv_code.

    " Buscamos si el usuario esta añadido en la tabla interna que va actualizar la T16FW, la que contiene
    " la relación de códigos de liberación y usuarios. Si es así, no busco porque se usará el mismo
    READ TABLE mt_t16fw_update ASSIGNING FIELD-SYMBOL(<ls_t16fw>)
                               WITH KEY frggr = iv_group_of_code
                                        objid = iv_username.
    IF sy-subrc NE 0.

      " Miramos el usuario tiene código en el grupo donde tiene que tenerlo
      READ TABLE mt_lib_group_of_user ASSIGNING FIELD-SYMBOL(<ls_lib_group_user>)
                                      WITH KEY username = iv_username
                                               group = iv_group_of_code.
      IF sy-subrc = 0.
        rv_code = <ls_lib_group_user>-code.
      ELSE.

        " Ahora vamos a mirar que codigos de usuario tiene en otros grupos para ver si se puede utilizar
        LOOP AT mt_lib_group_of_user ASSIGNING FIELD-SYMBOL(<ls_lib_other_group_user>)
                                       WHERE group NE iv_group_of_code
                                             AND username = iv_username.
          " Miro si tengo el codigo en el grupo
          READ TABLE mt_users_from_lib_group ASSIGNING FIELD-SYMBOL(<ls_code_free>)
                                             WITH KEY group = iv_group_of_code
                                                      code = <ls_lib_other_group_user>-code.
          IF sy-subrc = 0.
            " Existe, pero vamos a mirar si esta marcado como baja. Si esta como baja se puede usar el código
            IF <ls_code_free>-username_desc = mv_texto_baja.
              rv_code = <ls_code_free>-code.
            ENDIF.
          ELSE. " Si no existe lo podemos utilizar.
            rv_code = <ls_lib_other_group_user>-code.
          ENDIF.
        ENDLOOP.
      ENDIF.
    ELSE.
      rv_code = <ls_t16fw>-frgco.
    ENDIF.
  ENDMETHOD.


  METHOD reset_approvers_t16fs.

    DO.
      DATA(lv_campo) = |FRGC{ sy-index }|.

      ASSIGN COMPONENT lv_campo OF STRUCTURE cs_t16fs TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        IF <field> IS NOT INITIAL.
          CLEAR: <field>.
        ELSE.
          " Se sale porque ya no hay más aprobadores.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD build_text_t16fd.
    DATA lt_langu TYPE STANDARD TABLE OF sylangu.

    CLEAR: rt_values.

    " Saco los idioma donde tengo que poner el texto
    zcl_rel_constants=>get_constante_en_tabla( EXPORTING iv_pattern = 'LANGU_DESC_CUSTO_%'
                                               CHANGING ct_table   = lt_langu ).
    IF lt_langu IS INITIAL. " Si no idioma, que no debería de ocurrir, relleno los idiomas por defecto
      INSERT 'S' INTO TABLE lt_langu.
      INSERT 'E' INTO TABLE lt_langu.
      INSERT 'D' INTO TABLE lt_langu.
      INSERT 'F' INTO TABLE lt_langu.
    ENDIF.

    rt_values = VALUE #( FOR <wa> IN lt_langu ( frggr = iv_group
                                    frgco = iv_code
                                    spras = <wa>
                                    frgct = iv_text )  ).
  ENDMETHOD.


  METHOD fill_it_upd_md_approvers.

    " Añadimos la registro en la tabla de actualización T16FW que contiene el grupo, codigo y usuario si se ha indicado por parámetro2.
    " Siempre y cuando no tengo la línea insertada previamente. Los duplicados no afectan al proceso pero no queda limpio.
    IF iv_fill_t16fw = abap_true
       AND NOT line_exists( mt_t16fw_update[ frggr = is_approver-group
                            frgco = iv_code
                            otype = zif_rel_data=>cs_strategy-master_data-otype_usersap
                            objid = is_approver-username ]  ).


      INSERT VALUE #( frggr = is_approver-group
                      frgco = iv_code
                      otype = zif_rel_data=>cs_strategy-master_data-otype_usersap
                      objid = is_approver-username ) INTO TABLE mt_t16fw_update.

      " Relleno los textos del código de liberación. Aquí he optado por una solución salomómica y machaco el texto que pueda tener, tanto
      " si se reaprovecha como si no. El motivo, es que a lo mejor el texto no es correcto, no esta en todos los idiomas, etc.. actualizo siempre
      " y me aseguro que estará bien en todos los idiomas.
      INSERT LINES OF build_text_t16fd( EXPORTING iv_group = is_approver-group
                                                 iv_code = iv_code
                                                 iv_text = |{ is_approver-username_desc CASE = UPPER }| ) INTO TABLE mt_t16fd_update.

    ENDIF.

    " Ahora la tabla de codigos de liberación y su función de workflow si se ha indicado por parámetro.
    " Aquí de nuevo evito los duplicados.
    IF iv_fill_t16fc = abap_true
       AND NOT line_exists( mt_t16fc_update[ frggr = is_approver-group
                                             frgco = iv_code
                                             frgwf = zif_rel_data=>cs_strategy-master_data-workflow_function ] )   .

      INSERT VALUE #( frggr = is_approver-group
                      frgco = iv_code
                      frgwf = zif_rel_data=>cs_strategy-master_data-workflow_function ) INTO TABLE mt_t16fc_update.
    ENDIF.

    " Ahora ponemos el código en el nivel de la tabla T16FS si se ha indicado por parámetro.
    IF iv_fill_t16fs = abap_true.
      ASSIGN mt_t16fs_update[ frggr = is_approver-group
                              frgsx = is_approver-strategy ] TO FIELD-SYMBOL(<ls_t16fs_update>).
      IF sy-subrc = 0.
        DATA(lv_campo) = |FRGC{ is_approver-level }|.
        ASSIGN COMPONENT lv_campo OF STRUCTURE <ls_t16fs_update> TO FIELD-SYMBOL(<field>).
        IF sy-subrc = 0.
          <field> = iv_code.
        ENDIF.
      ELSE.
        " Tal como esta el proceso de strategias en la tabla interna T16FS debe estar llena en este paso
        " así que si esta vacia en este paso doy error.
        INSERT zcl_ca_utilities=>fill_return( iv_type       = zif_rel_data=>cs_msg-type_error
                                                iv_id         = zif_rel_data=>cs_msg-id
                                                iv_number     = '035'
                                                iv_langu      = mv_langu ) INTO TABLE et_return.

      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD det_update_approver_code.

    CLEAR: ev_code.

    " Segun el motivo del cambio el proceso puede variar.
    CASE is_approver-type_change.
      WHEN zif_rel_data=>cs_strategy-change_request-type_change_approver-cancelation.
        " Mirarmos si el usuario tiene código en el grupo actual o en otro grupo
        ev_code = get_code_user_to_group( iv_username  = is_approver-username
                                                iv_group_of_code = is_approver-group ).
        " El usuario no tiene código de aprobación por lo tanto aprovecharemos el código existente para asignarlo
        " al usuario.
        IF ev_code IS INITIAL.
          " Como el código es el mismo que ya habia solo hay que actualiza la tabla donde esta el codigo y usuario, y la tabla
          " con la denominación del código
          fill_it_upd_md_approvers(
            EXPORTING
              iv_code       = is_approver-code
              is_approver   = is_approver
              iv_fill_t16fw = abap_true
              iv_fill_t16fc = abap_false
              iv_fill_t16fs = abap_false ).

          " Devuelve el mismo código que el que ya tiene
          ev_code = is_approver-code.
        ELSE.

          " Si el código obtenido es el mismo que el que ya tenía entonces se ha producido una incongruencia en la selección.
          " Incongruencia porque no tiene sentido dar de baja un codigo que lo vuelves a asignar.
          " En ese caso no haré nada, se queda todo como est.a
          IF ev_code NE is_approver-code.

            " Si ya existe vamos a dar de baja el usuario actual del código en todos los sitios donde este. Se le pasa
            " el usuario que esta actualmente en la foto de SAP ya que es ese el usuario que hay darlo de baja.
            fill_del_user_groups( EXPORTING is_old_approver = mt_approvers_sap[ group = is_approver-group
                                                                                code = is_approver-code ] ).

            " Hago el reemplazo del usuario que ya habia antes por el nuevo.
            DATA(ls_new_approver) = is_approver.
            ls_new_approver-code = ev_code.
            fill_repl_user_groups( EXPORTING is_old_approver = CORRESPONDING #( mt_approvers_sap[ group = is_approver-group
                                                                                                  code = is_approver-code ] )
                                             is_new_approver = ls_new_approver ).

          ENDIF.

        ENDIF.
      WHEN zif_rel_data=>cs_strategy-change_request-type_change_approver-change_functions.
        " El cambio de funciones implica que el usuario actual sigue conservando el código. Y para el nuevo
        " usuario se realiza el mismo proceso que si fuera una nueva alta.
        det_new_approver_code( EXPORTING is_approver = is_approver
                               IMPORTING ev_code     = ev_code ).

    ENDCASE.


  ENDMETHOD.

  METHOD fill_it_dele_md_approvers.


    " Si hay usuario se borrará en la tabla T16FW.
    IF is_approver-username IS NOT INITIAL.
      INSERT VALUE #( frggr = is_approver-group
                    frgco = is_approver-code
                    otype = zif_rel_data=>cs_strategy-master_data-otype_usersap
                    objid = is_approver-username ) INTO TABLE mt_t16fw_delete.
    ENDIF.

    INSERT LINES OF build_text_t16fd( EXPORTING iv_group = is_approver-group
                                               iv_code = is_approver-code
                                               iv_text = CONV #( mv_texto_baja ) ) INTO TABLE mt_t16fd_update.

  ENDMETHOD.


  METHOD fill_del_user_groups.

    " Antes de borrar se mira si el usuario que voy a borrar esta en un proceso de actualización. Si es así, no lo voy a borrar
    " porque luego se va a tener que usar su codigo. Esto tampoco tiene mucho sentido pero hemos visto que hay equivocaciones
    " en el cambio de usuarios y en vez de seleccionar cambio de funcionalidad seleccionan la baja, pero en un nivel siguente o anterior ponen
    " un cambio de funcionalidad.
    LOOP AT mt_approvers TRANSPORTING NO FIELDS WHERE username =  is_old_approver-username
                                                      AND cdchngind NE ''.
      EXIT.
    ENDLOOP.
    IF sy-subrc NE 0.

      " Si el codigo tiene usuario se va a eliminar de todos los códigos donde esta asignado, y el código se le pondrá el texto de BAJA
      IF is_old_approver-username IS NOT INITIAL.
        " Sacamos en todos los sitios donde esta el usuario
        mo_md_query->get_liberation_code_from_user( EXPORTING it_r_users        = VALUE #( ( sign = 'I' option = 'EQ' low = is_old_approver-username ) )
                                                    IMPORTING et_user_lib_group = DATA(lt_user_codes) ).

        " Por cada sitio donde esta el usuario vamos llamando al método que alimenta las tablas internas para realizar el borrado.
        LOOP AT lt_user_codes ASSIGNING FIELD-SYMBOL(<ls_user_codes>).

          " Si para el grupo y codigo esta dentro de la tabla de actualización de codigos no lo borro porque ese codigo se reaprovechará para otro usuario.
          IF NOT line_exists( mt_t16fw_update[ frggr = <ls_user_codes>-group frgco = <ls_user_codes>-code  ] ).

            fill_it_dele_md_approvers( is_approver = VALUE #( group = <ls_user_codes>-group
                                                              code = <ls_user_codes>-code
                                                              username = <ls_user_codes>-username )  ).

          ENDIF.

        ENDLOOP.

      ELSE.

        " Si el código no tiene usuario solo se pone el código de liberación como baja.
        fill_it_dele_md_approvers( is_approver = VALUE #( group = is_old_approver-group
                                                          code = is_old_approver-code )  ).

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD fill_repl_user_groups.

    " Primero vamos actualizar el grupo que se esta tratando.
    fill_it_upd_md_approvers(
                   EXPORTING
                     iv_code       = is_new_approver-code
                     is_approver   = is_new_approver
                     iv_fill_t16fw = abap_true
                     iv_fill_t16fc = abap_true
                     iv_fill_t16fs = abap_true ).

    " Si el código de liberación tiene usuario lo que se hace es poner el nuevo código en los grupos de liberación donde
    " estaba al antiguo.
    IF is_old_approver-username IS NOT INITIAL.

      mo_md_query->get_liberation_code_from_user( EXPORTING it_r_users        = VALUE #( ( sign = 'I' option = 'EQ' low = is_old_approver-username ) )
                                                  IMPORTING et_user_lib_group = DATA(lt_user_codes) ).

      IF lt_user_codes IS NOT INITIAL.

        mo_md_query->get_approv_strat_from_group( EXPORTING it_group_code = CORRESPONDING #( lt_user_codes )
                                                            iv_filter_lib_code = abap_true
                                                  IMPORTING et_approvers = DATA(lt_user_strategy) ).

        LOOP AT lt_user_strategy ASSIGNING FIELD-SYMBOL(<ls_user_codes>).

          " Si para el grupo y codigo esta dentro de la tabla de actualización de codigos no lo reemplazo porque ya se va hacer.
          IF NOT line_exists( mt_t16fw_update[ frggr = <ls_user_codes>-group frgco = <ls_user_codes>-code  ] ).

            fill_it_upd_md_approvers(
                 EXPORTING
                   iv_code       = is_new_approver-code
                   is_approver   = VALUE #( group = <ls_user_codes>-group
                                            code = is_new_approver-code
                                            strategy = <ls_user_codes>-strategy
                                            username = is_new_approver-username
                                            username_desc = is_new_approver-username_desc )
                   iv_fill_t16fw = abap_true
                   iv_fill_t16fc = abap_true
                   iv_fill_t16fs = abap_true ).

          ENDIF.

        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD build_liberation_statuses.

    " Vamos a tomar como base para construir los estados la tabla MT_16FS_UPDATE que es como lo hace SAP, ya que en esta tabla ya esta
    " el orden de los códigos con su respectiva estrategia.
    LOOP AT mt_t16fs_update ASSIGNING FIELD-SYMBOL(<ls_t16fs_dummy>)
                            GROUP BY (  frggr = <ls_t16fs_dummy>-frggr
                                        frgsx = <ls_t16fs_dummy>-frgsx )
                            ASSIGNING FIELD-SYMBOL(<group>).

      " Sacamos los de las tablas de estados de liberación para el grupo y estrategia que estoy
      " procesando.
      mo_md_query->get_liberation_statuses( EXPORTING iv_group = <group>-frggr
                                                      iv_strategy   = <group>-frgsx
                                            IMPORTING et_t16fk      = DATA(lt_t16fk)
                                                      et_t16fv      = DATA(lt_t16fv) ).

      " Si tengo datos los añado a la tabla de borrado. El motivo es que el código de liberación es clave en ambas tablas y por lo tanto
      " me es mucho más sencillo borrarlo y crearlo de nuevo, que mirar que cambia y que no. Porque al final casi el 100% de casos habrá que borrar algun
      " que otro registro.
      INSERT LINES OF lt_t16fk INTO TABLE mt_t16fk_delete.
      INSERT LINES OF lt_t16fv INTO TABLE mt_t16fv_delete.

      " La tabla interna de estados de liberación se le añade una entrada solo con el grupo y estrategia y con estado bloqueado
      LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<ls_t16fs>).

        " Paso la estructura en tabla interna que me servirá para alimentar las tablas de estados
        DATA(lt_t16fs_column) = mo_md_query->convert_t16fs_row_data( is_t16fs = <ls_t16fs> ).

        IF lt_t16fs_column IS NOT INITIAL. " Cuando se borra no habrá codigos de liberación

          DATA(lv_num_approvers) = lines( lt_t16fs_column ).

          " Los estados de liberación se informan en dos tablas: T16FV y T16FK.

          " Para el estado de liberación tiene que tener un primer registro en blanco con todos los códigos desmarcados
          " y bloqueados.
          INSERT VALUE #( frggr = <ls_t16fs>-frggr
                          frgsx = <ls_t16fs>-frgsx
                          frgkx = zif_rel_data=>cs_strategy-master_data-release_indicator-blocked ) INTO TABLE mt_t16fk_update.



          LOOP AT lt_t16fs_column ASSIGNING FIELD-SYMBOL(<ls_t16fs_column>).
            DATA(lv_tabix) = sy-tabix.

            " Añdimos el registro del requisito de liberación que es  donde se le indica el nivel de los codigos. Por ello vamos a recorrer cada uno de los
            " códigos para colocarlo y marcarlos.
            " Añadimos el código y lo asignamos a un puntero.
            INSERT VALUE #(  frggr = <ls_t16fs_column>-group
                             frgsx = <ls_t16fs_column>-strategy
                             frgco = <ls_t16fs_column>-code )
                          INTO TABLE mt_t16fv_update
                          ASSIGNING FIELD-SYMBOL(<ls_t16fv>).

            "Añdimos el reguistro de los estados. Este registro se rellena como si fuera un rectangulo, se van marcados cada FRGAx con X
            " según su nivel.
            INSERT VALUE #( frggr = <ls_t16fs>-frggr
               frgsx = <ls_t16fs>-frgsx
               frgkx = COND #( WHEN lv_tabix = lv_num_approvers THEN zif_rel_data=>cs_strategy-master_data-release_indicator-released
                                                                ELSE zif_rel_data=>cs_strategy-master_data-release_indicator-blocked ) )
               INTO TABLE mt_t16fk_update
               ASSIGNING FIELD-SYMBOL(<ls_t16fk>).

            DO <ls_t16fs_column>-level TIMES.
              " El campo común para ambas tablas-
              DATA(lv_campo) = |FRGA{ sy-index }|.

              " Los campos "FRGAx" de los requisitos se rellenan de la siguiente manera:
              " Se pone una 'X' cuando en el nivel de aprobación donde esta situado el código.
              " Se pone un '+' cuando el nivel es inferior al del código
              ASSIGN COMPONENT lv_campo OF STRUCTURE <ls_t16fv> TO FIELD-SYMBOL(<field>).
              IF sy-subrc = 0.
                IF sy-index = <ls_t16fs_column>-level.
                  <field> = zif_rel_data=>cs_strategy-master_data-release_strag_indicator-fixed.
                ELSE.
                  <field> = zif_rel_data=>cs_strategy-master_data-release_strag_indicator-required.
                ENDIF.
              ENDIF.

              ASSIGN COMPONENT lv_campo OF STRUCTURE <ls_t16fk> TO <field>.
              IF sy-subrc = 0.
                <field> = zif_rel_data=>cs_strategy-master_data-release_strag_indicator-fixed.
              ENDIF.
            ENDDO.

          ENDLOOP.
        ENDIF.
      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD delete_approvers_t16fs.

    DATA(lv_index_main) = 1.
    DO.

      DATA(lv_campo) = |FRGC{ lv_index_main }|.

      ASSIGN COMPONENT lv_campo OF STRUCTURE cs_t16fs TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        IF <field> IS NOT INITIAL.

          " Si el código de aprobación esta marcado para probar se quita el codigo y se desplazan los aprobadores
          IF line_exists( mt_approvers[ group = cs_t16fs-frggr
                                        strategy = cs_t16fs-frgsx
                                        code = <field>
                                        cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-delete ] ).

            CLEAR: <field>. " Lo limpio antes si el proceso no lo puedo machacar

            DATA(lv_index) = sy-index.
            DATA(lv_move_code) = abap_false.
            DO.

              lv_campo = |FRGC{ lv_index }|.
              ASSIGN COMPONENT lv_campo OF STRUCTURE cs_t16fs TO <field>.

              DATA(lv_index_next) = lv_index + 1.
              DATA(lv_campo_next) = |FRGC{ lv_index_next }|.

              ASSIGN COMPONENT lv_campo_next OF STRUCTURE cs_t16fs TO FIELD-SYMBOL(<field_next>).
              IF sy-subrc = 0.
                IF <field_next> IS NOT INITIAL.
                  <field> = <field_next>.

                  " Borro el siguiente por si en la siguiente iteracción no puede sobreescribirse por ser el ultimo.
                  CLEAR: <field_next>.

                  lv_move_code = abap_true.

                ELSE.
                  EXIT.
                ENDIF.

              ELSE.
                EXIT.
              ENDIF.
              lv_index = lv_index + 1.
            ENDDO.

          ENDIF.

        ELSE.
          EXIT.
        ENDIF.
        " Solo si no hemos movido codigos incrementaremos uno el valor. Si se ha movido nos quedamos
        " donde estamos para verificar que el codigo que se ha movido a esta posición esta también borrado.
        IF lv_move_code = abap_false.
          lv_index_main = lv_index_main + 1.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.


  METHOD delete_amount.

    DATA: lt_allocvaluesnum  TYPE STANDARD TABLE OF bapi1003_alloc_values_num WITH EMPTY KEY.
    DATA: lt_allocvalueschar TYPE STANDARD TABLE OF bapi1003_alloc_values_char WITH EMPTY KEY.
    DATA: lt_allocvaluescurr TYPE STANDARD TABLE OF bapi1003_alloc_values_curr WITH EMPTY KEY.
    DATA: lt_return TYPE STANDARD TABLE OF bapiret2 WITH EMPTY KEY.
    DATA: lv_status        TYPE clstatus.
    DATA: lv_standardclass TYPE stdclass.
    DATA lt_bsart TYPE STANDARD TABLE OF atwrt.

    CLEAR: et_return.

    " Primero hemos de sacar los datos actuales que tiene el sistema de clasificación
    " para el código y grupo de liberación

    " Sacamos la tabla
    DATA(lv_obtab) = get_klart_table( ).

    " Antes de borrar vamos a asegurarnos que existe
    DATA(lv_objectkey) = CONV objnum( |{ is_amount-group }{ is_amount-strategy }| ).
    CALL FUNCTION 'BAPI_OBJCL_GETDETAIL'
      EXPORTING
        objectkey       = lv_objectkey
        objecttable     = lv_obtab
        classnum        = zif_rel_data=>cs_strategy-classification-klasse
        classtype       = zif_rel_data=>cs_strategy-classification-klart
      IMPORTING
        status          = lv_status
        standardclass   = lv_standardclass
      TABLES
        allocvaluesnum  = lt_allocvaluesnum[]
        allocvalueschar = lt_allocvalueschar[]
        allocvaluescurr = lt_allocvaluescurr[]
        return          = lt_return[].

    READ TABLE lt_return ASSIGNING FIELD-SYMBOL(<ls_return_bapi>) WITH KEY type = zif_rel_data=>cs_msg-type_error.
    IF sy-subrc NE 0.
      CLEAR: lt_return.

      CALL FUNCTION 'BAPI_OBJCL_DELETE'
        EXPORTING
          objectkey   = lv_objectkey
          objecttable = lv_obtab
          classnum    = zif_rel_data=>cs_strategy-classification-klasse
          classtype   = zif_rel_data=>cs_strategy-classification-klart
        TABLES
          return      = lt_return.

      READ TABLE lt_return ASSIGNING <ls_return_bapi> WITH KEY type = zif_rel_data=>cs_msg-type_error.
      IF sy-subrc NE 0.
        INSERT zcl_ca_utilities=>fill_return( iv_id = zif_rel_data=>cs_msg-id
                                                iv_type = zif_rel_data=>cs_msg-type_success
                                                iv_number = '039'
                                                iv_message_v1 = is_amount-group
                                                iv_message_v2 = is_amount-strategy ) INTO TABLE et_return.
      ELSE.
        INSERT <ls_return_bapi> INTO TABLE et_return.
      ENDIF.
    ELSE.
      INSERT <ls_return_bapi> INTO TABLE et_return.
    ENDIF.

  ENDMETHOD.


  METHOD read_constants.
    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'LANGU_TEXTO_BAJA'
                                                    IMPORTING ev_value    = mv_langu_texto_baja ).

    zcl_rel_constants=>obtener_constantes( EXPORTING iv_constant = 'CUSTO_TEXTO_BAJA'
                                               IMPORTING ev_value    = mv_texto_baja ).

  ENDMETHOD.

ENDCLASS.
