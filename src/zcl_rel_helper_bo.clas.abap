CLASS zcl_rel_helper_bo DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING
        iv_langu TYPE sylangu DEFAULT sy-langu.
    "! <p class="shorttext synchronized">Obtiene los datos de cabecera a partir de los campos claves</p>
    "! @parameter iv_key | <p class="shorttext synchronized">Clave del registro</p>
    "! @parameter iv_request_id | <p class="shorttext synchronized">Acción</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS get_header_from_key_fields
      IMPORTING
                iv_key           TYPE zrel_bo_sc_strategy_header-key OPTIONAL
                iv_request_id    TYPE zrel_bo_sc_strategy_header-request_id OPTIONAL
      RETURNING VALUE(rt_header) TYPE zrel_bo_i_strategy_header.
    "! <p class="shorttext synchronized">Obtención de datos a partir del nodo cabecera</p>
    "! @parameter it_params | <p class="shorttext synchronized">Parametros de selección</p>
    "! @parameter et_header | <p class="shorttext synchronized">Cabecera</p>
    "! @parameter et_buyers | <p class="shorttext synchronized">Compradores</p>
    "! @parameter et_buyers_sap | <p class="shorttext synchronized">Compradores SAP</p>
    "! @parameter et_amount | <p class="shorttext synchronized">Importes (o estrategias)</p>
    "! @parameter et_amount_sap | <p class="shorttext synchronized">Importes (o estrategias) en SAP</p>
    "! @parameter et_pgroup | <p class="shorttext synchronized">Datos del grupo de compras</p>
    "! @parameter et_pgroup_sap | <p class="shorttext synchronized">Datos del grupo de compras en SAP</p>
    "! @parameter et_approvers | <p class="shorttext synchronized">Aprobadores</p>
    "! @parameter et_approvers_sap | <p class="shorttext synchronized">Aprobadores en SAP</p>
    "! @parameter et_md_change_status | <p class="shorttext synchronized">Status de modificación de los datos maestros</p>
    "! @parameter et_steps_approvers | <p class="shorttext synchronized">Pasos de la aprobación</p>
    METHODS query_all_data_from_header
      IMPORTING
        it_params           TYPE /bobf/t_frw_query_selparam
      EXPORTING
        et_header           TYPE zrel_bo_i_strategy_header
        et_buyers           TYPE zrel_bo_i_strategy_buyers
        et_buyers_sap       TYPE zrel_bo_i_strategy_buyers_sap
        et_amounts          TYPE zrel_bo_i_strategy_amount
        et_amounts_sap      TYPE zrel_bo_i_strategy_amount_sap
        et_pgroup           TYPE zrel_bo_i_strategy_pgroup
        et_pgroup_sap       TYPE zrel_bo_i_strategy_pgroup_sap
        et_approvers        TYPE zrel_bo_i_strategy_approvers
        et_approvers_sap    TYPE zrel_bo_i_strategy_apprv_sap
        et_md_change_status TYPE zrel_bo_i_strategy_md_chng_st
        et_steps_approvers  TYPE zrel_bo_i_strategy_step_aprov.
    "! <p class="shorttext synchronized">Grabación de datos en el BOPF</p>
    "! @parameter it_mod | <p class="shorttext synchronized">Datos para el BOPF</p>
    "! @parameter ev_error_save | <p class="shorttext synchronized">Error al grabar</p>
    METHODS save_data_bopf
      IMPORTING
        it_mod        TYPE /bobf/t_frw_modification OPTIONAL
      EXPORTING
        ev_error_save TYPE sap_bool.
    "! <p class="shorttext synchronized">Grabación de datos en el BOPF</p>
    "! @parameter it_mod | <p class="shorttext synchronized">Datos para el BOPF</p>
    "! @parameter ev_error_modify | <p class="shorttext synchronized">Error al modificar</p>
    METHODS modify_data_bopf
      IMPORTING
        it_mod          TYPE /bobf/t_frw_modification
      EXPORTING
        ev_error_modify TYPE sap_bool.
  PROTECTED SECTION.
    DATA mv_langu TYPE sylangu.
    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_conf_mngr TYPE REF TO /bobf/if_frw_configuration.
    DATA mo_bopf_util TYPE REF TO zcl_ca_bopf_util.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_helper_bo IMPLEMENTATION.
  METHOD constructor.
    mv_langu = sy-langu.

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

      CATCH /bobf/cx_frw.
        "TODO: Error handling...
    ENDTRY.

  ENDMETHOD.

  METHOD get_header_from_key_fields.
    DATA lt_params TYPE /bobf/t_frw_query_selparam.
    CLEAR: rt_header.

    IF iv_key IS NOT INITIAL.

      lt_params = VALUE /bobf/t_frw_query_selparam( ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-key
                                                            sign = 'I'
                                                            option = 'EQ'
                                                            low = iv_key ) ).

    ELSE.
      lt_params = VALUE /bobf/t_frw_query_selparam( ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-request_id
                                                            sign = 'I'
                                                            option = 'EQ'
                                                            low = iv_request_id ) ).

    ENDIF.
    mo_svc_mngr->query( EXPORTING iv_query_key = zif_rel_bo_strategy_c=>sc_query-root-select_by_elements
                                    it_selection_parameters = lt_params
                                    iv_fill_data = abap_true
                        IMPORTING et_data = rt_header ).
  ENDMETHOD.



  METHOD query_all_data_from_header.
    CLEAR: et_header, et_amounts, et_approvers, et_buyers, et_buyers_sap, et_pgroup.
    CLEAR: et_amounts_sap, et_approvers_sap, et_pgroup_sap, et_md_change_status.
    CLEAR: et_steps_approvers.

    mo_svc_mngr->query( EXPORTING iv_query_key = zif_rel_bo_strategy_c=>sc_query-root-select_by_elements
                                    it_selection_parameters = it_params
                                    iv_fill_data = abap_true
                        IMPORTING et_data = et_header
                                  et_key = DATA(lt_header_keys) ).

    IF et_header IS NOT INITIAL.

      " Se recuperan los datos de los nodos del BOPF si se han solicitado por parámetro

      IF et_amounts IS SUPPLIED.
        mo_svc_mngr->retrieve_by_association(
            EXPORTING
              iv_node_key                 = zif_rel_bo_strategy_c=>sc_node-root
              it_key                  = lt_header_keys
              iv_association          = zif_rel_bo_strategy_c=>sc_association-root-amount
              iv_fill_data            = abap_true
            IMPORTING
               et_data                 = et_amounts ).
      ENDIF.

      IF et_amounts_sap IS SUPPLIED.
        mo_svc_mngr->retrieve_by_association(
            EXPORTING
              iv_node_key                 = zif_rel_bo_strategy_c=>sc_node-root
              it_key                  = lt_header_keys
              iv_association          = zif_rel_bo_strategy_c=>sc_association-root-amount_sap
              iv_fill_data            = abap_true
            IMPORTING
               et_data                 = et_amounts_sap ).
      ENDIF.

      IF et_approvers IS SUPPLIED.
        mo_svc_mngr->retrieve_by_association(
            EXPORTING
              iv_node_key                 = zif_rel_bo_strategy_c=>sc_node-root
              it_key                  = lt_header_keys
              iv_association          = zif_rel_bo_strategy_c=>sc_association-root-approvers
              iv_fill_data            = abap_true
            IMPORTING
               et_data                 = et_approvers ).
      ENDIF.

      IF et_approvers_sap IS SUPPLIED.
        mo_svc_mngr->retrieve_by_association(
            EXPORTING
              iv_node_key                 = zif_rel_bo_strategy_c=>sc_node-root
              it_key                  = lt_header_keys
              iv_association          = zif_rel_bo_strategy_c=>sc_association-root-approvers_sap
              iv_fill_data            = abap_true
            IMPORTING
               et_data                 = et_approvers_sap ).
      ENDIF.

      IF et_buyers IS SUPPLIED.
        mo_svc_mngr->retrieve_by_association(
            EXPORTING
              iv_node_key                 = zif_rel_bo_strategy_c=>sc_node-root
              it_key                  = lt_header_keys
              iv_association          = zif_rel_bo_strategy_c=>sc_association-root-buyers
              iv_fill_data            = abap_true
            IMPORTING
               et_data                 = et_buyers ).
      ENDIF.

      IF et_buyers_sap IS SUPPLIED.
        mo_svc_mngr->retrieve_by_association(
            EXPORTING
              iv_node_key                 = zif_rel_bo_strategy_c=>sc_node-root
              it_key                  = lt_header_keys
              iv_association          = zif_rel_bo_strategy_c=>sc_association-root-buyers_sap
              iv_fill_data            = abap_true
            IMPORTING
               et_data                 = et_buyers_sap ).
      ENDIF.

      IF et_pgroup IS SUPPLIED.
        mo_svc_mngr->retrieve_by_association(
              EXPORTING
                iv_node_key                 = zif_rel_bo_strategy_c=>sc_node-root
                it_key                  = lt_header_keys
                iv_association          = zif_rel_bo_strategy_c=>sc_association-root-pgroup
                iv_fill_data            = abap_true
              IMPORTING
                 et_data                 = et_pgroup ).
      ENDIF.

      IF et_pgroup_sap IS SUPPLIED.
        mo_svc_mngr->retrieve_by_association(
              EXPORTING
                iv_node_key                 = zif_rel_bo_strategy_c=>sc_node-root
                it_key                  = lt_header_keys
                iv_association          = zif_rel_bo_strategy_c=>sc_association-root-pgroup_sap
                iv_fill_data            = abap_true
              IMPORTING
                 et_data                 = et_pgroup_sap ).
      ENDIF.

      IF et_md_change_status IS SUPPLIED.
        mo_svc_mngr->retrieve_by_association(
              EXPORTING
                iv_node_key                 = zif_rel_bo_strategy_c=>sc_node-root
                it_key                  = lt_header_keys
                iv_association          = zif_rel_bo_strategy_c=>sc_association-root-md_change_status
                iv_fill_data            = abap_true
              IMPORTING
                 et_data                 = et_md_change_status ).
      ENDIF.

      IF et_steps_approvers IS SUPPLIED.
        mo_svc_mngr->retrieve_by_association(
              EXPORTING
                iv_node_key                 = zif_rel_bo_strategy_c=>sc_node-root
                it_key                  = lt_header_keys
                iv_association          = zif_rel_bo_strategy_c=>sc_association-root-step_approvers
                iv_fill_data            = abap_true
              IMPORTING
                 et_data                 = et_steps_approvers ).
      ENDIF.

    ENDIF.
  ENDMETHOD.

  METHOD save_data_bopf.
    CLEAR: ev_error_save.

    IF it_mod IS NOT INITIAL.
      " Se envian los datos al BOPF y se graba.
      mo_bopf_util->modify_save_data(
        EXPORTING
          it_mod      = it_mod
          iv_langu    = mv_langu
        IMPORTING
          ev_rejected = ev_error_save ).
    ELSE.
      " Si no hay datos solo se graba porque ya deben de estar en el cache del BOPF
      mo_bopf_util->save_data(
        EXPORTING
          iv_langu    = mv_langu
        IMPORTING
          ev_rejected = ev_error_save ).

    ENDIF.
  ENDMETHOD.

  METHOD modify_data_bopf.
    ev_error_modify = abap_false.

    " Se envian los datos al BOPF
    mo_bopf_util->modify_data(
      EXPORTING
        it_mod      = it_mod
        iv_langu    = mv_langu
      IMPORTING
        et_return = DATA(lt_return) ).

    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = zif_rel_data=>cs_msg-type_error.
    IF sy-subrc = 0.
      ev_error_modify = abap_true.
    ENDIF.
  ENDMETHOD.

ENDCLASS.
