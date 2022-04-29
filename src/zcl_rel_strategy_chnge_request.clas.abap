CLASS zcl_rel_strategy_chnge_request DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.
    TYPES: BEGIN OF ts_rel_external_internal_id,
             internal_id TYPE /bobf/conf_key,
             external_id TYPE zrel_e_request_id,
           END OF ts_rel_external_internal_id.
    TYPES: tt_rel_external_internal_id TYPE STANDARD TABLE OF ts_rel_external_internal_id WITH EMPTY KEY.
    TYPES: BEGIN OF ts_strategies_request_data.
        INCLUDE TYPE zrel_bo_sc_strategy_amount.
    TYPES:
      approvers TYPE zrel_bo_i_strategy_approvers,
      END OF ts_strategies_request_data.
    TYPES: tt_strategies_request_data TYPE STANDARD TABLE OF ts_strategies_request_data WITH EMPTY KEY.
    " La estructura que se devuelve con los datos de la petición es la misma cuando viene
    " una nueva solicitud. Es decir, los aprobador cuelgan de la estrategia.
    TYPES: BEGIN OF ts_pgroup_request_data.
        INCLUDE TYPE zrel_bo_sc_strategy_header.
    TYPES:
      buyers     TYPE zrel_bo_i_strategy_buyers,
*      buyers_sap TYPE zrel_bo_i_strategy_buyers_sap,
      strategies TYPE tt_strategies_request_data,
      pgroup     TYPE zrel_bo_sc_strategy_pgroup,
      END OF ts_pgroup_request_data.
    TYPES: tt_pgroup_request_data TYPE STANDARD TABLE OF ts_pgroup_request_data WITH EMPTY KEY.
    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING
        iv_langu TYPE sylangu DEFAULT sy-langu.
    "! <p class="shorttext synchronized">Nueva solicitud de cambio</p>
    "! @parameter is_request_data | <p class="shorttext synchronized">Datos</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    "! @parameter ev_request_id | <p class="shorttext synchronized">ID de petición</p>
    METHODS new_request
      IMPORTING
        is_request_data TYPE zif_rel_data=>ts_pgroup_request_change
      EXPORTING
        et_return       TYPE bapiret2_t
        ev_request_id   TYPE zrel_e_request_id.
    "! <p class="shorttext synchronized">Devuelve los ID externos a partir de los ID interno</p>
    "! Resumiendo, de la clave del BOPF devuelve el valor del rango de numeros
    "! @parameter it_internal_id | <p class="shorttext synchronized">Tabla claves del BOPF</p>
    "! @parameter et_external_id | <p class="shorttext synchronized">Tabla con el ID externo</p>
    METHODS get_interal_id_from_external
      IMPORTING
        it_internal_id TYPE /bobf/t_frw_key
      EXPORTING
        et_external_id TYPE tt_rel_external_internal_id..

    "! <p class="shorttext synchronized">Devuelve los datos de la petición por grupo de compras</p>
    "! También se puede filtrar por status pero es opcional
    "! @parameter it_r_pgroup | <p class="shorttext synchronized">Rango de grupos de compras</p>
    "! @parameter it_r_status | <p class="shorttext synchronized">Rango de status de la petición</p>
    "! @parameter et_request_data | <p class="shorttext synchronized">Solicitud de datos</p>
    METHODS get_pgroup_request_data
      IMPORTING it_r_pgroup     TYPE zif_rel_data=>tt_r_purchase_group
                it_r_status     TYPE zif_rel_data=>tt_r_request_status OPTIONAL
      EXPORTING et_request_data TYPE tt_pgroup_request_data.
    "! <p class="shorttext synchronized">Aprobación de solicitud</p>
    "! @parameter iv_request_id | <p class="shorttext synchronized">ID de solicitud</p>
    "! @parameter iv_action | <p class="shorttext synchronized">Acción</p>
    "! @parameter iv_reason | <p class="shorttext synchronized">Motivo de la aprobación</p>
    "! @parameter iv_department_approver | <p class="shorttext synchronized">Aprobador del departamento</p>
    "! @parameter iv_department_approver_reason | <p class="shorttext synchronized">Aprobador del departamento - motivo</p>
    "! @parameter iv_launch_update_md | <p class="shorttext synchronized">Lanza el proceso de actualización de datos maestros</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS approve_request
      IMPORTING
        iv_request_id                 TYPE zrel_e_request_id
        iv_action                     TYPE zrel_e_action_approv
        iv_reason                     TYPE zrel_e_approval_reason OPTIONAL
        iv_department_approver        TYPE syuname OPTIONAL
        iv_department_approver_reason TYPE string OPTIONAL
        iv_launch_update_md           TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_return                     TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Lanza el proceso de actualización en los datos maestros</p>
    "! @parameter iv_key | <p class="shorttext synchronized">Clave del registro</p>
    "! @parameter iv_request_id | <p class="shorttext synchronized">ID Petición</p>
    "! @parameter iv_send_result_by_mail | <p class="shorttext synchronized">Envio del resultado por mail</p>
    "! @parameter et_return | <p class="shorttext synchronized">Retorno del proceso</p>
    METHODS launch_update_master_data
      IMPORTING
        iv_key                 TYPE zrel_bo_sc_strategy_header-key OPTIONAL
        iv_request_id          TYPE zrel_bo_sc_strategy_header-request_id OPTIONAL
        iv_send_result_by_mail TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_return              TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Obtiene los datos de aprobacion</p>
    "! @parameter user | <p class="shorttext synchronized">Usuarios</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter filter_request | <p class="shorttext synchronized">Filtros para la busqueda</p>
    "! @parameter approval_data | <p class="shorttext synchronized">Datos de la aprobacion</p>
    METHODS get_approval_data
      IMPORTING
        !it_filter_request TYPE zcl_int_ui_contlr_docs=>tt_request_data
        !iv_approver       TYPE zrel_e_approver
      EXPORTING
        et_approval_data   TYPE zif_rel_data=>tt_pgroup_all_data.
  PROTECTED SECTION.
    TYPES: tv_request_type TYPE c LENGTH 1.
    TYPES: tt_bo_sp_buyers TYPE STANDARD TABLE OF zrel_bo_sp_strategy_buyers WITH EMPTY KEY.
    TYPES: tt_bo_sp_approvers TYPE STANDARD TABLE OF zrel_bo_sp_strategy_approvers WITH EMPTY KEY.
    TYPES: BEGIN OF ts_depart_approver,
             wf_id                      TYPE zrel_bo_sc_strategy_header-wf_id,
             department_approver        TYPE syuname,
             department_approver_desc   TYPE string,
             department_approver_reason TYPE string,
             forwarded_by               TYPE syuname,
             forwarded_by_desc          TYPE string,
             forwarded_date             TYPE sy-datum,
             forwarded_time             TYPE sy-uzeit,
           END OF ts_depart_approver.
    TYPES: tt_depart_approver TYPE SORTED TABLE OF ts_depart_approver WITH NON-UNIQUE KEY wf_id.
    CONSTANTS: BEGIN OF cs_request_type,
                 new    TYPE tv_request_type VALUE 'N',
                 update TYPE tv_request_type VALUE 'C',
                 delete TYPE tv_request_type VALUE 'D',
               END OF cs_request_type.

    DATA mo_svc_mngr TYPE REF TO /bobf/if_tra_service_manager.
    DATA mo_txn_mngr TYPE REF TO /bobf/if_tra_transaction_mgr.
    DATA mo_conf_mngr TYPE REF TO /bobf/if_frw_configuration.
    DATA mo_bopf_util TYPE REF TO zcl_ca_bopf_util.
    DATA mo_bopf_helper TYPE REF TO zcl_rel_helper_bo.
    DATA mo_md_query TYPE REF TO zcl_rel_strategy_md_query.
    DATA mt_buyers_sap TYPE zif_rel_data=>tt_users_purchase_group .
    DATA mt_strategy_sap TYPE zif_rel_data=>tt_pgroup_strategy_data .
    DATA mv_langu TYPE sylangu.
    DATA mv_group_create  TYPE frggr.
    DATA mv_strategy_create TYPE frgsx.


    "! <p class="shorttext synchronized">Inicialización variables para el BOPF</p>
    METHODS init_bopf.
    "! <p class="shorttext synchronized">Datos del nodo root del BOPF</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupos de compra</p>
    "! @parameter iv_request_type | <p class="shorttext synchronized">tipo de petición</p>
    "! @parameter ev_root_key | <p class="shorttext synchronized">Clave nodo principal</p>
    "! @parameter ev_request_id | <p class="shorttext synchronized">ID de solicitud</p>
    "! @parameter et_mod | <p class="shorttext synchronized">Datos a modificar en el BOPF</p>
    METHODS fill_data_root_node
      IMPORTING
                is_values       TYPE zrel_bo_sp_strategy_header
                iv_request_type TYPE tv_request_type
      EXPORTING
                ev_root_key     TYPE /bobf/conf_key
                ev_request_id   TYPE zrel_e_request_id
                et_mod          TYPE /bobf/t_frw_modification
      RAISING   zcx_rel.
    "! <p class="shorttext synchronized">Datos del nodo de grupo de compras</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">Clave del nodo principal</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter is_values | <p class="shorttext synchronized">Valores</p>
    "! @parameter iv_request_type | <p class="shorttext synchronized">Tipo de petición</p>
    "! @parameter et_mod | <p class="shorttext synchronized">Datos a modificar en el BOPF</p>
    METHODS fill_data_pgroup_node
      IMPORTING
        iv_root_key       TYPE /bobf/conf_key
        iv_purchase_group TYPE ekgrp
        is_values         TYPE zrel_bo_sp_strategy_pgroup
        iv_request_type   TYPE zcl_rel_strategy_chnge_request=>tv_request_type
      EXPORTING
        et_mod            TYPE /bobf/t_frw_modification.

    "! <p class="shorttext synchronized">Datos del nodo de compradores</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">Clave del nodo principal</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter it_values | <p class="shorttext synchronized">Valores</p>
    "! @parameter iv_request_type | <p class="shorttext synchronized">Tipo de petición</p>
    "! @parameter iv_request_type | <p class="shorttext synchronized">Tipo de petición</p>
    "! @parameter et_mod | <p class="shorttext synchronized">Datos a modificar en el BOPF</p>
    METHODS fill_data_buyers_node
      IMPORTING
        it_values         TYPE zcl_rel_strategy_chnge_request=>tt_bo_sp_buyers
        iv_purchase_group TYPE ekgrp
        iv_root_key       TYPE /bobf/conf_key
        iv_request_type   TYPE zcl_rel_strategy_chnge_request=>tv_request_type
      EXPORTING
        et_mod            TYPE /bobf/t_frw_modification.
    "! <p class="shorttext synchronized">Datos de los nodos de strategias: importe y aprobadores</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">Clave del nodo principal</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter iv_company | <p class="shorttext synchronized">Sociedad</p>
    "! @parameter it_values | <p class="shorttext synchronized">Valores</p>
    "! @parameter iv_request_type | <p class="shorttext synchronized">Tipo de petición</p>
    "! @parameter et_mod | <p class="shorttext synchronized">Datos a modificar en el BOPF</p>
    METHODS fill_data_strategy_node
      IMPORTING
        it_values         TYPE zif_rel_data=>tt_strategy_data
        iv_purchase_group TYPE ekgrp
        iv_company        TYPE bukrs
        iv_root_key       TYPE /bobf/conf_key
        iv_request_type   TYPE zcl_rel_strategy_chnge_request=>tv_request_type
      EXPORTING
        et_mod            TYPE /bobf/t_frw_modification.
    "! <p class="shorttext synchronized">Datos del nodo importe</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">Clave del nodo principal</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter is_values | <p class="shorttext synchronized">Valores</p>
    "! @parameter iv_request_type | <p class="shorttext synchronized">Tipo de petición</p>
    "! @parameter et_mod | <p class="shorttext synchronized">Datos a modificar en el BOPF</p>
    METHODS fill_data_amount_node
      IMPORTING
        is_values         TYPE zrel_bo_sp_strategy_amount
        iv_purchase_group TYPE ekgrp
        iv_root_key       TYPE /bobf/conf_key
        iv_request_type   TYPE zcl_rel_strategy_chnge_request=>tv_request_type
      EXPORTING
        et_mod            TYPE /bobf/t_frw_modification.
    "! <p class="shorttext synchronized">Datos del nodo aprobadores</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">Clave del nodo principal</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter it_values | <p class="shorttext synchronized">Valores</p>
    "! @parameter iv_request_type | <p class="shorttext synchronized">Tipo de petición</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Grupo de liberación</p>
    "! @parameter iv_strategy | <p class="shorttext synchronized">Estrategia de liberación</p>
    "! @parameter et_mod | <p class="shorttext synchronized">Datos a modificar en el BOPF</p>
    METHODS fill_data_approvers_node
      IMPORTING
        it_values         TYPE zcl_rel_strategy_chnge_request=>tt_bo_sp_approvers
        iv_purchase_group TYPE ekgrp
        iv_root_key       TYPE /bobf/conf_key
        iv_request_type   TYPE zcl_rel_strategy_chnge_request=>tv_request_type
        iv_group          TYPE t16fs-frggr
        iv_strategy       TYPE t16fs-frgsx
      EXPORTING
        et_mod            TYPE /bobf/t_frw_modification.



    "! <p class="shorttext synchronized">Obtención de datos a partir del nodo cabecera</p>
    "! @parameter it_header | <p class="shorttext synchronized">Cabecera</p>
    "! @parameter it_buyers | <p class="shorttext synchronized">Compradores</p>
    "! @parameter it_buyers_sap | <p class="shorttext synchronized">Compradores SAP</p>
    "! @parameter it_amount | <p class="shorttext synchronized">Importes (o estrategias)</p>
    "! @parameter it_approvers | <p class="shorttext synchronized">Aprobadores</p>
    "! @parameter it_pgroup | <p class="shorttext synchronized">Maestro del grupo de compras</p>
    "! @parameter et_output_data | <p class="shorttext synchronized">Salida de datos</p>
    METHODS convert_bopf_data_2_output
      IMPORTING
        it_header      TYPE zrel_bo_i_strategy_header
        it_buyers      TYPE zrel_bo_i_strategy_buyers
        it_amount      TYPE zrel_bo_i_strategy_amount
        it_approvers   TYPE zrel_bo_i_strategy_approvers
        it_pgroup      TYPE zrel_bo_i_strategy_pgroup
      EXPORTING
        et_output_data TYPE tt_pgroup_request_data.
    "! <p class="shorttext synchronized">Determina grupo y estrategia liberacion en creacion</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter iv_company | <p class="shorttext synchronized">Sociedad</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Grupo de liberación</p>
    "! @parameter ev_group | <p class="shorttext synchronized">Grupo de liberación</p>
    "! @parameter ev_strategy | <p class="shorttext synchronized">Estrategia</p>
    METHODS determine_new_strategy_code
      IMPORTING
        iv_purchase_group TYPE ekgrp
        iv_company        TYPE bukrs
        iv_group          TYPE zrel_bo_sp_strategy_amount-group OPTIONAL
      EXPORTING
        ev_group          TYPE zrel_bo_sp_strategy_amount-group
        ev_strategy       TYPE zrel_bo_sp_strategy_amount-strategy.
    "! <p class="shorttext synchronized">Lanzamiento del workflow de aprobación</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">Id de cabecera del workflow a lanzar</p>
    "! @parameter iv_request_id | <p class="shorttext synchronized">Id solicitud</p>
    "! @parameter et_return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS launch_wf_new_request
      IMPORTING
        iv_root_key   TYPE /bobf/conf_key
        iv_request_id TYPE zrel_e_request_id
      EXPORTING
        et_return     TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Actualiza el aprobador de departamento en el WF</p>
    "! @parameter iv_department_approver | <p class="shorttext synchronized">Aprobador del departamento</p>
    "! @parameter iv_department_approver_reason | <p class="shorttext synchronized">Aprobador del departamento - motivo</p>
    "! @parameter iv_wf_id | <p class="shorttext synchronized">Id solicitud</p>
    METHODS update_depart_approver_wf
      IMPORTING
        iv_department_approver        TYPE syuname
        iv_department_approver_reason TYPE string OPTIONAL
        iv_wf_id                      TYPE zrel_bo_sc_strategy_header-wf_id.
    "! <p class="shorttext synchronized">Obtiene el aprobador de departamento en el WF</p>
    "! @parameter iv_department_approver | <p class="shorttext synchronized">Aprobador del departamento</p>
    "! @parameter iv_department_approver_reason | <p class="shorttext synchronized">Aprobador del departamento - motivo</p>
    "! @parameter iv_wf_id | <p class="shorttext synchronized">Id solicitud</p>
    METHODS get_depart_approver_wf
      IMPORTING
        it_r_wf_id   TYPE zif_rel_data=>tt_r_wf_id
      EXPORTING
        et_approvers TYPE tt_depart_approver.
    "! <p class="shorttext synchronized">Realiza la aprobación del paso</p>
    "! @parameter is_header | <p class="shorttext synchronized">Cabecera</p>
    "! @parameter iv_action | <p class="shorttext synchronized">Acción</p>
    "! @parameter iv_reason | <p class="shorttext synchronized">Motivo de aprobación</p>
    "! @parameter ev_completed | <p class="shorttext synchronized">Completado</p>
    "! @parameter ev_next_status | <p class="shorttext synchronized">Siguiente status</p>
    "! @parameter et_return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS approve_step_wf
      IMPORTING
        is_header      TYPE zrel_bo_sc_strategy_header
        iv_action      TYPE zrel_e_action_approv
        iv_reason      TYPE string
      EXPORTING
        ev_completed   TYPE sap_bool
        ev_next_status TYPE zwfe_e_status
        et_return      TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Convierte los filtros en ranges</p>
    "! @parameter it_filter_request | <p class="shorttext synchronized">Filtros</p>
    "! @parameter et_r_request_id | <p class="shorttext synchronized">Rango de ID de peticiones</p>
    "! @parameter et_r_request_date | <p class="shorttext synchronized">Rango de fecha peticion</p>
    "! @parameter et_r_request_status | <p class="shorttext synchronized">Rango de status</p>
    "! @parameter et_r_request_by | <p class="shorttext synchronized">Rango de solicitante</p>
    "! @parameter et_r_purchase_group | <p class="shorttext synchronized">Rango de grupo de compras</p>
    "! @parameter et_r_dept_subs | <p class="shorttext synchronized">Rango de departamento</p>
    "! @parameter et_r_approver | <p class="shorttext synchronized">Aprobadores</p>
    "! @parameter et_bopf_selections | <p class="shorttext synchronized">Selecciones para filtrar BOPF</p>
    METHODS convert_filters_2_ranges
      IMPORTING
        it_filter_request   TYPE zcl_int_ui_contlr_docs=>tt_request_data
      EXPORTING
        et_r_request_id     TYPE zif_rel_data=>tt_r_request_id
        et_r_request_date   TYPE zif_rel_data=>tt_r_date
        et_r_request_status TYPE zif_rel_data=>tt_r_request_status
        et_r_request_by     TYPE zif_rel_data=>tt_r_username
        et_r_purchase_group TYPE zif_rel_data=>tt_r_purchase_group
        et_r_dept_subs      TYPE zif_rel_data=>tt_r_dept_subs
        et_bopf_selections  TYPE /bobf/t_frw_query_selparam.
    "! <p class="shorttext synchronized">Obtiene los datos maestros a partir de los grupos de compra</p>
    "! @parameter it_r_purhase_group | <p class="shorttext synchronized">Filtros</p>
    "! @parameter et_strategies | <p class="shorttext synchronized">Datos de las estrategias</p>
    "! @parameter et_buyers | <p class="shorttext synchronized">Compradores</p>
    "! @parameter et_pgroup_info | <p class="shorttext synchronized">Info del grupo de compras</p>
    METHODS get_pgroup_sap_md
      IMPORTING
        it_r_purchase_group TYPE zif_rel_data=>tt_r_purchase_group
      EXPORTING
        et_strategies       TYPE zif_rel_data=>tt_pgroup_strategy_data
        et_buyers           TYPE zif_rel_data=>tt_users_purchase_group
        et_pgroup_info      TYPE zcl_rel_strategy_md_query=>tt_pgroup_info .
    "! <p class="shorttext synchronized">Se crea la foto de lo que hay en SAP</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">ID del registro donde se hará la foto</p>
    "! @parameter iv_commit | <p class="shorttext synchronized">Se graba los datos</p>
    METHODS create_sap_snapshopt
      IMPORTING
        iv_root_key TYPE /bobf/conf_key
        iv_commit   TYPE sap_bool DEFAULT abap_false.
    "! <p class="shorttext synchronized">Rellena los datos de SAP proveniente del maestro de datos</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter it_strategies | <p class="shorttext synchronized">Datos de estrategias</p>
    "! @parameter it_buyers | <p class="shorttext synchronized">Compradores</p>
    "! @parameter it_pgroup | <p class="shorttext synchronized">Información del grupo de compras</p>
    "! @parameter cs_approval_data | <p class="shorttext synchronized">Datos de aprobación</p>
    METHODS fill_apprv_data_sap_from_md
      IMPORTING
        iv_purchase_group TYPE ekgrp
        it_strategies     TYPE zif_rel_data=>tt_pgroup_strategy_data
        it_buyers         TYPE zif_rel_data=>tt_users_purchase_group
        it_pgroup         TYPE zcl_rel_strategy_md_query=>tt_pgroup_info
      CHANGING
        cs_approval_data  TYPE zif_rel_data=>ts_pgroup_all_data.
    "! <p class="shorttext synchronized">Rellena los datos de SAP proveniente del BOPF</p>
    "! @parameter iv_root_key | <p class="shorttext synchronized">Clave de la cabecera</p>
    "! @parameter it_buyers | <p class="shorttext synchronized">Compradores</p>
    "! @parameter it_amount | <p class="shorttext synchronized">Importes</p>
    "! @parameter it_approvers | <p class="shorttext synchronized">Aprobadores</p>
    "! @parameter it_pgroup | <p class="shorttext synchronized">Info del grupo de compras</p>
    METHODS fill_apprv_data_sap_from_bo
      IMPORTING
        iv_root_key      TYPE zrel_bo_sc_strategy_header-key
        it_buyers        TYPE zrel_bo_i_strategy_buyers_sap
        it_amount        TYPE zrel_bo_i_strategy_amount_sap
        it_approvers     TYPE zrel_bo_i_strategy_apprv_sap
        it_pgroup        TYPE zrel_bo_i_strategy_pgroup_sap
      CHANGING
        cs_approval_data TYPE zif_rel_data=>ts_pgroup_all_data.
    "! <p class="shorttext synchronized">Id de peticiones del aprobador</p>
    "! @parameter it_bopf_selections | <p class="shorttext synchronized">Selecciones del BOPF</p>
    "! @parameter it_r_approvers | <p class="shorttext synchronized">Aprobadores</p>
    "! @parameter et_r_request_id | <p class="shorttext synchronized">Rango de ID de peticiones</p>
    METHODS get_request_id_of_approvers
      IMPORTING
        it_bopf_selections TYPE /bobf/t_frw_query_selparam
        iv_approver        TYPE syuname
      EXPORTING
        et_r_request_id    TYPE zif_rel_data=>tt_r_request_id.
    "! <p class="shorttext synchronized">Envio de mail del workflow aprobado/rechazado</p>
    "! @parameter iv_key | <p class="shorttext synchronized">Clave del nodo</p>
    "! @parameter et_return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS send_mail_wf_approved
      IMPORTING
        iv_key    TYPE zrel_bo_sc_strategy_header-key
      EXPORTING
        et_return TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Envio de mail de aprobación al responsable departamento</p>
    "! @parameter is_header | <p class="shorttext synchronized">Cabecera</p>
    "! @parameter iv_department_approver | <p class="shorttext synchronized">Aprobador</p>
    "! @parameter iv_department_approver_reason | <p class="shorttext synchronized">Motivo</p>
    "! @parameter et_return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS send_mail_approv_depart
      IMPORTING
                is_header                     TYPE zrel_bo_sc_strategy_header
                iv_department_approver        TYPE syuname
                iv_department_approver_reason TYPE string
      EXPORTING et_return                     TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Envio mail de aprobación</p>
    "! @parameter iv_key | <p class="shorttext synchronized">Clave</p>
    METHODS send_mail_approve
      IMPORTING
        iv_key TYPE /bobf/conf_key.
  PRIVATE SECTION.

ENDCLASS.



CLASS zcl_rel_strategy_chnge_request IMPLEMENTATION.
  METHOD constructor.
    mv_langu = iv_langu.

    " Consulta de datos de maestros
    mo_md_query = NEW zcl_rel_strategy_md_query( iv_langu = mv_langu ).

    " Inicializacion de variables para el BOPF
    init_bopf( ).
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

  METHOD new_request.

    " Ahora con los datos de entrada + los de SAP se rellenan las distintos datos
    TRY.
        " Nodo principal
        fill_data_root_node( EXPORTING is_values = CORRESPONDING #( is_request_data )
                                       iv_request_type = cs_request_type-new
                             IMPORTING ev_root_key = DATA(lv_root_key)
                                       ev_request_id = DATA(lv_request_id)
                                       et_mod = DATA(lt_mod) ).

        " Datos del grupo de compra
        fill_data_pgroup_node( EXPORTING iv_root_key = lv_root_key
                                         iv_purchase_group = is_request_data-purchase_group
                                         is_values = VALUE #( purchase_group_desc = is_request_data-purchase_group_desc
                                                              cdchngind = is_request_data-cdchngind )
                                         iv_request_type = cs_request_type-new
                               IMPORTING et_mod = DATA(lt_mod_pgroup) ).
        INSERT LINES OF lt_mod_pgroup INTO TABLE lt_mod.

        " Nodo de los compradores
        fill_data_buyers_node(  EXPORTING it_values = CORRESPONDING tt_bo_sp_buyers( is_request_data-buyers )
                                          iv_purchase_group = is_request_data-purchase_group
                                          iv_root_key = lv_root_key
                                          iv_request_type = cs_request_type-new
                                IMPORTING et_mod = DATA(lt_mod_buyers) ).
        INSERT LINES OF lt_mod_buyers INTO TABLE lt_mod.

        " Nodo de las estrategias. Dentro de los datos de las estregias están la de los aprobadores. Todo se
        " procesa a la vez porque están relacionados.
        fill_data_strategy_node( EXPORTING it_values = is_request_data-strategies
                                           iv_purchase_group = is_request_data-purchase_group
                                           iv_company = is_request_data-company
                                           iv_root_key = lv_root_key
                                           iv_request_type = cs_request_type-new
                                IMPORTING et_mod = DATA(lt_mod_strategies) ).
        INSERT LINES OF lt_mod_strategies INTO TABLE lt_mod.

        " Finalmente se lanza el proceso que almacena, que no graba en base de datos, los datos del BOPF. Porque
        " antes hay que lanzar el workflow
        mo_bopf_helper->modify_data_bopf( EXPORTING it_mod = lt_mod
                                          IMPORTING ev_error_modify = DATA(lv_error_modify) ).

        IF lv_error_modify = abap_false.

          " Se lanza el proceso de workflow para la nueva petición
          launch_wf_new_request( EXPORTING iv_root_key = lv_root_key
                                     iv_request_id = lv_request_id
                           IMPORTING et_return = et_return ).

          " Si en el proceso de lanzar el WF no hay errores entonces devuelvo el ID generado.
          READ TABLE et_return TRANSPORTING NO FIELDS WITH KEY type = zif_rel_data=>cs_msg-type_error.
          IF sy-subrc NE 0.
            ev_request_id = ev_request_id.
          ENDIF.

        ELSE.
          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                                                                 i_id         = zif_rel_data=>cs_msg-id
                                                                 i_number     = '004'
                                                                 i_langu      = mv_langu ) INTO TABLE et_return.
        ENDIF.


      CATCH zcx_rel INTO DATA(lx_rel).

        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                                               i_id         = lx_rel->if_t100_message~t100key-msgid
                                               i_number     = lx_rel->if_t100_message~t100key-msgno
                                               i_message_v1 = lx_rel->mv_msgv1
                                               i_message_v2 = lx_rel->mv_msgv2
                                               i_message_v3 = lx_rel->mv_msgv3
                                               i_message_v4 = lx_rel->mv_msgv4
                                               i_langu      = mv_langu ) INTO TABLE et_return.

    ENDTRY.
  ENDMETHOD.


  METHOD fill_data_root_node.

    CLEAR: ev_request_id, ev_root_key, et_mod.

    DATA(lo_root) = NEW zrel_bo_sc_strategy_header(  ).
    ASSIGN lo_root->* TO FIELD-SYMBOL(<ls_root>).

    " Se pasan los campos de valores.
    <ls_root> = CORRESPONDING #( is_values ).

    CASE iv_request_type.
      WHEN cs_request_type-new.

        " Clave del nodo
        ev_root_key = lo_root->key = /bobf/cl_frw_factory=>get_new_key( ).

        " Nota Iván: Hay campos que se determinan en la clase ZCL_REL_D_STRATEGY_HEADER->NEW_REQUEST. Ejemplo de campos:
        " solicitante, fecha solicitud o estado. El único campo que hago fuera es el ID externo que lo hago para simplificar
        " el proceso de creación.

        " ID de solicitud
        CALL FUNCTION 'NUMBER_GET_NEXT'
          EXPORTING
            nr_range_nr             = zif_rel_data=>cs_strategy-change_request-snro_nrrange
            object                  = zif_rel_data=>cs_strategy-change_request-snro_object
          IMPORTING
            number                  = ev_request_id
          EXCEPTIONS
            interval_not_found      = 1                " Intervalos no encontrados
            number_range_not_intern = 2                " Rango de números no es interno
            object_not_found        = 3                " Objeto no definido en TNRO
            quantity_is_0           = 4                " La cantidad de números solicitados debe ser superior a 0
            quantity_is_not_1       = 5                " Number of numbers requested must be 1
            interval_overflow       = 6                " Interval used up. Change not possible.
            buffer_overflow         = 7                " Buffer is full
            OTHERS                  = 8.
        IF sy-subrc NE 0.
          RAISE EXCEPTION TYPE zcx_rel
            EXPORTING
              textid = zcx_rel=>error_get_request_id.
        ENDIF.

        lo_root->request_id = ev_request_id.

        " Busco el departamento del grupo de compras
        SELECT SINGLE dept_subs INTO lo_root->dept_subs
               FROM zrel_t003
               WHERE ekgrp = lo_root->purchase_group.

      WHEN cs_request_type-update.
    ENDCASE.

    INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-root
                    change_mode = COND #( WHEN iv_request_type = cs_request_type-new THEN /bobf/if_frw_c=>sc_modify_create ELSE /bobf/if_frw_c=>sc_modify_update )
                    key = lo_root->key
                    data = lo_root )
                 INTO TABLE et_mod.

  ENDMETHOD.


  METHOD fill_data_pgroup_node.

    CLEAR: et_mod.

    DATA(lo_pgrup) = NEW zrel_bo_sc_strategy_pgroup(  ).
    ASSIGN lo_pgrup->* TO FIELD-SYMBOL(<ls_pgroup>).

    " Se pasan los campos de valores.
    <ls_pgroup> = CORRESPONDING #( is_values ).

    CASE iv_request_type.
      WHEN cs_request_type-new.

        <ls_pgroup>-key = /bobf/cl_frw_factory=>get_new_key( ).


    ENDCASE.

    INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-pgroup
                    change_mode = COND #( WHEN iv_request_type = cs_request_type-new THEN /bobf/if_frw_c=>sc_modify_create ELSE /bobf/if_frw_c=>sc_modify_update )
                    key = lo_pgrup->key
                    data = lo_pgrup
                    source_node = zif_rel_bo_strategy_c=>sc_node-root
                    association = zif_rel_bo_strategy_c=>sc_association-root-pgroup
                    source_key = iv_root_key )
                 INTO TABLE et_mod.


  ENDMETHOD.





  METHOD fill_data_buyers_node.

    CLEAR et_mod.

    " Nota Iván: Aquí, el tema de modificación no se plantea inicialmente. Solo creación.
    " Porque la modificación para este caso sería borrar lo que has introducido previamente y
    " añadir lo nuevo. De momento no se implementa porque incialmente el proyecto no
    " tiene pensando modificar procesos de aprobación.

    " Solo si hay alguna petición de cambio es cuando se realiza el proceso.
    " Este control lo podría haber puesto el loop donde se rellenan los valores
    " pero lo dejo montado para casuísticas que puedan aparecer y me obliguen a cambiarlo.
    " De esta manera lo tengo todo cubierto.
    LOOP AT it_values TRANSPORTING NO FIELDS WHERE cdchngind IS NOT INITIAL.
      EXIT.
    ENDLOOP.
    IF sy-subrc = 0.

      " Se rellenan los valores pasados por parámetro
      LOOP AT it_values ASSIGNING FIELD-SYMBOL(<ls_values>) WHERE cdchngind IS NOT INITIAL.

        DATA(lo_buyers) = NEW zrel_bo_sc_strategy_buyers(  ).
        ASSIGN lo_buyers->* TO FIELD-SYMBOL(<ls_buyers>).

        " Se pasan los campos de valores.
        <ls_buyers> = CORRESPONDING #( <ls_values> ).
        <ls_buyers>-key = /bobf/cl_frw_factory=>get_new_key( ).

        INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-buyers
                        change_mode = /bobf/if_frw_c=>sc_modify_create
                        key = lo_buyers->key
                        data = lo_buyers
                        source_node = zif_rel_bo_strategy_c=>sc_node-root
                        association = zif_rel_bo_strategy_c=>sc_association-root-buyers
                        source_key = iv_root_key )
                     INTO TABLE et_mod.
      ENDLOOP.

      " Ahora la foto de lo que hay en SAP. En ese caso solo se realiza la creación.
      IF iv_request_type = cs_request_type-new.
        LOOP AT mt_buyers_sap ASSIGNING FIELD-SYMBOL(<ls_buyers_sap>).
          DATA(lo_buyers_sap) = NEW zrel_bo_sc_strategy_buyers_sap(  ).
          ASSIGN lo_buyers_sap->* TO FIELD-SYMBOL(<ls_buyers_bo_sap>).

          " Se pasan los campos de valores.
          <ls_buyers_bo_sap> = CORRESPONDING #( <ls_buyers_sap> ).
          <ls_buyers_bo_sap>-key = /bobf/cl_frw_factory=>get_new_key( ).

          INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-buyers_sap
                          change_mode = /bobf/if_frw_c=>sc_modify_create
                          key = lo_buyers_sap->key
                          data = lo_buyers_sap
                          source_node = zif_rel_bo_strategy_c=>sc_node-root
                          association = zif_rel_bo_strategy_c=>sc_association-root-buyers_sap
                          source_key = iv_root_key )
                       INTO TABLE et_mod.
        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD fill_data_strategy_node.

    CLEAR: et_mod.

    " En los valores tenemos primero los importes y dentro de el esta el campo de los aprobadores
    " Se hará un split porque el BOPF esta por separado. Porque realmente son cosas independientes pero
    " a nivel de frontend es correcto que se pase una debajo del otro.
    " Aunque la info se guarde de manera independiente voy a guardar los datos de la estrategia cuando:
    " 1) Se modifica la propia estrategía 2) Cuando se modifique los aprobador de la estrategia aunque
    " no se haya modificado. El motivo es en la recuperación de datos/histórico se pueda relacionar
    " la modificación de ese aprobador a que estrategia pertenecia. Esto me obligará a vigilar el
    " indicador de actualización cuando se modifique la parametrización estándar.
    LOOP AT it_values ASSIGNING FIELD-SYMBOL(<ls_values>).

      LOOP AT <ls_values>-approvers TRANSPORTING NO FIELDS WHERE cdchngind IS NOT INITIAL.
        EXIT.
      ENDLOOP.

      IF sy-subrc = 0 OR <ls_values>-cdchngind IS NOT INITIAL.

        " Se rellena el nodo de importe.
        DATA(ls_values_amount) = CORRESPONDING  zrel_bo_sp_strategy_amount( <ls_values> ).

        " Si la estrategia se esta creando hay que determinar el grupo de liberación y estrategia que
        " luego se propagará para los importes
        IF ls_values_amount-cdchngind = zif_rel_data=>cs_strategy-change_request-change_indicator-insert.
          determine_new_strategy_code( EXPORTING iv_purchase_group = iv_purchase_group
                                                 iv_company = iv_company
                                                 iv_group = <ls_values>-group
                                       IMPORTING ev_group = ls_values_amount-group
                                                 ev_strategy = ls_values_amount-strategy ).
        ENDIF.

        fill_data_amount_node( EXPORTING is_values = ls_values_amount
                                       iv_purchase_group = iv_purchase_group
                                       iv_root_key = iv_root_key
                                       iv_request_type = iv_request_type
                            IMPORTING et_mod = DATA(lt_mod_amount) ).
        INSERT LINES OF lt_mod_amount INTO TABLE et_mod.

        " Se rellena el nodo de los aprobadores.
        fill_data_approvers_node( EXPORTING it_values = CORRESPONDING tt_bo_sp_approvers( <ls_values>-approvers )
                                             iv_purchase_group = iv_purchase_group
                                             iv_root_key = iv_root_key
                                             iv_request_type = iv_request_type
                                             iv_group = ls_values_amount-group
                                             iv_strategy = ls_values_amount-strategy
                                  IMPORTING et_mod = DATA(lt_mod_approvers) ).
        INSERT LINES OF lt_mod_approvers INTO TABLE et_mod.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD fill_data_amount_node.

    CLEAR et_mod.

    DATA(lo_amount) = NEW zrel_bo_sc_strategy_amount(  ).
    ASSIGN lo_amount->* TO FIELD-SYMBOL(<ls_amount>).

    " Se pasan los campos de valores.
    <ls_amount> = CORRESPONDING #( is_values ).

    CASE iv_request_type.
      WHEN cs_request_type-new.
        lo_amount->key = /bobf/cl_frw_factory=>get_new_key( ).

      WHEN cs_request_type-update.
    ENDCASE.

    INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-amount
            change_mode = COND #( WHEN iv_request_type = cs_request_type-new THEN /bobf/if_frw_c=>sc_modify_create ELSE /bobf/if_frw_c=>sc_modify_update )
            key = lo_amount->key
            data = lo_amount
            source_node = zif_rel_bo_strategy_c=>sc_node-root
            association = zif_rel_bo_strategy_c=>sc_association-root-amount
            source_key = iv_root_key )
         INTO TABLE et_mod.

  ENDMETHOD.


  METHOD fill_data_approvers_node.
    CLEAR et_mod.

    " Solo se actualiza los registros modificados
    LOOP AT it_values ASSIGNING FIELD-SYMBOL(<ls_values>) WHERE cdchngind IS NOT INITIAL.
      DATA(lo_approvers) = NEW zrel_bo_sc_strategy_approvers( ).
      ASSIGN lo_approvers->* TO FIELD-SYMBOL(<ls_approvers>).

      " Se pasan los campos de valores.
      <ls_approvers> = CORRESPONDING #( <ls_values> ).
      lo_approvers->group = iv_group.
      lo_approvers->strategy = iv_strategy.

      CASE iv_request_type.
        WHEN cs_request_type-new.
          lo_approvers->key = /bobf/cl_frw_factory=>get_new_key( ).


        WHEN cs_request_type-update.
      ENDCASE.

      INSERT VALUE #( node = zif_rel_bo_strategy_c=>sc_node-approvers
              change_mode = COND #( WHEN iv_request_type = cs_request_type-new THEN /bobf/if_frw_c=>sc_modify_create ELSE /bobf/if_frw_c=>sc_modify_update )
              key = lo_approvers->key
              data = lo_approvers
              source_node = zif_rel_bo_strategy_c=>sc_node-root
              association = zif_rel_bo_strategy_c=>sc_association-root-approvers
              source_key = iv_root_key )
           INTO TABLE et_mod.

    ENDLOOP.
  ENDMETHOD.


  METHOD get_interal_id_from_external.

    DATA lt_data TYPE zrel_bo_i_strategy_header.
    CLEAR: et_external_id.

    DATA(lt_params) = VALUE /bobf/t_frw_query_selparam( FOR <wa> IN it_internal_id
                                                        ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-key
                                                          sign = 'I'
                                                          option = 'EQ'
                                                          low = <wa>-key ) ).

    mo_svc_mngr->query( EXPORTING iv_query_key = zif_rel_bo_strategy_c=>sc_query-root-select_by_elements
                                    it_selection_parameters = lt_params
                                    iv_fill_data = abap_true
                        IMPORTING et_data = lt_data ).


    et_external_id = VALUE #( FOR <wa1> IN lt_data ( internal_id = <wa1>-key external_id = <wa1>-request_id ) ).

  ENDMETHOD.

  METHOD get_pgroup_request_data.

    CLEAR: et_request_data.


    DATA(lt_params) = VALUE /bobf/t_frw_query_selparam( FOR <wa> IN it_r_pgroup
                                                        ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-purchase_group
                                                          sign = <wa>-sign
                                                          option = <wa>-option
                                                          low = <wa>-low ) ).
    IF it_r_status IS NOT INITIAL. " Se filtra por estado si esta informado
      lt_params = VALUE #( BASE lt_params FOR <wa1> IN it_r_status
                                ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-request_status
                                  sign = <wa1>-sign
                                  option = <wa1>-option
                                  low = <wa1>-low ) ).
    ENDIF.

    " Se obtienen los datos a partir del nodo de cabecera
    mo_bopf_helper->query_all_data_from_header( EXPORTING it_params = lt_params
                                IMPORTING et_header = DATA(lt_header)
                                          et_buyers = DATA(lt_buyers)
                                          et_buyers_sap = DATA(lt_buyers_sap)
                                          et_amounts = DATA(lt_amount)
                                          et_approvers = DATA(lt_approvers)
                                          et_pgroup = DATA(lt_pgroup) ).

    " Se convierte los datos del BOPF a la estructura de salida
    convert_bopf_data_2_output( EXPORTING it_header = lt_header
                                          it_buyers = lt_buyers
                                          it_amount = lt_amount
                                          it_approvers = lt_approvers
                                          it_pgroup = lt_pgroup
                                IMPORTING et_output_data = et_request_data ).

  ENDMETHOD.





  METHOD convert_bopf_data_2_output.

    CLEAR: et_output_data.

    LOOP AT it_header ASSIGNING FIELD-SYMBOL(<ls_header>).

      " Datos iniciales de cabecera
      INSERT CORRESPONDING #( <ls_header> ) INTO TABLE et_output_data ASSIGNING FIELD-SYMBOL(<ls_output>).

      " Compradores
      <ls_output>-buyers = VALUE #( FOR <wa> IN it_buyers WHERE ( parent_key = <ls_header>-key ) ( <wa> ) ).
      " Compradores originales de SAP
*      <ls_output>-buyers_sap = VALUE #( FOR <wa1> IN it_buyers_sap WHERE ( parent_key = <ls_header>-key ) ( <wa1> ) ).

      " Cambios en el propio grupo de compras. En ese caso solo es la descripción
      READ TABLE it_pgroup INTO <ls_output>-pgroup WITH KEY parent_key = <ls_header>-key.

      " Los datos de strategias se construyen de dos nodos del BOPF
      LOOP AT it_amount ASSIGNING FIELD-SYMBOL(<ls_amount>) WHERE parent_key = <ls_header>-key.
        INSERT CORRESPONDING #( <ls_amount> ) INTO TABLE <ls_output>-strategies ASSIGNING FIELD-SYMBOL(<ls_strategies>).

        <ls_strategies>-approvers = VALUE #( FOR <wa2> IN it_approvers WHERE ( parent_key = <ls_header>-key
                                                                               AND group = <ls_amount>-group
                                                                               AND strategy = <ls_amount>-strategy )
                                                                             ( <wa2> ) ).
        SORT  <ls_strategies>-approvers BY level.
      ENDLOOP.

      SORT <ls_output>-strategies BY level.

    ENDLOOP.

  ENDMETHOD.


  METHOD determine_new_strategy_code.
    CLEAR: ev_group, ev_strategy.

    " Si el grupo pasado por parámetro esta en blanco o el primer digito es el de creación, buscaré el grupo por sociedad/grupo de compras, si viene informada,
    " y sino pues crea un grupo ficticio.
    IF iv_group(1) = zif_rel_data=>cs_strategy-change_request-strategy_ini_char_new OR iv_group IS INITIAL.
      " El grupo de liberación primero voy a obtener del grupo de compras. Si no es posible entonces creará uno ficticio.
      ev_group = mo_md_query->get_lib_group_from_pgroup( iv_purchase_group = iv_purchase_group
                                                         iv_company = iv_company  ).

      IF ev_group IS INITIAL.
        " El codigo definitivo se determinará una vez este aprobado la solicitud y se vaya a crear los datos maestros. En el momento
        " de grabar la solicitud pongo un carácter para que luego sea fácil identificar que se esta creando.
        mv_group_create = COND #( WHEN mv_group_create IS INITIAL THEN  |{ zif_rel_data=>cs_strategy-change_request-strategy_ini_char_new }1|
                                                                  ELSE |{ zif_rel_data=>cs_strategy-change_request-strategy_ini_char_new }{ ( mv_group_create+1(1) + 1 ) }| ).
        ev_group = mv_group_create.
      ENDIF.

    ELSE.
      ev_group = iv_group.
    ENDIF.




    " La estrategía si que será un numero incremental
    mv_strategy_create = COND #( WHEN mv_strategy_create IS INITIAL THEN |{ zif_rel_data=>cs_strategy-change_request-strategy_ini_char_new }1|
                                                                    ELSE |{ zif_rel_data=>cs_strategy-change_request-strategy_ini_char_new }{ ( mv_strategy_create+1(1) + 1 ) }| ).
    ev_strategy = mv_strategy_create.

  ENDMETHOD.

  METHOD launch_wf_new_request.
    DATA lt_return TYPE  bapiret2_t .
    DATA lt_result_process TYPE zrel_i_a_exp_new_workflow.

    CLEAR: et_return.

    " Parámetros de entrada
    DATA(lo_params) = NEW zrel_s_a_general_params( langu = mv_langu ).

    mo_svc_mngr->do_action( EXPORTING iv_act_key = zif_rel_bo_strategy_c=>sc_action-root-launch_workflow
                                  it_key = VALUE #( ( key = iv_root_key ) )
                                  is_parameters = lo_params
                        IMPORTING eo_message = DATA(lo_message)
                                  et_data = lt_result_process ).

    " Se pasan los mensajes del BOPF a la estructura de retorno
    mo_bopf_util->conv_message_bopf_2_return(
      EXPORTING
        io_message = lo_message
        iv_langu   = mv_langu
      CHANGING
        ct_return  = lt_return ).

    " Si la acción devuelve error se lanza el rollback de los datos y se devuelve un mensaje indicando el error.
    READ TABLE lt_return TRANSPORTING NO FIELDS WITH KEY type = zif_rel_data=>cs_msg-type_error.
    IF sy-subrc = 0.
      " Si hay error en el WF hago un rollback para que los datos se borren del BOPF
      mo_bopf_util->rollback( ).

      INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                                                 i_id         = zif_rel_data=>cs_msg-id
                                                 i_number     = '004'
                                                 i_langu      = mv_langu ) INTO TABLE et_return.
    ELSE.
      " Si va todo bien se graban los datos, controlando que no haya errores (que no debería porque tendría que haberlos dato
      " en el envio de los datos.
      mo_bopf_helper->save_data_bopf( IMPORTING ev_error_save = DATA(lv_error_save) ).

      IF lv_error_save = abap_false.
        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_success
                                               i_id         = zif_rel_data=>cs_msg-id
                                               i_number     = '003'
                                               i_message_v1 = |{ iv_request_id ALPHA = OUT }|
                                               i_langu      = mv_langu ) INTO TABLE et_return.

        " Paso final es el envio del mail de notificación. Los parámetros de entra son los mismos que para el lanzamiento
        " del workflow.
        send_mail_approve( EXPORTING iv_key = iv_root_key ).

      ELSE.
        " Fuerzo el rollback para que no quede nada intermedio ni del bopf ni del workflow
        mo_bopf_util->rollback( ).

        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                      i_id         = zif_rel_data=>cs_msg-id
                      i_number     = '004'
                      i_langu      = mv_langu ) INTO TABLE et_return.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD approve_request.
    DATA lt_header TYPE zrel_bo_i_strategy_header .

    CLEAR: et_return.

    DATA(lt_params) = VALUE /bobf/t_frw_query_selparam( ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-request_id
                                                          sign = 'I'
                                                          option = 'EQ'
                                                          low = iv_request_id ) ).

    mo_svc_mngr->query( EXPORTING iv_query_key = zif_rel_bo_strategy_c=>sc_query-root-select_by_elements
                                    it_selection_parameters = lt_params
                                    iv_fill_data = abap_true
                        IMPORTING et_data = lt_header ).

    IF lt_header IS NOT INITIAL.



      " Se mira si esta pendiente de aprobadr
      IF lt_header[ 1 ]-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending
         OR lt_header[ 1 ]-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending_confirmation.

        " Se mira si tenemos el ID de workflow generado.
        IF lt_header[ 1 ]-wf_id IS NOT INITIAL.

          " Si se pasa el aprobador de departamento que sería el segundo nivel se actualiza en los valores del WF
          IF iv_department_approver IS NOT INITIAL.
            update_depart_approver_wf( EXPORTING iv_department_approver = iv_department_approver
                                                 iv_department_approver_reason = iv_department_approver_reason
                                                 iv_wf_id = lt_header[ 1 ]-wf_id ).
          ENDIF.

          " Ahora realizamos la aprobación del workflow. Este nos dirá si esta completo o no. Si lo esta es cuando se lanzará
          " el proceso de actualización en el modelo de SAP.
          approve_step_wf( EXPORTING is_header = lt_header[ 1 ]
                                     iv_action = iv_action
                                     iv_reason = iv_reason
                           IMPORTING ev_completed = DATA(lv_completed)
                           ev_next_status = DATA(lv_next_status)
                                     et_return = DATA(lt_return_approv) ).

          INSERT LINES OF lt_return_approv INTO TABLE et_return.

          " Si no hay errores en el proceso de actualización y la aprobación del WF devuelve que esta completado
          " es el momento de realizar los siguientes pasos:
          " 1) Actualizar los datos en el maestro.
          " 2) Enviar el mail al aprobador que la solicitud ha sido aprobada y los cambios realizados
          READ TABLE lt_return_approv TRANSPORTING NO FIELDS WITH KEY type = zif_rel_data=>cs_msg-type_error.
          IF sy-subrc NE 0.
            IF lv_completed = abap_true.

              " El proceso de actualización se tiene que lanzar en fondo
              " La actualización de datos maestros se realiza si se indica por parámetro. Esto se hace para poder probar
              " individualmente la actualización de los datos maestro en programas de testeo debido a la complejidad que ello supone.
              " Por ello por defecto es true para que siempre lo haga, salvo que se indica lo contrario.
              IF iv_launch_update_md = abap_true.
                CALL FUNCTION 'ZREL_LAUNCH_MASTER_DATA' IN BACKGROUND TASK
                  EXPORTING
                    iv_request_id = lt_header[ 1 ]-request_id
                    iv_langu      = mv_langu.

                INSERT zcl_ca_utilities=>fill_return( iv_id = zif_rel_data=>cs_msg-id
                                                      iv_type = zif_rel_data=>cs_msg-type_success
                                                      iv_langu = mv_langu
                                                      iv_number = '025' ) INTO TABLE et_return.

                COMMIT WORK AND WAIT. " Necesario para lanzar el proceso en background task
              ENDIF.


              " Si el siguiente status es el de pendiente de confirmar entonces envio el mail al responsable del departamento
            ELSEIF lv_next_status = zif_rel_data=>cs_wf_engine-status-pending_approv_depart.
              send_mail_approv_depart( EXPORTING is_header = lt_header[ 1 ]
                                                 iv_department_approver = iv_department_approver
                                                 iv_department_approver_reason = iv_department_approver_reason ).

              " Si el status determinado es el AP se envia el mismo mail que se hace cuando se envia
              " la aprobación.
            ELSEIF lv_next_status = zif_rel_data=>cs_wf_engine-status-pending_ap_final.
              send_mail_approve( EXPORTING iv_key = lt_header[ 1 ]-key ).

              " En este paso se envie mail solo cuando se rechaza, será cuando se actualizen los datos maestros cuando se enviará el mail de aprobación.
            ELSEIF iv_action = zif_rel_data=>cs_strategy-change_request-approvals-action_approve-reject.
              send_mail_wf_approved( EXPORTING iv_key = lt_header[ 1 ]-key
                                     IMPORTING et_return = DATA(lt_return_mail) ).
            ENDIF.

          ENDIF.

        ELSE.
          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                                   i_id         = zif_rel_data=>cs_msg-id
                                   i_number     = '009'
                                   i_langu      = mv_langu
                                   i_message_v1 = |{ iv_request_id ALPHA = OUT }| ) INTO TABLE et_return.
        ENDIF.

      ELSE.
        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                                   i_id         = zif_rel_data=>cs_msg-id
                                   i_number     = '008'
                                   i_langu      = mv_langu
                                   i_message_v1 = |{ iv_request_id ALPHA = OUT }| ) INTO TABLE et_return.

      ENDIF.

    ELSE.
      INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                           i_id         = zif_rel_data=>cs_msg-id
                           i_number     = '007'
                           i_langu      = mv_langu
                           i_message_v1 = |{ iv_request_id ALPHA = OUT }| ) INTO TABLE et_return.
    ENDIF.

  ENDMETHOD.


  METHOD update_depart_approver_wf.

    DATA(lo_wf) = NEW zcl_wfe_workflow_engine( ).

    lo_wf->update_values(
      EXPORTING
        iv_wf_id  = iv_wf_id
        it_values = VALUE #( ( field = zif_rel_data=>cs_wf_engine-field_values-depart_approver value = iv_department_approver )
                             ( field = zif_rel_data=>cs_wf_engine-field_values-depart_approver_reason value = iv_department_approver_reason ) )
      IMPORTING
        et_return = DATA(lt_return) ).

  ENDMETHOD.


  METHOD approve_step_wf.
    DATA lt_return_data TYPE zrel_bo_i_result_approv.

    CLEAR: ev_completed, ev_next_status.

    DATA(lo_params) = NEW zrel_bo_s_approv_params( langu = mv_langu
                                                   action = iv_action
                                                   reason = iv_reason ).


    mo_svc_mngr->do_action( EXPORTING iv_act_key = zif_rel_bo_strategy_c=>sc_action-root-approve_process
                                    it_key = VALUE #( ( key = is_header-key ) )
                                    is_parameters = lo_params
                          IMPORTING eo_message = DATA(lo_message)
                                    et_data = lt_return_data ).

    " Se pasan los mensajes del BOPF a la estructura de retorno
    mo_bopf_util->conv_message_bopf_2_return(
      EXPORTING
        io_message = lo_message
        iv_langu   = mv_langu
      CHANGING
        ct_return  = et_return ).

    " Si la acción devuelve error se lanza el rollback de los datos y se devuelve un mensaje indicando el error.
    READ TABLE et_return TRANSPORTING NO FIELDS WITH KEY type = zif_rel_data=>cs_msg-type_error.
    IF sy-subrc = 0.
      " Si hay error en el WF hago un rollback para que los datos se borren del BOPF
      mo_bopf_util->rollback( ).


    ELSE.
      " Miramos si el WF esta completado
      ev_completed = lt_return_data[ 1 ]-wf_completed.
      ev_next_status = lt_return_data[ 1 ]-next_status.

      " Si el workflow esta completado se hace una foto de lo que hay en el custo de SAP para luego poder comparar que se ha cambiado.
      " Se hace justo aquí para aprovechar el save de los datos del BOPF que se tiene que hacer si o si porque en la accción se actualizan
      " valores, y necesito que todo se grabe a la vez en el BOPF y evitar datos parciales.
      IF ev_completed = abap_true.
        create_sap_snapshopt( EXPORTING iv_root_key = is_header-key ).
      ENDIF.

      " Si va todo bien se graban los datos, controlando que no haya errores (que no debería porque tendría que haberlos dato
      " en el envio de los datos ).
      mo_bopf_helper->save_data_bopf( IMPORTING ev_error_save = DATA(lv_error_save) ).

    ENDIF.

  ENDMETHOD.


  METHOD launch_update_master_data.
    DATA lt_mod TYPE /bobf/t_frw_modification .
    DATA lt_header TYPE zrel_bo_i_strategy_header .
    DATA lt_params TYPE /bobf/t_frw_query_selparam.

    CLEAR: et_return.

    " Busco los datos de cabecera.
    " NOTA IRB: Este método esta pensando para poderse lanzar no solo desde la aprobación sino desde posibles
    " reprocesos por ello busco los datos de cabecera para saber si esta pendiente de modificar el datos maestro.
    " Que si se llama desde la aprobación es un poco redundante buscar de nuevo los datos pero es la gracia de los BOPF
    " que se tomará del cache.
    lt_header = mo_bopf_helper->get_header_from_key_fields( EXPORTING iv_key = iv_key iv_request_id = iv_request_id ).

    IF lt_header IS NOT INITIAL.

      " Si el status de modificacion del dato maestro esta en blanco o completado se devuelve un error.
      IF lt_header[ 1 ]-change_md_status IS INITIAL.
        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                                            i_id         = zif_rel_data=>cs_msg-id
                                            i_number     = '014'
                                            i_langu      = mv_langu
                                            i_message_v1 = |{ iv_request_id ALPHA = OUT }| ) INTO TABLE et_return.
      ELSEIF lt_header[ 1 ]-change_md_status = zif_rel_data=>cs_strategy-master_data-change_md_status-done.

        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                                          i_id         = zif_rel_data=>cs_msg-id
                                          i_number     = '013'
                                          i_langu      = mv_langu
                                          i_message_v1 = |{ iv_request_id ALPHA = OUT }| ) INTO TABLE et_return.

      ELSE.

        " Se llama a la clase CRUD que es la que realizará los cambios en el maestro de las estrategias.
        DATA(lo_md_crud) = NEW zcl_rel_strategy_md_crud( iv_langu = mv_langu ).

        " Lanzo el proceso de actualización a partir de los datos del BOPF.
        lo_md_crud->dispatch_update_master_data(
          EXPORTING
            iv_request_id = iv_request_id
            iv_key = iv_key
          IMPORTING
            et_return    = DATA(lt_return_crud)
            ev_master_data_updated = DATA(lv_master_data_updated) ).

        READ TABLE lt_return_crud TRANSPORTING NO FIELDS WITH KEY type = zif_rel_data=>cs_msg-type_error.
        IF sy-subrc = 0.
          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                                        i_id         = zif_rel_data=>cs_msg-id
                                        i_number     = '016'
                                        i_langu      = mv_langu
                                        i_message_v1 = lt_header[ 1 ]-purchase_group ) INTO TABLE et_return.


        ELSE.
          INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_success
                                          i_id         = zif_rel_data=>cs_msg-id
                                          i_number     = '017'
                                          i_langu      = mv_langu
                                          i_message_v1 = lt_header[ 1 ]-purchase_group ) INTO TABLE et_return.

          " Si los datos maestros han sido actualizados se envia el mail al solicitante
          IF lv_master_data_updated = abap_true.
            send_mail_wf_approved( EXPORTING iv_key = lt_header[ 1 ]-key
                                     IMPORTING et_return = DATA(lt_return_mail) ).
            INSERT LINES OF lt_return_mail INTO TABLE et_return.
          ENDIF.

        ENDIF.

        " Envio el resultado del proceso vía email.
        IF iv_send_result_by_mail = abap_true.
          " Parámetros de entrada
          DATA(lo_params) = NEW zrel_s_a_general_params( langu = mv_langu ).

          mo_svc_mngr->do_action( EXPORTING iv_act_key = zif_rel_bo_strategy_c=>sc_action-root-send_md_change
                                            it_key = VALUE #( ( key = lt_header[ 1 ]-key ) )
                                            is_parameters = lo_params ).
        ENDIF.

      ENDIF.

    ELSE.
      INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                           i_id         = zif_rel_data=>cs_msg-id
                           i_number     = '007'
                           i_langu      = mv_langu
                           i_message_v1 = |{ iv_request_id ALPHA = OUT }| ) INTO TABLE et_return.
    ENDIF.

  ENDMETHOD.

  METHOD get_approval_data.
    CLEAR: et_approval_data.

    " Convertimos los filtros en el formato de búsqueda del BOPF
    convert_filters_2_ranges( EXPORTING it_filter_request = it_filter_request
                              IMPORTING et_bopf_selections = DATA(lt_bopf_selparams) ).

    " Obtenemos las peticiones segun los filtros y los permisos que tenga el usuario.
    " Una vez filtrado se leerán todos los datos.
    get_request_id_of_approvers( EXPORTING it_bopf_selections = lt_bopf_selparams
                                        iv_approver = iv_approver
                              IMPORTING et_r_request_id = DATA(lt_r_request_id) ).
    IF lt_r_request_id IS NOT INITIAL.

      " Se obtienen todos los datos
      mo_bopf_helper->query_all_data_from_header( EXPORTING it_params = VALUE #( FOR <wa1> IN lt_r_request_id
                                                                 ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-request_id
                                                                   sign = <wa1>-sign
                                                                   option = <wa1>-option
                                                                   low = <wa1>-low
                                                                   high = <wa1>-high ) )
                                  IMPORTING et_header = DATA(lt_header)
                                            et_buyers = DATA(lt_buyers)
                                            et_buyers_sap = DATA(lt_buyers_sap)
                                            et_amounts = DATA(lt_amount)
                                            et_amounts_sap = DATA(lt_amount_sap)
                                            et_approvers = DATA(lt_approvers)
                                            et_approvers_sap = DATA(lt_approvers_sap)
                                            et_pgroup = DATA(lt_pgroup)
                                            et_pgroup_sap = DATA(lt_pgroup_sap)
                                            et_steps_approvers = DATA(lt_steps_approvers) ).

      " Se convierte los datos del BOPF a una estructura de datos que es lo más parecido a
      " la estructura de salida.
      convert_bopf_data_2_output( EXPORTING it_header = lt_header
                                            it_buyers = lt_buyers
                                            it_amount = lt_amount
                                            it_approvers = lt_approvers
                                            it_pgroup = lt_pgroup
                                  IMPORTING et_output_data = DATA(lt_request_data) ).

      " Sacamos los grupos de compra que estan pendientes para poder obtener los datos de SAP
      get_pgroup_sap_md( EXPORTING it_r_purchase_group = VALUE zif_rel_data=>tt_r_purchase_group( FOR <wa> IN lt_header
                                                                                                          WHERE ( request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending
                                                                                                                  OR request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending_confirmation )
                                                                                                                  ( sign = 'I'
                                                                                                                    option = 'EQ'
                                                                                                                    low = <wa>-purchase_group ) )
                                  IMPORTING et_strategies = DATA(lt_strategies_md)
                                            et_buyers = DATA(lt_buyers_md)
                                            et_pgroup_info = DATA(lt_pgroup_md) ).

      " Sacamos los datos de aprobadores del departamento del workflow
      get_depart_approver_wf( EXPORTING it_r_wf_id = VALUE #( FOR <wa> IN lt_header ( sign = 'I' option = 'EQ' low = <wa>-wf_id ) )
                              IMPORTING et_approvers = DATA(lt_depart_approvers) ).

      " Ahora recorremos los datos de cabecera y vamos rellenando los datos de salida
      LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<ls_header>).

        " Buscamos el registro formateado por la clave.
        READ TABLE lt_request_data ASSIGNING FIELD-SYMBOL(<ls_request_data>) WITH KEY key = <ls_header>-key.

        " Añadimos una linea en blanco a la tabla de salida.
        APPEND INITIAL LINE TO et_approval_data ASSIGNING FIELD-SYMBOL(<ls_approval_data>).



        " Formateamos los datos de aprobación a la estructura de salida. Este método es el mismo que se usa
        " para formatear los datos en el servicio que devuelve los datos de sap del grupo de compras + los valores solicitados para el cambio
        zcl_rel_utilities=>transfer_req_data_2_strag_data( EXPORTING is_request_data  = <ls_request_data>
                                                           CHANGING cs_strategy_data = <ls_approval_data> ).

        " Los datos de SAP que se rellenarán depende del estado de la peticion. Si esta pendiente se sacan del propio SAP. Si no lo esta se saca del modelo
        IF <ls_header>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending
           OR <ls_header>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending_confirmation.
          fill_apprv_data_sap_from_md( EXPORTING it_strategies = lt_strategies_md
                                                 it_buyers = lt_buyers_md
                                                 it_pgroup = lt_pgroup_md
                                                 iv_purchase_group = <ls_header>-purchase_group
                                       CHANGING  cs_approval_data = <ls_approval_data> ).

        ELSE.
          " Todo lo que no esta pendiente la información que había en SAP en su momento se obtendrá de lo que hay guardado en el BOPF.
          fill_apprv_data_sap_from_bo( EXPORTING iv_root_key = <ls_header>-key
                                                 it_buyers = lt_buyers_sap
                                                 it_amount = lt_amount_sap
                                                 it_approvers = lt_approvers_sap
                                                 it_pgroup = lt_pgroup_sap
                                       CHANGING cs_approval_data = <ls_approval_data> ).
        ENDIF.

        " Completamos con los datos del aprobador del partamento
        READ TABLE lt_depart_approvers ASSIGNING FIELD-SYMBOL(<ls_depart_approvers>) WITH TABLE KEY wf_id = <ls_header>-wf_id.
        IF sy-subrc = 0.
          <ls_approval_data>-department_approver = <ls_depart_approvers>-department_approver.
          <ls_approval_data>-department_approver_desc = <ls_depart_approvers>-department_approver_desc.
          <ls_approval_data>-department_approver_reason = <ls_depart_approvers>-department_approver_reason.
          <ls_approval_data>-forwarded_by = <ls_depart_approvers>-forwarded_by.
          <ls_approval_data>-forwarded_by_desc = <ls_depart_approvers>-forwarded_by_desc.
          <ls_approval_data>-forwarded_date = <ls_depart_approvers>-forwarded_date.
          <ls_approval_data>-forwarded_time = <ls_depart_approvers>-forwarded_time.
        ENDIF.

        " Solo se puede hacer el forward si esta pendiente de aprobar y no esta el forwarded_by informado. Ya que si lo
        " esta, es que o esta pendiente de confirmación o ya la ha realizado. En esos dos casos ya no se pueden reenviar.
*        <ls_approval_data>-forwarded_allow = COND #( WHEN <ls_header>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending
*                                                          AND <ls_approval_data>-forwarded_by IS INITIAL
*                                                     THEN abap_true
*                                                     ELSE abap_false ).
*                                                     <ls_approval_data>-forwarded_allow.
        " Nota Iván: El icono se fuerza que no se muestre nunca para el fase1 del arranque
        <ls_approval_data>-forwarded_allow = abap_false.

      ENDLOOP.

      SORT et_approval_data BY request_date DESCENDING.
    ENDIF.
  ENDMETHOD.


  METHOD convert_filters_2_ranges.

    CLEAR: et_r_dept_subs, et_r_purchase_group, et_r_request_by,et_r_request_date, et_r_request_id, et_r_request_status.
    CLEAR: et_bopf_selections.

    LOOP AT it_filter_request ASSIGNING FIELD-SYMBOL(<ls_filter_request>).

      CASE <ls_filter_request>-filter_id.
        WHEN  zcl_rel_filters=>mv_filter_depart_subs.
          IF <ls_filter_request>-parent_values IS NOT INITIAL.
            et_r_dept_subs = VALUE #( FOR <wa> IN <ls_filter_request>-parent_values ( sign = 'I' option = 'EQ' low = <wa>-code ) ).

            et_bopf_selections = VALUE #( BASE et_bopf_selections FOR <wa5> IN <ls_filter_request>-parent_values
                                      ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-dept_subs
                                        sign = 'I'
                                        option = 'EQ'
                                        low = <wa5>-code ) ).
          ENDIF.
        WHEN  zcl_rel_filters=>mv_filter_purchase_group.
          et_r_purchase_group = VALUE #( FOR <wa1> IN <ls_filter_request>-parent_values ( sign = 'I' option = 'EQ' low = <wa1>-code ) ).

          et_bopf_selections = VALUE #( BASE et_bopf_selections FOR <wa6> IN <ls_filter_request>-parent_values
                          ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-purchase_group
                            sign = 'I'
                            option = 'EQ'
                            low = <wa6>-code ) ).

        WHEN  zcl_rel_filters=>mv_filter_requester.
          et_r_request_by = VALUE #( FOR <wa2> IN <ls_filter_request>-parent_values ( sign = 'I' option = 'EQ' low = <wa2>-code ) ).

          et_bopf_selections = VALUE #( BASE et_bopf_selections FOR <wa7> IN <ls_filter_request>-parent_values
                          ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-request_by
                            sign = 'I'
                            option = 'EQ'
                            low = <wa7>-code ) ).

        WHEN  zcl_rel_filters=>mv_filter_req_date.
          et_r_request_date = VALUE #( FOR <wa3> IN <ls_filter_request>-parent_values ( sign = 'I' option = 'EQ' low = <wa3>-code ) ).

          et_bopf_selections = VALUE #( BASE et_bopf_selections FOR <wa8> IN <ls_filter_request>-parent_values
                          ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-request_date
                            sign = 'I'
                            option = 'EQ'
                            low = <wa8>-code ) ).

        WHEN  zcl_rel_filters=>mv_filter_req_id.
          et_r_request_id = VALUE #( FOR <wa3> IN <ls_filter_request>-parent_values ( sign = 'I' option = 'EQ' low = |{ <wa3>-code ALPHA = IN }| ) ).

          et_bopf_selections = VALUE #( BASE et_bopf_selections FOR <wa9> IN <ls_filter_request>-parent_values
                   ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-request_id
                     sign = 'I'
                     option = 'EQ'
                     low = |{ <wa9>-code ALPHA = IN }| ) ).

        WHEN  zcl_rel_filters=>mv_filter_status.
          et_r_request_status = VALUE #( FOR <wa4> IN <ls_filter_request>-parent_values ( sign = 'I' option = 'EQ' low = <wa4>-code ) ).

          et_bopf_selections = VALUE #( BASE et_bopf_selections FOR <wa10> IN <ls_filter_request>-parent_values
                   ( attribute_name = zif_rel_bo_strategy_c=>sc_query_attribute-root-select_by_elements-request_status
                     sign = 'I'
                     option = 'EQ'
                     low = <wa10>-code ) ).
      ENDCASE.

    ENDLOOP.

  ENDMETHOD.


  METHOD get_pgroup_sap_md.

    CLEAR: et_strategies, et_buyers.

    IF it_r_purchase_group IS NOT INITIAL.

      DATA(lo_md) = NEW zcl_rel_strategy_md_query( iv_langu = mv_langu ).

      lo_md->get_strate_data_from_pgroup(
        EXPORTING
          it_r_purchase_group = it_r_purchase_group
        IMPORTING
          et_strategy_data    = et_strategies ).

      lo_md->get_purchase_group_buyer( EXPORTING it_r_purchase_group = it_r_purchase_group
                                                   IMPORTING et_buyer_purchase_group = et_buyers ).

      lo_md->get_pgroup_info( EXPORTING it_r_purchase_group = it_r_purchase_group
                              IMPORTING et_info = et_pgroup_info ).

    ENDIF.

  ENDMETHOD.


  METHOD create_sap_snapshopt.

    " El proceso de foto no devuelve errores. Simplemente graba lo que hay en SAP.
    mo_svc_mngr->do_action( EXPORTING iv_act_key = zif_rel_bo_strategy_c=>sc_action-root-sap_snapshot
                                    it_key = VALUE #( ( key = iv_root_key ) )
                          IMPORTING eo_message = DATA(lo_message) ).

    IF iv_commit = abap_true.
      mo_bopf_helper->save_data_bopf(  ).
    ENDIF.

  ENDMETHOD.


  METHOD fill_apprv_data_sap_from_md.

    " Datos de las estrategias
    READ TABLE it_strategies ASSIGNING FIELD-SYMBOL(<ls_strategies>) WITH KEY purchase_group = iv_purchase_group.
    IF sy-subrc = 0.

      LOOP AT <ls_strategies>-strategies ASSIGNING FIELD-SYMBOL(<ls_amount_md>).
        APPEND INITIAL LINE TO cs_approval_data-strategies ASSIGNING FIELD-SYMBOL(<ls_amount>).
        zcl_ca_utilities=>clone_structure_values( EXPORTING is_source = <ls_amount_md>
                                                          iv_ignore_itab = abap_false
                                                CHANGING cs_destiny = <ls_amount> ).
      ENDLOOP.
    ENDIF.

    " Compradores
    cs_approval_data-buyers =  VALUE #( FOR <wa1> IN it_buyers WHERE ( purchase_group = iv_purchase_group ) ( CORRESPONDING #( <wa1> )  ) ).


    " Información del grupo de compras
    READ TABLE it_pgroup ASSIGNING FIELD-SYMBOL(<ls_pgroup_info>) WITH KEY purchase_group = iv_purchase_group.
    IF sy-subrc = 0.
      cs_approval_data = CORRESPONDING #( BASE ( cs_approval_data ) <ls_pgroup_info> ).
    ENDIF.
  ENDMETHOD.


  METHOD fill_apprv_data_sap_from_bo.

    " leemos los importes cuyo clave padre es la pasada por parámetro
    LOOP AT it_amount ASSIGNING FIELD-SYMBOL(<ls_amount>) WHERE parent_key = iv_root_key.

      INSERT CORRESPONDING #( <ls_amount> ) INTO TABLE cs_approval_data-strategies ASSIGNING FIELD-SYMBOL(<ls_strategies>).

      " Ahora sacamos los aprobadores que tengan el mismo padre y grupo y estrategia de liberación
      LOOP AT it_approvers ASSIGNING FIELD-SYMBOL(<ls_approvers>) WHERE parent_key = iv_root_key
                                                                        AND group = <ls_amount>-group
                                                                        AND strategy = <ls_amount>-strategy.
        INSERT CORRESPONDING #( <ls_approvers> ) INTO TABLE <ls_strategies>-approvers.

      ENDLOOP.
    ENDLOOP.

    " Compradores
    cs_approval_data-buyers =  VALUE #( FOR <wa1> IN it_buyers WHERE ( parent_key = iv_root_key ) ( CORRESPONDING #( <wa1> )  ) ).

    " Información del grupo de compras
    READ TABLE it_pgroup ASSIGNING FIELD-SYMBOL(<ls_pgroup_info>) WITH KEY parent_key = iv_root_key.
    IF sy-subrc = 0.
      cs_approval_data = CORRESPONDING #( BASE ( cs_approval_data ) <ls_pgroup_info> ).
    ENDIF.

  ENDMETHOD.


  METHOD get_request_id_of_approvers.
    DATA lt_header TYPE zrel_bo_i_strategy_header.

    CLEAR: et_r_request_id.
    DATA(lo_wfe_query) = NEW zcl_wfe_model_data_query( iv_langu = mv_langu ).

    mo_svc_mngr->query( EXPORTING iv_query_key = zif_rel_bo_strategy_c=>sc_query-root-select_by_elements
                                      it_selection_parameters = it_bopf_selections
                                      iv_fill_data = abap_true
                          IMPORTING et_data = lt_header ).

    IF lt_header IS NOT INITIAL.
      " Se instancia la clase que hará el control de autorizaciones
      DATA(lo_auth) = NEW zcl_rel_user_authorizations( iv_user = iv_approver ).

      LOOP AT lt_header ASSIGNING FIELD-SYMBOL(<ls_header>).
        DATA(lv_tabix) = sy-tabix.
        " La primera validación es mirar si el usuario es aprobador. Si lo es el registro
        " se incluye
        DATA(lv_have_auth) = abap_false.
        IF lo_auth->authority_check( iv_actvt = zif_rel_auth_data=>cs_actvt_auth_check-approver
                                     iv_dept_subs = <ls_header>-dept_subs ).
          lv_have_auth = abap_true.
        ELSE.
          " Si no tiene autorización veamos si el status esta pendiente de confirmación, completado o rechazado se mira
          " si el aprobador pasado por parámetro forma, o ha formado, parte como aprobador. Si es asi también se incluye el registro.
          IF <ls_header>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending_confirmation
             OR <ls_header>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-approved
             OR <ls_header>-request_status = zif_rel_data=>cs_strategy-change_request-approvals-request_status-rejected.

            lo_wfe_query->get_steps_approvers_data( EXPORTING it_params_sl = VALUE #( ( selname = zif_wfe_data=>cs_model_data-steps_approvers-fields-wf_id
                                                                                        kind = 'P'
                                                                                        option = 'EQ'
                                                                                        low = <ls_header>-wf_id )
                                                                                       ( selname = zif_wfe_data=>cs_model_data-steps_approvers-fields-approver
                                                                                        kind = 'P'
                                                                                        option = 'EQ'
                                                                                        low = iv_approver ) )
                                                    IMPORTING et_steps_approvers = DATA(lt_steps_approvers) ).

            IF lt_steps_approvers IS NOT INITIAL.
              lv_have_auth = abap_true.
            ENDIF.
          ENDIF.
        ENDIF.

        IF lv_have_auth = abap_true.
          INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_header>-request_id ) INTO TABLE et_r_request_id.
        ENDIF.
      ENDLOOP.
    ENDIF.
  ENDMETHOD.


  METHOD send_mail_wf_approved.

    " Parámetros de entrada
    DATA(lo_params) = NEW zrel_s_a_general_params( langu = mv_langu ).

    " Aquí la respuesta se ignora porque solo es el envio del mail.
    mo_svc_mngr->do_action( EXPORTING iv_act_key = zif_rel_bo_strategy_c=>sc_action-root-send_mail_approved
                                  it_key = VALUE #( ( key = iv_key ) )
                                  is_parameters = lo_params
                        IMPORTING eo_message = DATA(lo_message) ).

  ENDMETHOD.

  METHOD get_depart_approver_wf.

    CLEAR: et_approvers.

    DATA(lo_wf) = NEW zcl_wfe_model_data_query( iv_langu = mv_langu ).

    " Valores del workflow
    lo_wf->get_values_data( EXPORTING it_params_sl = VALUE #( FOR <wa> IN it_r_wf_id ( selname = zif_wfe_data=>cs_model_data-values-fields-wf_id
                                                                kind = <wa>-sign
                                                                option = <wa>-option
                                                                low = <wa>-low  ) )
                            IMPORTING et_values = DATA(lt_values) ).

    " Pasos del workflow para saber quien ha enviado el
    lo_wf->get_steps_data( EXPORTING it_params_sl = VALUE #( FOR <wa> IN it_r_wf_id ( selname = zif_wfe_data=>cs_model_data-steps-fields-wf_id
                                                                                      kind = <wa>-sign
                                                                                      option = <wa>-option
                                                                                      low = <wa>-low  ) )
                           IMPORTING et_steps     = DATA(lt_steps) ).

    LOOP AT lt_values ASSIGNING FIELD-SYMBOL(<ls_values_dummy>)
                      GROUP BY ( wf_id = <ls_values_dummy>-wf_id )
                      ASSIGNING FIELD-SYMBOL(<group>).

      DATA(ls_approver) = VALUE ts_depart_approver( wf_id = <group>-wf_id ).
      LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<ls_values>) WHERE ( field = zif_rel_data=>cs_wf_engine-field_values-depart_approver
                                                                        OR field =  zif_rel_data=>cs_wf_engine-field_values-depart_approver_reason )   .
        CASE <ls_values>-field.
          WHEN zif_rel_data=>cs_wf_engine-field_values-depart_approver.
            ls_approver-department_approver = <ls_values>-value.
          WHEN zif_rel_data=>cs_wf_engine-field_values-depart_approver_reason.
            ls_approver-department_approver_reason = <ls_values>-value.
        ENDCASE.
      ENDLOOP.
      IF sy-subrc = 0. " Si hay registro es que se hay reenviado al responsable del departamento

        " Sacamos quien le ha hecho el forward y cuando.
        READ TABLE lt_steps ASSIGNING FIELD-SYMBOL(<ls_steps>)
                            WITH TABLE KEY wf_id = <group>-wf_id
                                           status = zif_rel_data=>cs_wf_engine-status-pending_ap.
        IF sy-subrc = 0.
          ls_approver-forwarded_time = <ls_steps>-aetim.
          ls_approver-forwarded_by = <ls_steps>-approved_by.
          ls_approver-forwarded_by_desc = mo_md_query->get_username_desc( ls_approver-forwarded_by ).
          ls_approver-forwarded_by_desc = COND #( WHEN ls_approver-forwarded_by_desc IS INITIAL
                                                       THEN ls_approver-forwarded_by_desc
                                                       ELSE ls_approver-forwarded_by_desc ).
          ls_approver-forwarded_date = <ls_steps>-aedat.
        ENDIF.

        " Descripción del aprobador
        ls_approver-department_approver_desc = mo_md_query->get_username_desc( ls_approver-department_approver ).
        ls_approver-department_approver_desc = COND #( WHEN ls_approver-department_approver_desc IS INITIAL
                                                       THEN ls_approver-department_approver
                                                       ELSE ls_approver-department_approver_desc ).


        INSERT ls_approver INTO TABLE et_approvers.
      ENDIF.


    ENDLOOP.



  ENDMETHOD.


  METHOD send_mail_approv_depart.

    " Parámetros de entrada
    DATA(lo_params) = NEW zrel_bo_s_approv_depart( langu = mv_langu
                                                   depart_approver = iv_department_approver
                                                   depart_approver_reason = iv_department_approver_reason ).

    mo_svc_mngr->do_action( EXPORTING iv_act_key = zif_rel_bo_strategy_c=>sc_action-root-send_mail_depart_approv
                                  it_key = VALUE #( ( key = is_header-key ) )
                                  is_parameters = lo_params
                        IMPORTING eo_message = DATA(lo_message) ).


    " Se pasan los mensajes del BOPF a la estructura de retorno
    mo_bopf_util->conv_message_bopf_2_return(
      EXPORTING
        io_message = lo_message
        iv_langu   = mv_langu
      CHANGING
        ct_return  = et_return ).

  ENDMETHOD.


  METHOD send_mail_approve.

    DATA(lo_params) = NEW zrel_s_a_general_params( langu = mv_langu ).
    mo_svc_mngr->do_action( EXPORTING iv_act_key = zif_rel_bo_strategy_c=>sc_action-root-send_mail_approve
                                      it_key = VALUE #( ( key = iv_key ) )
                                      is_parameters = lo_params ).

  ENDMETHOD.

ENDCLASS.
