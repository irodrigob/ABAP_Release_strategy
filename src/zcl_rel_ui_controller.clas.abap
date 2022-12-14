CLASS zcl_rel_ui_controller DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES:
      BEGIN OF ts_depart_subs,
        dept_subs      TYPE zrel_e_depart_subsidiary,
        dept_subs_desc TYPE zrel_e_depart_subsidiary_desc,
        can_edit       TYPE sap_bool,
      END OF ts_depart_subs .
    TYPES:
      tt_depart_subs TYPE STANDARD TABLE OF ts_depart_subs WITH EMPTY KEY .
    TYPES: tt_filter_buyers TYPE STANDARD TABLE OF syuname WITH EMPTY KEY.
    TYPES: tt_filter_approvers TYPE STANDARD TABLE OF syuname WITH EMPTY KEY.
    TYPES: tt_filter_purchase_group TYPE STANDARD TABLE OF ekgrp WITH EMPTY KEY.
    TYPES: tt_filter_liberation_group TYPE STANDARD TABLE OF frggr WITH EMPTY KEY.
    TYPES: tt_filter_liberation_code TYPE STANDARD TABLE OF frgco WITH EMPTY KEY.


    "! <p class="shorttext synchronized">Devuelve la información del usuario</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter user_info | <p class="shorttext synchronized">Información del usuario</p>
    "! @parameter return | <p class="shorttext synchronized">Mensaje del proceso</p>
    CLASS-METHODS get_user_info
      IMPORTING
        !langu     TYPE sy-langu
        !user      TYPE syuname OPTIONAL
      EXPORTING
        !user_info TYPE zcl_rel_user=>ts_user_info
        !return    TYPE bapiret2_t .
    "! <p class="shorttext synchronized">Devuelve los departamento del usuario</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter depart_subs | <p class="shorttext synchronized">Departamento/usuario</p>
    CLASS-METHODS get_user_depart_subs
      IMPORTING
        !langu       TYPE sy-langu DEFAULT sy-langu
        !user        TYPE syuname OPTIONAL
      EXPORTING
        !depart_subs TYPE tt_depart_subs .
    "! <p class="shorttext synchronized">Devuelve los departamento del usuario</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter depart_subs | <p class="shorttext synchronized">Departamento/usuario</p>
    "! @parameter strategy_data | <p class="shorttext synchronized">Datos de la estrategia</p>
    CLASS-METHODS get_strategy_data_from_pgroup
      IMPORTING
        !langu         TYPE sy-langu DEFAULT sy-langu
        !depart_subs   TYPE zrel_e_depart_subsidiary
      EXPORTING
        !strategy_data TYPE zif_rel_data=>tt_pgroup_all_data .
    "! <p class="shorttext synchronized">Devuelve los usuarios de SAP</p>
    "! Estos usuarios se usarán para escoger compradores o aprobadores de estrategias.
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter users | <p class="shorttext synchronized">Usuarios</p>
    METHODS get_system_users
      IMPORTING
        !langu TYPE sy-langu DEFAULT sy-langu
      EXPORTING
        !users TYPE zif_rel_data=>tt_users .
    "! <p class="shorttext synchronized">Nueva solicitud de cambio</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter request_data | <p class="shorttext synchronized">Datos</p>
    "! @parameter return | <p class="shorttext synchronized">Mensaje del proceso</p>
    "! @parameter request_id | <p class="shorttext synchronized">ID de petición</p>
    CLASS-METHODS new_request_change
      IMPORTING
        !langu        TYPE sy-langu DEFAULT sy-langu
        !request_data TYPE zif_rel_data=>ts_pgroup_request_change
      EXPORTING
        !return       TYPE bapiret2_t
        !request_id   TYPE zrel_e_request_id .
    "! <p class="shorttext synchronized">Aprobación de solicitud</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter request_id | <p class="shorttext synchronized">ID de petición</p>
    "! @parameter action | <p class="shorttext synchronized">Acción</p>
    "! @parameter reason | <p class="shorttext synchronized">Motivo de la aprobación</p>
    "! @parameter return | <p class="shorttext synchronized">Mensaje del proceso</p>
    CLASS-METHODS approve_request
      IMPORTING
        !langu                      TYPE sy-langu DEFAULT sy-langu
        !request_id                 TYPE zrel_e_request_id
        !action                     TYPE zrel_e_action_approv
        !reason                     TYPE zrel_e_approval_reason OPTIONAL
        !department_approver        TYPE syuname OPTIONAL
        !department_approver_reason TYPE string OPTIONAL
      EXPORTING
        !return                     TYPE bapiret2_t .
    "! <p class="shorttext synchronized">Obtiene los tipos de filtros configuración</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter filters | <p class="shorttext synchronized">Filtros</p>
    METHODS get_filters_type
      IMPORTING
        !langu   TYPE sy-langu DEFAULT sy-langu
        !user    TYPE syuname OPTIONAL
      EXPORTING
        !filters TYPE zcl_ca_manag_filters=>tt_filter_conf_ext .
    "! <p class="shorttext synchronized">Obtiene los datos de los filtros pasado por parámetro</p>
    "! @parameter it_filters_request | <p class="shorttext synchronized">Filtros solicitados</p>
    "! @parameter et_filters | <p class="shorttext synchronized">Datos de los filtros</p>
    METHODS get_filters_data
      IMPORTING
        !it_filters_request TYPE zcl_ca_manag_filters=>tt_data_request
        !user               TYPE syuname OPTIONAL
        !langu              TYPE sy-langu DEFAULT sy-langu
      EXPORTING
        !et_filters_data    TYPE zcl_ca_manag_filters=>tt_data_response .
    "! <p class="shorttext synchronized">Obtiene los datos de aprobacion</p>
    "! @parameter user | <p class="shorttext synchronized">Usuarios</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter filter_request | <p class="shorttext synchronized">Filtros para la busqueda</p>
    "! @parameter approval_data | <p class="shorttext synchronized">Datos de la aprobacion</p>
    METHODS get_approval_data
      IMPORTING
        !user           TYPE syuname DEFAULT sy-uname
        !langu          TYPE sylangu DEFAULT sy-langu
        !filter_request TYPE zcl_int_ui_contlr_docs=>tt_request_data
      EXPORTING
        approval_data   TYPE zif_rel_data=>tt_pgroup_all_data.
    "! <p class="shorttext synchronized">Lanza el proceso de actualización de datos maestros</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter request_id | <p class="shorttext synchronized">ID de la solicitud</p>
    "! @parameter return | <p class="shorttext synchronized">Resultado del proceso</p>
    METHODS launch_update_master_data
      IMPORTING !langu      TYPE sy-langu DEFAULT sy-langu
                !request_id TYPE zrel_t010-request_id
      EXPORTING
                return      TYPE bapiret2_t.
    "! <p class="shorttext synchronized">Obtiene el listado de grupos de liberación</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter list | <p class="shorttext synchronized">Listado</p>
    METHODS get_liberation_group_list
      IMPORTING !langu TYPE sy-langu DEFAULT sy-langu
      EXPORTING list   TYPE zcl_rel_strategy_md_query=>tt_liberation_group_list.
    "! <p class="shorttext synchronized">Obtiene el listado de grupos de compra</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter list | <p class="shorttext synchronized">Listado</p>
    METHODS get_purchase_group_list
      IMPORTING !langu TYPE sy-langu DEFAULT sy-langu
      EXPORTING list   TYPE zcl_rel_strategy_md_query=>tt_purchase_group_list.
    "! <p class="shorttext synchronized">Obtiene el listado de códigos de liberación</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter list | <p class="shorttext synchronized">Listado</p>
    METHODS get_code_liberation_list
      IMPORTING !langu TYPE sy-langu DEFAULT sy-langu
      EXPORTING list   TYPE zcl_rel_strategy_md_query=>tt_code_liberation_list.
    "! <p class="shorttext synchronized">Búsqueda de estrategias por multiples campos</p>
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter buyers | <p class="shorttext synchronized">Compradores</p>
    "! @parameter approvers | <p class="shorttext synchronized">Aprobadores</p>
    "! @parameter purchase_group | <p class="shorttext synchronized">Grupos de compra</p>
    "! @parameter liberation_code | <p class="shorttext synchronized">Códigos de liberación</p>
    METHODS search_multiple_values
      IMPORTING langu            TYPE sy-langu DEFAULT sy-langu
                buyers           TYPE tt_filter_buyers OPTIONAL
                approvers        TYPE tt_filter_approvers OPTIONAL
                purchase_group   TYPE tt_filter_purchase_group OPTIONAL
                liberation_group_code   TYPE zcl_rel_strategy_md_query=>tt_filter_liberation_code OPTIONAL
                liberation_group TYPE tt_filter_liberation_group OPTIONAL
                liberation_code  TYPE tt_filter_liberation_code OPTIONAL
      EXPORTING
                !strategy_data   TYPE zif_rel_data=>tt_pgroup_all_data .

  PROTECTED SECTION.
    "! <p class="shorttext synchronized">Transfiere la solicitud de strategia al modelo de estrategia</p>
    "! Este metodo transfiere el modelo de estrategias de una solicitud de cambio al modelo de salida
    "! de estrategias
    "! @parameter langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter request_data | <p class="shorttext synchronized">Datos</p>
    "! @parameter return | <p class="shorttext synchronized">Retorno del proceso</p>
    "! @parameter request_id | <p class="shorttext synchronized">ID de petición</p>
    CLASS-METHODS transfer_req_data_2_strag_data
      IMPORTING
        is_request_data  TYPE zcl_rel_strategy_chnge_request=>ts_pgroup_request_data
      CHANGING
        cs_strategy_data TYPE zif_rel_data=>ts_pgroup_all_data.
  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_ui_controller IMPLEMENTATION.


  METHOD approve_request.

    DATA(lo_request_change) = NEW zcl_rel_strategy_chnge_request( iv_langu = langu ).

    lo_request_change->approve_request(
      EXPORTING
        iv_action = action
        iv_reason = reason
        iv_request_id = request_id
        iv_department_approver = department_approver
        iv_department_approver_reason = department_approver_reason
      IMPORTING
        et_return       = return ).

  ENDMETHOD.


  METHOD get_approval_data.

    DATA(lo_request_change) = NEW zcl_rel_strategy_chnge_request( iv_langu = langu ).

    lo_request_change->get_approval_data(
      EXPORTING
        it_filter_request = filter_request
        iv_approver = COND #( WHEN user IS INITIAL THEN sy-uname ELSE user  )
      IMPORTING
        et_approval_data  = approval_data ).

  ENDMETHOD.


  METHOD get_filters_data.
    DATA(lo_filters) = NEW zcl_rel_filters( iv_langu = langu ).

    lo_filters->get_filters_data( EXPORTING it_filters_request = it_filters_request
                                  IMPORTING et_filters_data = et_filters_data ).

  ENDMETHOD.


  METHOD get_filters_type.

    DATA(lo_filters) = NEW zcl_rel_filters( iv_langu = langu ).

    lo_filters->get_filters_config(
      IMPORTING
        et_filters = filters ).

  ENDMETHOD.


  METHOD get_strategy_data_from_pgroup.

    DATA(lo_master_data) = NEW zcl_rel_strategy_md_query( iv_langu = langu ).
    DATA(lo_request_data) = NEW zcl_rel_strategy_chnge_request( iv_langu = langu ).

    IF depart_subs IS NOT INITIAL.
      " Sacamos los grupos de comprados asociados al departamento
      lo_master_data->get_pgroup_from_dept_subs(
        EXPORTING
          iv_dept_subs      = depart_subs
        IMPORTING
          et_purchase_group = DATA(lt_purchase_group) ).

      IF lt_purchase_group IS NOT INITIAL.

        " Compradores de los grupos
        lo_master_data->get_purchase_group_buyer(
          EXPORTING
            it_r_purchase_group = VALUE #( FOR <wa> IN lt_purchase_group ( sign = 'I' option = 'EQ' low = <wa>-purchase_group  ) )
          IMPORTING
            et_buyer_purchase_group   = DATA(lt_buyer_purchase_group) ).

        " Datos de las estrategias
        lo_master_data->get_strate_data_from_pgroup(
          EXPORTING
            it_r_purchase_group = VALUE #( FOR <wa> IN lt_purchase_group ( sign = 'I' option = 'EQ' low = <wa>-purchase_group ) )
          IMPORTING
            et_strategy_data    = DATA(lt_strategy_md_data) ).

        " Datos de las peticiones de cambios pendientes de aprobar para los grupos de compra
        lo_request_data->get_pgroup_request_data( EXPORTING it_r_pgroup     = VALUE #( FOR <wa> IN lt_purchase_group ( sign = 'I' option = 'EQ' low = <wa>-purchase_group ) )
                                                            it_r_status = VALUE #( ( sign = 'I' option = 'EQ' low = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending ) )
                                                   IMPORTING et_request_data = DATA(lt_request_data) ).

        LOOP AT lt_purchase_group ASSIGNING FIELD-SYMBOL(<ls_purchase_group>).
          INSERT CORRESPONDING #( <ls_purchase_group> ) INTO TABLE strategy_data ASSIGNING FIELD-SYMBOL(<ls_strategy_data>).

          " Compradores actuales
          <ls_strategy_data>-buyers = VALUE #( FOR <wa1> IN lt_buyer_purchase_group
                                               WHERE ( purchase_group = <ls_purchase_group>-purchase_group )
                                                     ( username = <wa1>-username
                                                       username_desc = <wa1>-username_desc ) ).
          READ TABLE lt_strategy_md_data ASSIGNING FIELD-SYMBOL(<ls_strategy_md_data>)
                                      WITH KEY purchase_group = <ls_purchase_group>-purchase_group.
          IF sy-subrc = 0.
            <ls_strategy_data>-strategies = <ls_strategy_md_data>-strategies.
          ENDIF.

          " Se informa los datos solicitados, si los hubiese.
          READ TABLE lt_request_data ASSIGNING FIELD-SYMBOL(<ls_request_data>) WITH KEY purchase_group = <ls_purchase_group>-purchase_group.
          IF sy-subrc = 0.
            " Debido a los campos complejos no es posible hacer un corresponding.
            zcl_rel_utilities=>transfer_req_data_2_strag_data( EXPORTING is_request_data = <ls_request_data>
                                            CHANGING cs_strategy_data = <ls_strategy_data> ).
          ENDIF.


        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.


  METHOD get_system_users.

    NEW zcl_rel_general_master_data( iv_langu = langu )->get_system_users( IMPORTING et_users = users ).

  ENDMETHOD.


  METHOD get_user_depart_subs.

    CLEAR: depart_subs.
    " Si no hay usuario informado manda el de sistema. En caso contrario el del parámetro.
    IF user IS INITIAL.
      DATA(lv_user) = CONV syuname( sy-uname ).
    ELSE.
      lv_user = user.
    ENDIF.

    DATA(lo_master_data) = NEW zcl_rel_strategy_md_query( iv_langu = langu ).

    lo_master_data->get_depart_subs_from_users( EXPORTING iv_user = lv_user
                                           IMPORTING et_dept_subs = DATA(lt_dept_subs) ).

    " El servicio permite varios usuarios con lo cual la tabla de salida tiene usuario y nombre. A nosotros nos interesa
    " solo el departamento y su descripción.
    depart_subs = CORRESPONDING #( lt_dept_subs ).
    SORT depart_subs BY dept_subs.
    DELETE ADJACENT DUPLICATES FROM depart_subs COMPARING dept_subs.

  ENDMETHOD.


  METHOD get_user_info.

    TRY.
        " Si no hay usuario informado manda el de sistema. En caso contrario el del parámetro.
        IF user IS INITIAL.
          DATA(lv_user) = CONV syuname( sy-uname ).
        ELSE.
          lv_user = user.
        ENDIF.

        DATA(lo_user) = NEW zcl_rel_user(  iv_user  = lv_user  iv_langu = langu ).

        lo_user->get_user_info(
          IMPORTING
            es_user_info = user_info ).

      CATCH zcx_rel INTO DATA(lx_data).

        INSERT zcl_ca_utilidades=>fill_return( i_type       = zif_rel_data=>cs_msg-type_error
                                               i_id         = lx_data->if_t100_message~t100key-msgid
                                               i_number     = lx_data->if_t100_message~t100key-msgno
                                               i_message_v1 = lx_data->mv_msgv1
                                               i_message_v2 = lx_data->mv_msgv2
                                               i_message_v3 = lx_data->mv_msgv3
                                               i_message_v4 = lx_data->mv_msgv4
                                               i_langu      = langu ) INTO TABLE return.
    ENDTRY.
  ENDMETHOD.


  METHOD new_request_change.

    DATA(lo_request_change) = NEW zcl_rel_strategy_chnge_request( iv_langu = langu ).

    lo_request_change->new_request(
      EXPORTING
        is_request_data = request_data
      IMPORTING
        et_return       = return
        ev_request_id   = request_id ).
  ENDMETHOD.


  METHOD transfer_req_data_2_strag_data.

    " Copiamos los datos base pero ignoramos los campos que son estructuras. El motivo es que
    " en ambos sitios se llaman igual pero en los datos de strategia tienen que ir a un campo distinto
    zcl_ca_utilities=>clone_structure_values( EXPORTING is_source = is_request_data
                                                        iv_ignore_itab = abap_true
                                              CHANGING cs_destiny = cs_strategy_data ).

    " Clonamos el campo de compradores
    zcl_ca_utilities=>clone_table_values( EXPORTING it_source = is_request_data-buyers
                                              CHANGING ct_destiny = cs_strategy_data-buyers_requested ).

    " Clonamos los campos de estrategias. En ese caso si que nos interesa que se clonan los campos que son
    " tablas internas (los aprobadores) porque son propios de la petición de cambio
    zcl_ca_utilities=>clone_table_values( EXPORTING it_source = is_request_data-strategies
                                              CHANGING ct_destiny = cs_strategy_data-strategies_requested ).

    " Los datos propios del maestro de grupo de compras, actualmente el texto se informan a mano.
    cs_strategy_data-purchase_group_requested_desc = is_request_data-pgroup-purchase_group_desc.
    cs_strategy_data-cdchngind = is_request_data-pgroup-cdchngind.

  ENDMETHOD.
  METHOD launch_update_master_data.
    DATA(lo_change) = NEW zcl_rel_strategy_chnge_request( iv_langu = langu ).

    lo_change->launch_update_master_data(
      EXPORTING
        iv_request_id = request_id
      IMPORTING
        et_return     = return ).

  ENDMETHOD.

  METHOD get_liberation_group_list.

    list = NEW zcl_rel_strategy_md_query( iv_langu = langu )->get_liberation_group_list(  ).

  ENDMETHOD.

  METHOD get_purchase_group_list.
    list = NEW zcl_rel_strategy_md_query( iv_langu = langu )->get_purchase_group_list(  ).
  ENDMETHOD.

  METHOD get_code_liberation_list.
    list = NEW zcl_rel_strategy_md_query( iv_langu = langu )->get_code_liberation_list(  ).
  ENDMETHOD.

  METHOD search_multiple_values.

    DATA(lo_md_query) = NEW zcl_rel_strategy_md_query( iv_langu = langu ).
    DATA(lo_request_data) = NEW zcl_rel_strategy_chnge_request( iv_langu = langu ).


    DATA(lt_r_buyers) = VALUE zif_rel_data=>tt_r_username( FOR <wa> IN buyers ( sign = 'I' option = 'EQ' low = <wa> ) ).
    DATA(lt_r_approvers) = VALUE zif_rel_data=>tt_r_username( FOR <wa1> IN approvers ( sign = 'I' option = 'EQ' low = <wa1> ) ).
    DATA(lt_r_purchase_group) = VALUE zif_rel_data=>tt_r_purchase_group( FOR <wa2> IN purchase_group ( sign = 'I' option = 'EQ' low = <wa2> ) ).
    DATA(lt_r_liberation_group) = VALUE zif_rel_data=>tt_r_liberation_group( FOR <wa5> IN liberation_group ( sign = 'I' option = 'EQ' low = <wa5> ) ).
    DATA(lt_r_liberation_code) = VALUE zif_rel_data=>tt_r_liberation_code( FOR <wa6> IN liberation_code ( sign = 'I' option = 'EQ' low = <wa6> ) ).

    lo_md_query->search_multiple_values(
      EXPORTING
        it_r_buyers         = lt_r_buyers
        it_r_approvers      = lt_r_approvers
        it_r_purchase_group = lt_r_purchase_group
        it_group_code_lib = liberation_group_code
        it_r_group = lt_r_liberation_group
        it_r_code = lt_r_liberation_code
      IMPORTING
        et_strategy_data    = DATA(lt_strategy_md_data)
        et_buyer_purchase_group = DATA(lt_buyer_purchase_group)
        et_purchase_group = DATA(lt_purchase_group) ).

    IF lt_purchase_group IS NOT INITIAL.

      " Datos de las peticiones de cambios pendientes de aprobar para los grupos de compra
      lo_request_data->get_pgroup_request_data( EXPORTING it_r_pgroup     = VALUE #( FOR <wa3> IN lt_purchase_group ( sign = 'I' option = 'EQ' low = <wa3>-purchase_group ) )
                                                          it_r_status = VALUE #( ( sign = 'I' option = 'EQ' low = zif_rel_data=>cs_strategy-change_request-approvals-request_status-pending ) )
                                                 IMPORTING et_request_data = DATA(lt_request_data) ).

      LOOP AT lt_purchase_group ASSIGNING FIELD-SYMBOL(<ls_purchase_group>).
        INSERT CORRESPONDING #( <ls_purchase_group> ) INTO TABLE strategy_data ASSIGNING FIELD-SYMBOL(<ls_strategy_data>).

        " Compradores actuales
        <ls_strategy_data>-buyers = VALUE #( FOR <wa4> IN lt_buyer_purchase_group
                                             WHERE ( purchase_group = <ls_purchase_group>-purchase_group )
                                                   ( username = <wa4>-username
                                                     username_desc = <wa4>-username_desc ) ).
        READ TABLE lt_strategy_md_data ASSIGNING FIELD-SYMBOL(<ls_strategy_md_data>)
                                    WITH KEY purchase_group = <ls_purchase_group>-purchase_group.
        IF sy-subrc = 0.
          <ls_strategy_data>-strategies = <ls_strategy_md_data>-strategies.
        ENDIF.

        " Se informa los datos solicitados, si los hubiese.
        READ TABLE lt_request_data ASSIGNING FIELD-SYMBOL(<ls_request_data>) WITH KEY purchase_group = <ls_purchase_group>-purchase_group.
        IF sy-subrc = 0.
          " Debido a los campos complejos no es posible hacer un corresponding.
          zcl_rel_utilities=>transfer_req_data_2_strag_data( EXPORTING is_request_data = <ls_request_data>
                                          CHANGING cs_strategy_data = <ls_strategy_data> ).
        ENDIF.


      ENDLOOP.

    ENDIF.

  ENDMETHOD.

ENDCLASS.
