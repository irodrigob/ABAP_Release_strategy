CLASS zcl_rel_strategy_md_query DEFINITION
  PUBLIC
  CREATE PUBLIC .

  PUBLIC SECTION.

    TYPES: BEGIN OF ts_user_pgroup_depart_subs.
        INCLUDE TYPE zif_rel_data=>ts_users_purchase_group.
    TYPES: dept_subs      TYPE zrel_e_depart_subsidiary,
           dept_subs_desc TYPE zrel_e_depart_subsidiary_desc,
           END OF ts_user_pgroup_depart_subs.
    TYPES: tt_user_pgroup_depart_subs TYPE STANDARD TABLE OF ts_user_pgroup_depart_subs WITH EMPTY KEY.
    TYPES: BEGIN OF ts_user_depart_subs,
             username       TYPE xubname,
             username_desc  TYPE string,
             dept_subs      TYPE zrel_e_depart_subsidiary,
             dept_subs_desc TYPE zrel_e_depart_subsidiary_desc,
             can_edit       TYPE sap_bool,
           END OF ts_user_depart_subs.
    TYPES: tt_user_depart_subs TYPE STANDARD TABLE OF ts_user_depart_subs WITH EMPTY KEY.
    TYPES: BEGIN OF ts_query_classification,
             objek          TYPE ausp-objek,
             atinn          TYPE ausp-atinn,
             value          TYPE ausp-atwrt,
             value_operand  TYPE zrel_e_strategy_operand,
             value_amount   TYPE zrel_e_strategy_amount,
             value_amount2  TYPE zrel_e_strategy_amount,
             value_currency TYPE waers,
           END OF ts_query_classification.
    TYPES: tt_query_classification TYPE STANDARD TABLE OF ts_query_classification WITH EMPTY KEY.
    TYPES: BEGIN OF ts_dept_purchase_group,
             dept_subs           TYPE zrel_e_depart_subsidiary,
             dept_subs_desc      TYPE zrel_t002t-description,
             purchase_group      TYPE ekgrp,
             purchase_group_desc TYPE eknam,
           END OF ts_dept_purchase_group.
    TYPES: tt_dept_purchase_group TYPE STANDARD TABLE OF ts_dept_purchase_group WITH EMPTY KEY.
    TYPES: BEGIN OF ts_user_lib_group,
             username      TYPE xubname,
             username_desc TYPE ad_namtext,
             group         TYPE t16fw-frggr,
             code          TYPE t16fw-frgco,
           END OF ts_user_lib_group.
    TYPES: tt_user_lib_group TYPE STANDARD TABLE OF ts_user_lib_group WITH EMPTY KEY.
    TYPES: BEGIN OF ts_pgroup_info,
             purchase_group      TYPE t024-ekgrp,
             purchase_group_desc TYPE t024-eknam,
           END OF ts_pgroup_info.
    TYPES: tt_pgroup_info TYPE STANDARD TABLE OF ts_pgroup_info WITH EMPTY KEY.
    TYPES: BEGIN OF ts_strategies_from_group,
             frggr TYPE t16fs-frggr,
             frgsx TYPE t16fs-frgsx,
             frgxt TYPE t16ft-frgxt,
             frgc1 TYPE frgco,
             frgc2 TYPE frgco,
             frgc3 TYPE frgco,
             frgc4 TYPE frgco,
             frgc5 TYPE frgco,
             frgc6 TYPE frgco,
             frgc7 TYPE frgco,
             frgc8 TYPE frgco,
             frgex TYPE frgex,
           END OF ts_strategies_from_group.
    TYPES: tt_strategies_from_group TYPE STANDARD TABLE OF ts_strategies_from_group WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_strategy_code_approvers,
             group    TYPE t16fs-frggr,
             strategy TYPE t16fs-frgsx,
             level    TYPE int1,
             code     TYPE t16fw-frgco,
           END   OF ts_strategy_code_approvers.
    TYPES: tt_strategy_code_approvers TYPE STANDARD TABLE OF ts_strategy_code_approvers WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_liberation_group_list,
             group      TYPE t16fg-frggr,
             group_desc TYPE t16fh-frggt,
           END OF ts_liberation_group_list.
    TYPES: tt_liberation_group_list TYPE STANDARD TABLE OF ts_liberation_group_list WITH EMPTY KEY.

    "! <p class="shorttext synchronized">CONSTRUCTOR</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    METHODS constructor
      IMPORTING
        iv_langu TYPE sylangu DEFAULT sy-langu.
    "! <p class="shorttext synchronized">Obtiene informaci??n de los grupos de comopra</p>
    "! Esta informaci??n se necesita para alimentar los datos del BOPF o modelos internos
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter it_r_purchase_group | <p class="shorttext synchronized">Rango grupo de compras</p>
    "! @parameter et_info | <p class="shorttext synchronized">Informaci??n del grupo de compras</p>
    METHODS get_pgroup_info
      IMPORTING
        iv_purchase_group   TYPE ekgrp OPTIONAL
        it_r_purchase_group TYPE zif_rel_data=>tt_r_purchase_group OPTIONAL
      EXPORTING
        et_info             TYPE tt_pgroup_info.
    "! <p class="shorttext synchronized">Grupos de compras filtrando por usuario</p>
    "! Devuelve donde el usuario es comprador y aprobador.
    "! @parameter it_r_users | <p class="shorttext synchronized">Rango de usuarios</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_get_desc | <p class="shorttext synchronized">Obtener descripciones</p>
    "! @parameter et_purchase_group | <p class="shorttext synchronized">Grupos de compras asignados a usuarios</p>
    METHODS get_purchase_group_from_users
      IMPORTING
        it_r_users        TYPE zif_rel_data=>tt_r_username OPTIONAL
        iv_user           TYPE xubname OPTIONAL
        iv_get_desc       TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_purchase_group TYPE zif_rel_data=>tt_users_purchase_group.
    "! <p class="shorttext synchronized">Compradores de grupos de compra</p>
    "! Se puede obtener los compradores filtrando por departamento/filial o por comprador
    "! @parameter it_r_users | <p class="shorttext synchronized">Rango de usuarios</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter it_r_purchase_group | <p class="shorttext synchronized">Rango grupo de compras</p>
    "! @parameter iv_get_desc | <p class="shorttext synchronized">Obtener descripciones</p>
    "! @parameter et_purchase_group | <p class="shorttext synchronized">Grupos de compras asignados a usuarios</p>
    METHODS get_purchase_group_buyer
      IMPORTING
        it_r_users              TYPE zif_rel_data=>tt_r_username OPTIONAL
        iv_user                 TYPE xubname OPTIONAL
        iv_purchase_group       TYPE ekgrp OPTIONAL
        it_r_purchase_group     TYPE zif_rel_data=>tt_r_purchase_group OPTIONAL
        iv_get_desc             TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_buyer_purchase_group TYPE zif_rel_data=>tt_users_purchase_group.
    "! <p class="shorttext synchronized">Aprobadores estrategias por grupos de compra por usuario(s)</p>
    "! Se devuelve los grupos de compras donde los usuarios pasados por par??metro son aprobadores
    "! La info se obtiene de las estrategias de liberaci??n
    "! @parameter it_r_users | <p class="shorttext synchronized">Rango de usuarios</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_get_desc | <p class="shorttext synchronized">Obtener descripciones</p>
    "! @parameter et_purchase_group | <p class="shorttext synchronized">Grupos de compras asignados a usuarios</p>
    METHODS get_pgroup_strategy_from_users
      IMPORTING
        it_r_users        TYPE zif_rel_data=>tt_r_username OPTIONAL
        iv_user           TYPE xubname OPTIONAL
        iv_get_desc       TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_purchase_group TYPE zif_rel_data=>tt_users_purchase_group.
    "! <p class="shorttext synchronized">Devuelve grupos de compra con Dpto./filiales del usuario</p>
    "! @parameter it_r_users | <p class="shorttext synchronized">Rango de usuarios</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter et_dept_subs | <p class="shorttext synchronized">Grupos de compras asignados a usuarios</p>
    METHODS get_pgroup_depart_from_users
      IMPORTING
        it_r_users          TYPE zif_rel_data=>tt_r_username OPTIONAL
        iv_user             TYPE xubname OPTIONAL
      EXPORTING
        et_pgroup_dept_subs TYPE tt_user_pgroup_depart_subs.
    "! <p class="shorttext synchronized">Devuelve los departamento/filiales del usuario</p>
    "! @parameter it_r_users | <p class="shorttext synchronized">Rango de usuarios</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter et_dept_subs | <p class="shorttext synchronized">Grupos de compras asignados a usuarios</p>
    METHODS get_depart_subs_from_users
      IMPORTING
        it_r_users   TYPE zif_rel_data=>tt_r_username OPTIONAL
        iv_user      TYPE xubname OPTIONAL
      EXPORTING
        et_dept_subs TYPE tt_user_depart_subs.
    "! <p class="shorttext synchronized">B??squeda en el sistema de clasificaci??n</p>
    "! En la interface zcl_rel_user=cs_strategy-query est??n los campos por lo que se puede buscar
    "! @parameter it_params_sl | <p class="shorttext synchronized">Valores a buscar</p>
    "! @parameter et_data | <p class="shorttext synchronized">Datos de la b??squeda</p>
    METHODS query_classification
      IMPORTING
        it_params_sl TYPE pivb_rsparamsl_255_t
      EXPORTING
        et_data      TYPE tt_query_classification.
    "! <p class="shorttext synchronized">Devuelve los grupos de compra de un departamento(s)</p>
    "! @parameter iv_dept_subs | <p class="shorttext synchronized">Departamento/subsidiaria</p>
    "! @parameter it_r_dept_subs | <p class="shorttext synchronized">Rango Departamento/subsidiaria</p>
    "! @parameter et_purchase_group | <p class="shorttext synchronized">Grupos de compra por departamento</p>
    METHODS get_pgroup_from_dept_subs
      IMPORTING
        iv_dept_subs      TYPE zrel_e_depart_subsidiary OPTIONAL
        it_r_dept_subs    TYPE zif_rel_data=>tt_r_dept_subs OPTIONAL
      EXPORTING
        et_purchase_group TYPE tt_dept_purchase_group.
    "! <p class="shorttext synchronized">Devuelve los datos de las estrateg??as por grupos de compra</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter it_r_purchase_group | <p class="shorttext synchronized">Rango grupo de compras</p>
    "! @parameter et_strategy_data | <p class="shorttext synchronized">Datos de las estrateg??as por grupos de compra</p>
    METHODS get_strate_data_from_pgroup
      IMPORTING
        iv_purchase_group   TYPE ekgrp OPTIONAL
        it_r_purchase_group TYPE zif_rel_data=>tt_r_purchase_group OPTIONAL
      EXPORTING
        et_strategy_data    TYPE zif_rel_data=>tt_pgroup_strategy_data.
    "! <p class="shorttext synchronized">Devuelve la descripci??n del usuario</p>
    "! @parameter rv_username_desc | <p class="shorttext synchronized">Nombre</p>
    METHODS get_username_desc
      IMPORTING iv_username             TYPE xubname
      RETURNING VALUE(rv_username_desc) TYPE ad_namtext.
    "! <p class="shorttext synchronized">Devuelve los usuarios de un c??digo o grupo liberaci??n</p>
    "! Importante los c??digos est??n ligados al grupo de liberaci??n que tambi??n se devuelve.
    "! @parameter it_r_code | <p class="shorttext synchronized">Rango de c??digos de liberaci??n</p>
    "! @parameter iv_code | <p class="shorttext synchronized">C??digo de liberaci??n</p>
    "! @parameter it_r_group | <p class="shorttext synchronized">Rango de grupos de liberaci??n</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Grupo de liberaci??n</p>
    "! @parameter iv_get_desc | <p class="shorttext synchronized">Obtener descripciones</p>
    "! @parameter et_user_lib_group | <p class="shorttext synchronized">Grupos de liberaci??n del usuario</p>
    METHODS get_users_from_lib_group_code
      IMPORTING
        it_r_code         TYPE zif_rel_data=>tt_r_liberation_code OPTIONAL
        iv_code           TYPE frgco  OPTIONAL
        iv_group          TYPE frggr OPTIONAL
        it_r_group        TYPE zif_rel_data=>tt_r_liberation_group OPTIONAL
        iv_get_desc       TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_user_lib_group TYPE tt_user_lib_group.
    "! <p class="shorttext synchronized">Devuelve el grupo Lib. a partir del grupo de compras</p>
    "! @parameter iv_ekgrp | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter rv_frggr | <p class="shorttext synchronized">Grupo de liberaci??n</p>
    METHODS get_release_group_from_pgroup
      IMPORTING
                iv_purchase_group TYPE ekgrp
      RETURNING VALUE(rv_group)   TYPE frggr.
    "! <p class="shorttext synchronized">Devuelve el grupo Lib. a partir del grupo de compras</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Grupo de liberaci??n</p>
    "! @parameter it_r_group | <p class="shorttext synchronized">Rango de grupo de liberaci??n</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">idioma</p>
    "! @parameter iv_add_desc | <p class="shorttext synchronized">A??adir descripciones</p>
    "! @parameter et_values | <p class="shorttext synchronized">Valores</p>
    "! @parameter et_values_desc | <p class="shorttext synchronized">Valores de la descripci??n</p>
    METHODS get_strategys_from_group
      IMPORTING
                iv_group         TYPE t16fs-frggr OPTIONAL
                it_r_group       TYPE zif_rel_data=>tt_r_liberation_group OPTIONAL
                iv_langu         TYPE sylangu OPTIONAL
                iv_add_desc      TYPE sap_bool DEFAULT abap_true
      RETURNING VALUE(rt_values) TYPE tt_strategies_from_group.
    "! <p class="shorttext synchronized">Devuelve el grupo de liberaci??n a partir de la sociedad</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Grupo de liberaci??n</p>
    "! @parameter it_r_group | <p class="shorttext synchronized">Rango de grupo de liberaci??n</p>
    METHODS get_release_group_from_company
      IMPORTING
                iv_company      TYPE bukrs
      RETURNING VALUE(rv_group) TYPE t16fs-frggr.
    "! <p class="shorttext synchronized">Mira si existe la estrategia en el sistema de clasificaci??n</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Grupo de liberaci??n</p>
    "! @parameter iv_strategy | <p class="shorttext synchronized">Estrateg??a liberaci??n</p>
    "! @parameter iv_strrv_existategy | <p class="shorttext synchronized">Existe</p>
    METHODS check_exist_strateg_classif
      IMPORTING iv_group        TYPE t16fs-frggr
                iv_strategy     TYPE t16fs-frgsx
      RETURNING VALUE(rv_exist) TYPE sap_bool.
    "! <p class="shorttext synchronized">Devuelve los c??digos de lib. que esta asignado un usuario</p>
    "! Importante los c??digos est??n ligados al grupo de liberaci??n que tambi??n se devuelve.
    "! @parameter it_r_users | <p class="shorttext synchronized">Rango de usuarios</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter iv_get_desc | <p class="shorttext synchronized">Obtener descripciones</p>
    "! @parameter et_user_lib_group | <p class="shorttext synchronized">Grupos de liberaci??n del usuario</p>
    METHODS get_liberation_code_from_user
      IMPORTING
        it_r_users        TYPE zif_rel_data=>tt_r_username OPTIONAL
        iv_user           TYPE xubname OPTIONAL
        iv_get_desc       TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_user_lib_group TYPE tt_user_lib_group.
    "! <p class="shorttext synchronized">Devuelve las descripciones de los codigos de liberaci??n</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Grupo</p>
    "! @parameter it_r_group | <p class="shorttext synchronized">Rango de grupo</p>
    "! @parameter iv_code | <p class="shorttext synchronized">C??digo</p>
    "! @parameter it_r_code | <p class="shorttext synchronized">Rango de c??digos</p>
    "! @parameter iv_langu | <p class="shorttext synchronized">Idioma</p>
    "! @parameter rt_descriptions | <p class="shorttext synchronized">Descripciones</p>
    METHODS get_descriptions_release_code
      IMPORTING
                iv_group               TYPE t16fd-frggr OPTIONAL
                it_r_group             TYPE zif_rel_data=>tt_r_liberation_group OPTIONAL
                iv_code                TYPE t16fd-frgco OPTIONAL
                it_r_code              TYPE zif_rel_data=>tt_r_liberation_code OPTIONAL
                iv_langu               TYPE sylangu OPTIONAL
      RETURNING VALUE(rt_descriptions) TYPE zrel_i_t16fd.
    "! <p class="shorttext synchronized">Devuelve los estados de liberaci??n</p>
    "! @parameter iv_group | <p class="shorttext synchronized">Grupo</p>
    "! @parameter it_r_group | <p class="shorttext synchronized">Rango de grupo</p>
    "! @parameter iv_strategy | <p class="shorttext synchronized">Estrateg??a</p>
    "! @parameter it_r_strategy | <p class="shorttext synchronized">Rango de estrategias</p>
    "! @parameter et_t16fk | <p class="shorttext synchronized">Estados de liberaci??n</p>
    "! @parameter et_t16fv | <p class="shorttext synchronized">Requisitos de liberaci??n</p>
    METHODS get_liberation_statuses
      IMPORTING
                iv_group      TYPE t16fk-frggr OPTIONAL
                it_r_group    TYPE zif_rel_data=>tt_r_liberation_group OPTIONAL
                iv_strategy   TYPE t16fk-frgsx OPTIONAL
                it_r_strategy TYPE zif_rel_data=>tt_r_strategy_code OPTIONAL
      EXPORTING et_t16fk      TYPE zrel_i_t16fk
                et_t16fv      TYPE zrel_i_t16fv.
    "! <p class="shorttext synchronized">Convierte la T16FS en una tabla</p>
    "! @parameter is_t16fs | <p class="shorttext synchronized">Datos de la T16FS</p>
    "! @parameter et_data | <p class="shorttext synchronized">Datos en formato tabla</p>
    METHODS convert_t16fs_row_data
      IMPORTING
                is_t16fs       TYPE t16fs
      RETURNING VALUE(rt_data) TYPE tt_strategy_code_approvers.
    "! <p class="shorttext synchronized">Obtiene grupo liberaci??n del grupo compra y/o sociedad</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter iv_company | <p class="shorttext synchronized">Sociedad</p>
    "! @parameter rv_group | <p class="shorttext synchronized">Grupo</p>
    METHODS get_lib_group_from_pgroup
      IMPORTING
                iv_purchase_group TYPE ekgrp
                iv_company        TYPE bukrs OPTIONAL
      RETURNING VALUE(rv_group)   TYPE t16fs-frggr.
    "! <p class="shorttext synchronized">Obtiene el listado de grupos de liberaci??n</p>
    "! @parameter rt_list | <p class="shorttext synchronized">Listado</p>
    METHODS get_liberation_group_list
      RETURNING VALUE(rt_list) TYPE tt_liberation_group_list.
  PROTECTED SECTION.
    TYPES: BEGIN OF ts_username_desc,
             username      TYPE xubname,
             username_desc TYPE ad_namtext,
           END OF ts_username_desc.
    TYPES: tt_username_desc TYPE STANDARD TABLE OF ts_username_desc WITH EMPTY KEY.
    TYPES: BEGIN OF ts_lib_group_code,
             group TYPE t16fw-frggr,
             code  TYPE t16fw-frgco,
           END OF ts_lib_group_code.
    TYPES: tt_lib_group_code TYPE STANDARD TABLE OF ts_lib_group_code WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_lib_group_strategy,
             group    TYPE t16fs-frggr,
             strategy TYPE t16fs-frgsx,
           END OF ts_lib_group_strategy.
    TYPES: tt_lib_group_strategy TYPE STANDARD TABLE OF ts_lib_group_strategy WITH DEFAULT KEY.
    TYPES: BEGIN OF ts_depart_group_strategy,
             purchase_group      TYPE ekgrp,
             purchase_group_desc TYPE eknam.
        INCLUDE TYPE ts_lib_group_strategy.
    TYPES:
               END OF ts_depart_group_strategy.
    TYPES: tt_depart_group_strategy TYPE STANDARD TABLE OF ts_depart_group_strategy WITH EMPTY KEY.
    TYPES: BEGIN OF ts_strategy_approvers.
        INCLUDE TYPE ts_strategy_code_approvers.
        INCLUDE TYPE ts_username_desc.
    TYPES: END   OF ts_strategy_approvers.
    TYPES: tt_strategy_approvers TYPE STANDARD TABLE OF ts_strategy_approvers WITH DEFAULT KEY.
    DATA mv_langu TYPE sylangu.
    DATA mt_username_desc TYPE tt_username_desc.




    "! <p class="shorttext synchronized">Devuelve los aprobadores de estrategia a por grupo</p>
    "! @parameter it_group_code | <p class="shorttext synchronized">Grupo y codigos de liberaci??n</p>
    "! @parameter iv_filter_lib_code | <p class="shorttext synchronized">Filtra los registros del codigo de liberaci??n</p>
    "! @parameter et_strategy | <p class="shorttext synchronized">Aprobadores de la estrategia</p>
    METHODS get_approv_strat_from_group
      IMPORTING
        it_group_code      TYPE tt_lib_group_code
        iv_filter_lib_code TYPE sap_bool DEFAULT abap_false
      EXPORTING
        et_approvers       TYPE tt_strategy_approvers.
    "! <p class="shorttext synchronized">Devuelve los aprobadores de una estrategia</p>
    "! @parameter it_group_strag | <p class="shorttext synchronized">Grupo y estrategia a buscar los datos</p>
    "! @parameter iv_filter_lib_code | <p class="shorttext synchronized">Filtra los registros del codigo de liberaci??n</p>
    "! @parameter et_strategy | <p class="shorttext synchronized">Aprobadores de la estrategia</p>
    METHODS get_approvers_strategy
      IMPORTING
        it_group_strag TYPE tt_lib_group_strategy
      EXPORTING
        et_approvers   TYPE tt_strategy_approvers.

    "! <p class="shorttext synchronized">Devuelvo grupo y strategia a partir grupos de compras</p>
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    "! @parameter it_r_purchase_group | <p class="shorttext synchronized">Rango grupo de compras</p>
    "! @parameter iv_get_desc | <p class="shorttext synchronized">Obtener descripciones</p>
    "! @parameter et_group_strag | <p class="shorttext synchronized">Grupos y estrategias por departamento</p>
    METHODS get_group_strag_from_pgroup
      IMPORTING
        iv_purchase_group   TYPE ekgrp
        it_r_purchase_group TYPE zif_rel_data=>tt_r_purchase_group
        iv_get_desc         TYPE sap_bool DEFAULT abap_true
      EXPORTING
        et_group_strag      TYPE tt_depart_group_strategy.
    "! <p class="shorttext synchronized">Completa las descripciones de los c??digos de liberaci??n</p>
    "! Se hace para aquellos usuarios que ya no existen salgan con una denominaci??n. Porque suele
    "! estar en la tabla de denominaciones
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    METHODS complete_lib_code_desc
      CHANGING
        ct_user_lib_group TYPE zcl_rel_strategy_md_query=>tt_user_lib_group.
    "! <p class="shorttext synchronized">Ordenaci??n por strategia por importe y operando</p>
    "! La ordenaci??n no se puede realizar en el sistema de clasificaci??n porque hay que convertir
    "! los valores a un formato legible para saber sus valores.
    "! @parameter iv_purchase_group | <p class="shorttext synchronized">Grupo de compras</p>
    METHODS sort_strategies
      CHANGING
        ct_strategies TYPE zif_rel_data=>tt_strategy_data.
    "! <p class="shorttext synchronized">A??ade los departamentos de los usuarios de AP</p>
    "! @parameter iv_user | <p class="shorttext synchronized">Usuario</p>
    "! @parameter et_deps_subs | <p class="shorttext synchronized">Usuario y sus departamentos</p>
    METHODS get_depart_from_users_auth
      IMPORTING iv_user      TYPE syuname
      EXPORTING et_dept_subs TYPE tt_user_depart_subs.

  PRIVATE SECTION.
ENDCLASS.



CLASS zcl_rel_strategy_md_query IMPLEMENTATION.
  METHOD constructor.
    mv_langu = sy-langu.
  ENDMETHOD.

  METHOD get_purchase_group_buyer.
    DATA lt_r_username TYPE zif_rel_data=>tt_r_username.
    DATA lt_r_purchase_group TYPE zif_rel_data=>tt_r_dept_subs.

    CLEAR et_buyer_purchase_group.

    IF iv_user IS NOT INITIAL.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = iv_user ) INTO TABLE lt_r_username.
    ELSE.
      lt_r_username = it_r_users.
    ENDIF.

    IF iv_purchase_group IS NOT INITIAL.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = iv_purchase_group ) INTO TABLE lt_r_purchase_group.
    ELSE.
      lt_r_purchase_group = it_r_purchase_group.
    ENDIF.

    SELECT DISTINCT zusuario AS username ekgrp AS purchase_group INTO CORRESPONDING FIELDS OF TABLE et_buyer_purchase_group
           FROM zm014
           WHERE zusuario IN lt_r_username
                 AND ekgrp IN lt_r_purchase_group.
    IF sy-subrc = 0.

      IF iv_get_desc = abap_true.
        SELECT ekgrp, eknam INTO TABLE @DATA(lt_grp_compra)
               FROM t024
               FOR ALL ENTRIES IN @et_buyer_purchase_group
               WHERE ekgrp = @et_buyer_purchase_group-purchase_group.


        LOOP AT et_buyer_purchase_group ASSIGNING FIELD-SYMBOL(<ls_data>).

          " Descricpion del grupo de compra
          READ TABLE lt_grp_compra ASSIGNING FIELD-SYMBOL(<ls_grp_compra>) WITH KEY ekgrp = <ls_data>-purchase_group.
          IF sy-subrc = 0.
            <ls_data>-purchase_group_desc = <ls_grp_compra>-eknam.
          ENDIF.

          " Descripci??n de usuario
          <ls_data>-username_desc = get_username_desc( <ls_data>-username ).
          " Si no hay descripci??n del usuario le pongo como descripci??n el c??digo
          <ls_data>-username_desc = COND #( WHEN <ls_data>-username_desc IS INITIAL THEN <ls_data>-username ELSE <ls_data>-username_desc ).


        ENDLOOP.

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_username_desc.
    CLEAR rv_username_desc.
    READ TABLE mt_username_desc ASSIGNING FIELD-SYMBOL(<ls_username_desc>) WITH KEY username = iv_username.
    IF sy-subrc = 0.
      rv_username_desc = <ls_username_desc>-username_desc.
    ELSE.
      TRY.
          NEW zcl_rel_user( iv_user = iv_username )->get_user_detail( IMPORTING es_user_detail = DATA(ls_user_detail) ).
          rv_username_desc = ls_user_detail-fullname.
          INSERT VALUE #( username = iv_username username_desc = ls_user_detail-fullname ) INTO TABLE mt_username_desc.
        CATCH zcx_rel.
      ENDTRY.

    ENDIF.
  ENDMETHOD.

  METHOD get_purchase_group_from_users.

    CLEAR: et_purchase_group.

    " Primero obtengmos los grupos donde el usuario es comprador
    get_purchase_group_buyer(
      EXPORTING
        it_r_users  = it_r_users
        iv_user     = iv_user
        iv_get_desc = iv_get_desc
      IMPORTING
        et_buyer_purchase_group     = DATA(lt_purchase_group_buyer) ).


    INSERT LINES OF lt_purchase_group_buyer INTO TABLE et_purchase_group.


    " Ahora lo sacamos donde el usuario es aprobador
    get_pgroup_strategy_from_users(
      EXPORTING
        it_r_users        = it_r_users
        iv_user           = iv_user
        iv_get_desc       = iv_get_desc
      IMPORTING
        et_purchase_group = DATA(lt_purchase_group_strag) ).

    INSERT LINES OF lt_purchase_group_strag INTO TABLE et_purchase_group.

    " Quito posibles duplicados
    SORT et_purchase_group BY purchase_group username.
    DELETE ADJACENT DUPLICATES FROM et_purchase_group COMPARING purchase_group username.

  ENDMETHOD.

  METHOD get_pgroup_depart_from_users.

    CLEAR: et_pgroup_dept_subs.

    " Para saber los departamentos/filiales tenemos que saber a que grupos de compras pertenece.
    get_purchase_group_from_users( EXPORTING iv_user = iv_user
                                        it_r_users = it_r_users
                                        iv_get_desc = abap_true
                              IMPORTING et_purchase_group = DATA(lt_purchase_group) ).

    IF lt_purchase_group IS NOT INITIAL.

      " Sacamos los departamentos.
      SELECT a~dept_subs, a~ekgrp, b~description AS dept_subs_desc  INTO TABLE @DATA(lt_dept_subs)
             FROM zrel_t003 AS a LEFT OUTER JOIN zrel_t002t AS b ON
                  b~dept_subs = a~dept_subs
                  AND b~spras = @mv_langu
             FOR ALL ENTRIES IN @lt_purchase_group
             WHERE a~ekgrp = @lt_purchase_group-purchase_group.

      " Ahora se cruzan los datos. Teniendo en cuenta que solo se a??adir??n aquellos grupos de compra con departamento asociado.
      LOOP AT lt_purchase_group ASSIGNING FIELD-SYMBOL(<ls_purchase_group>).

        READ TABLE lt_dept_subs ASSIGNING FIELD-SYMBOL(<ls_dept_subs>) WITH KEY ekgrp = <ls_purchase_group>-purchase_group.
        IF sy-subrc = 0.

          INSERT CORRESPONDING #( <ls_purchase_group> ) INTO TABLE et_pgroup_dept_subs
                 ASSIGNING FIELD-SYMBOL(<ls_pgroup_dept_subs>).

          <ls_pgroup_dept_subs> = CORRESPONDING #( BASE ( <ls_pgroup_dept_subs> ) <ls_dept_subs>  ).

        ENDIF.

      ENDLOOP.

    ENDIF.
  ENDMETHOD.

  METHOD get_depart_subs_from_users.

    CLEAR: et_dept_subs.

    " Llamo al m??todo que me da el detalle de grupos de compras y departamentos del usuario
    get_pgroup_depart_from_users(
      EXPORTING
        it_r_users          = it_r_users
        iv_user             = iv_user
      IMPORTING
        et_pgroup_dept_subs = DATA(lt_pgroup_dept_subs) ).

    " Paso los datos a tabla de salida que tiene los mismos campos salvo el grupo de compras
    LOOP AT lt_pgroup_dept_subs ASSIGNING FIELD-SYMBOL(<ls_pgrup_dept_subs>).
      DATA(ls_dept_subs) = CORRESPONDING ts_user_depart_subs( <ls_pgrup_dept_subs> ).
      " Usuarios que son compradores o aprobadores en los pedidos pueden editar.
      ls_dept_subs-can_edit = abap_true.
      INSERT ls_dept_subs INTO TABLE et_dept_subs.
    ENDLOOP.

    " Ahora se recuperan los departamentos que tendr??n acceso el usuario segun sus autorizaciones
    get_depart_from_users_auth( EXPORTING iv_user = iv_user
                              IMPORTING et_dept_subs = DATA(lt_depts_subs_auth) ).
    " A??ado los departamentos que no tenga en el apartado anterior
    LOOP AT lt_depts_subs_auth ASSIGNING FIELD-SYMBOL(<ls_dep_subs_auth>).
      READ TABLE et_dept_subs TRANSPORTING NO FIELDS WITH KEY dept_subs = <ls_dep_subs_auth>-dept_subs.
      IF sy-subrc NE 0.
        INSERT <ls_dep_subs_auth> INTO TABLE et_dept_subs.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.

  METHOD get_pgroup_strategy_from_users.
    DATA lt_r_ekgrp TYPE RANGE OF ekgrp.

    " Primero tenemos que saber en que grupos de liberaci??n esta el usuario
    get_liberation_code_from_user( EXPORTING iv_user = iv_user
                                         it_r_users = it_r_users
                                         iv_get_desc = iv_get_desc
                               IMPORTING et_user_lib_group = DATA(lt_user_lib_group) ).

    " Si hay datos hay que sacar para que estrateg??as de liberaci??n esta asignado el c??digo de liberaci??n
    IF lt_user_lib_group IS NOT INITIAL.

      " Pasamos los datos a una tabla para poder buscar las estrategias asociadas
      DATA(lt_lib_group_code) = CORRESPONDING tt_lib_group_code( lt_user_lib_group ).
      SORT lt_lib_group_code.
      DELETE ADJACENT DUPLICATES FROM lt_lib_group_code COMPARING ALL FIELDS.

      " Ahora sacamos las estrategias.
      get_approv_strat_from_group(
        EXPORTING
          it_group_code = lt_lib_group_code
          iv_filter_lib_code = abap_true " Para que me devuelve las l??neas con los c??digos pasados
        IMPORTING
          et_approvers   = DATA(lt_strategy) ).

      IF lt_strategy IS NOT INITIAL.
        " Los grupos de compras est??n en el sistema de clasificaci??n
        query_classification(
          EXPORTING
            it_params_sl = VALUE #( FOR <wa> IN lt_strategy ( selname = zif_rel_data=>cs_strategy-classification-query-fields-objek
                                                              kind = 'S'
                                                              sign = 'I'
                                                              option = 'EQ'
                                                              low = |{ <wa>-group }{ <wa>-strategy }| ) )
          IMPORTING
            et_data      = DATA(lt_classif_data) ).

        " Si se han encontrado datos hay que comenzar a montar los datos de salida
        IF lt_classif_data IS NOT INITIAL.
          " Leemos los grupos de compra obtenidos.
          LOOP AT lt_classif_data ASSIGNING FIELD-SYMBOL(<ls_classif_data>) WHERE atinn = zif_rel_data=>cs_strategy-classification-fields_charac-ekgrp.

            " Leemos las estrategias asociadas a los grupos de compra. Que como hemos filtrado para que nos devuelva los codigos que le pasamos solo habr??
            " los registros que nos interesa.
            LOOP AT lt_strategy ASSIGNING FIELD-SYMBOL(<ls_strategy>) WHERE group = <ls_classif_data>-objek(2)
                                                                            AND strategy = <ls_classif_data>-objek+2(2).

              READ TABLE lt_user_lib_group ASSIGNING FIELD-SYMBOL(<ls_user_lib_group>)
                                           WITH KEY group = <ls_strategy>-group
                                                    code = <ls_strategy>-code.
              IF sy-subrc = 0.
                " Solo quiero registros ??nicos por ello miro si ya he insertado el registro previamente.
                READ TABLE et_purchase_group TRANSPORTING NO FIELDS WITH KEY username = <ls_user_lib_group>-username
                                                                             purchase_group = <ls_classif_data>-value.
                IF sy-subrc NE 0.
                  INSERT VALUE #( username = <ls_user_lib_group>-username
                                  username_desc = <ls_user_lib_group>-username_desc
                                  purchase_group = <ls_classif_data>-value ) INTO TABLE et_purchase_group.
                  " Paso el grupo de compras a un ranges para buscar la descripci??n, en caso necesario.
                  INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_classif_data>-value ) INTO TABLE lt_r_ekgrp.
                ENDIF.
              ENDIF.
            ENDLOOP.
          ENDLOOP.

          " Se buscan las descripciones de los grupos de compra
          IF iv_get_desc = abap_true AND lt_r_ekgrp IS NOT INITIAL.
            SELECT ekgrp, eknam INTO TABLE @DATA(lt_grp_compra)
                    FROM t024
                    WHERE ekgrp IN @lt_r_ekgrp.

            LOOP AT et_purchase_group ASSIGNING FIELD-SYMBOL(<ls_purchase_group>).
              READ TABLE lt_grp_compra ASSIGNING FIELD-SYMBOL(<ls_grp_compra>) WITH KEY ekgrp = <ls_purchase_group>-purchase_group.
              IF sy-subrc = 0.
                <ls_purchase_group>-purchase_group_desc = <ls_grp_compra>-eknam.
              ENDIF.
            ENDLOOP.
          ENDIF.

        ENDIF.
      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_liberation_code_from_user.
    DATA lt_r_username TYPE zif_rel_data=>tt_r_username.

    CLEAR: et_user_lib_group.

    IF iv_user IS NOT INITIAL.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = iv_user ) INTO TABLE lt_r_username.
    ELSE.
      lt_r_username = it_r_users.
    ENDIF.


    SELECT objid AS username frggr AS group frgco AS code INTO CORRESPONDING FIELDS OF TABLE et_user_lib_group
           FROM t16fw
           WHERE objid IN lt_r_username
                 AND otype = zif_rel_data=>cs_strategy-master_data-otype_usersap.

    IF sy-subrc = 0.

      " Leemos los grupos de liberaci??n que se tienen que excluir del proceso.
      SELECT 'I' AS sign, 'EQ' AS option, frggr AS low, frggr AS high
            INTO TABLE @DATA(lt_r_excl_group)
            FROM zrel_t025.
      IF sy-subrc = 0.
        DELETE et_user_lib_group WHERE group IN lt_r_excl_group.
      ENDIF.

      " Saco las descripciones si se ha indicado
      IF iv_get_desc = abap_true AND et_user_lib_group IS NOT INITIAL.

        " Se completan las descripciones de aquellos c??digos que no tengan descripci??n de usuario
        complete_lib_code_desc( CHANGING ct_user_lib_group = et_user_lib_group ).

      ENDIF.

    ENDIF.

  ENDMETHOD.

  METHOD get_approv_strat_from_group.
    DATA lt_r_code_all TYPE zif_rel_data=>tt_r_liberation_code.
    CLEAR: et_approvers.

    " Las estrategias se buscan a partir de los grupos pasados por par??metro. Pero adem??s, tambi??n se puede
    " a??adir el filtro de codigo de liberaci??n.
    DATA(lt_r_group) = VALUE zif_rel_data=>tt_r_liberation_group( FOR <wa> IN it_group_code ( sign = 'I' option = 'EQ' low = <wa>-group ) ).

    " Saco los datos en bruto de las estrategias de liberacion
    SELECT * INTO TABLE @DATA(lt_t16fs)
          FROM t16fs
          WHERE frggr IN @lt_r_group.
    IF sy-subrc = 0.

      " Se recorren los registros para a??adir a la tabla de datos. La tabla
      " se devolver?? un registro por aprobador, por ello hay que pivotar los
      " campos de los aprobadores en registros.
      LOOP AT lt_t16fs ASSIGNING FIELD-SYMBOL(<ls_t16fs>).
        DATA(lt_strategy) = convert_t16fs_row_data( EXPORTING is_t16fs = <ls_t16fs> ).

        " Monto un ranges con los codigos de liberaci??n que se quieren filtrar por codigo de liberaci??n
        DATA(lt_r_code) = VALUE zif_rel_data=>tt_r_liberation_code( FOR <wa1> IN it_group_code WHERE ( group = <ls_t16fs>-frggr
                                                                                                       AND code NE '' )
                                                                                               ( sign = 'I' option = 'EQ' low = <wa1>-code ) ).

        " Me guardo los codigos de liberaci??n para luego poder buscar los usuarios asociados.
        INSERT LINES OF lt_r_code INTO TABLE lt_r_code_all.

        " Si se marca que se filtran por c??digo de liberaci??n, entonces solo entrar??n aquellas l??neas que tengan el c??digo de liberaci??n
        " pasado.
        IF iv_filter_lib_code = abap_true.
          LOOP AT lt_strategy ASSIGNING FIELD-SYMBOL(<ls_strategy>) WHERE code IN lt_r_code.
            INSERT CORRESPONDING #( <ls_strategy> ) INTO TABLE et_approvers.
          ENDLOOP.
        ELSE.
          " Si no se quiere filtrar por c??digo de liberaci??n, miro si la estrategia tiene alguno de los codigos pasados.
          " Si es as??, entonces entran todos.
          LOOP AT lt_strategy TRANSPORTING NO FIELDS WHERE code IN lt_r_code.
            EXIT.
          ENDLOOP.
          IF sy-subrc = 0.
            et_approvers = CORRESPONDING #( BASE ( et_approvers ) lt_strategy ).
          ENDIF.
        ENDIF.
      ENDLOOP.

      IF et_approvers IS NOT INITIAL.
        " Ahora saco los usuarios asociados a los c??digos
        get_users_from_lib_group_code(
          EXPORTING
            it_r_code         = lt_r_code_all
            it_r_group        = lt_r_group
          IMPORTING
            et_user_lib_group = DATA(lt_user_lib_group) ).

        IF lt_user_lib_group IS NOT INITIAL.
          LOOP AT et_approvers ASSIGNING FIELD-SYMBOL(<ls_approvers>).
            READ TABLE lt_user_lib_group ASSIGNING FIELD-SYMBOL(<ls_user_lib_group>)
                                         WITH KEY code = <ls_approvers>-code
                                                  group = <ls_approvers>-group.
            IF sy-subrc = 0.
              <ls_approvers>-username = <ls_user_lib_group>-username.
              <ls_approvers>-username_desc = <ls_user_lib_group>-username_desc.
            ENDIF.

          ENDLOOP.
        ENDIF.
      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD convert_t16fs_row_data.

    CLEAR: rt_data.

    DO.
      DATA(lv_campo) = |FRGC{ sy-index }|.

      ASSIGN COMPONENT lv_campo OF STRUCTURE is_t16fs TO FIELD-SYMBOL(<field>).
      IF sy-subrc = 0.
        IF <field> IS NOT INITIAL.
          INSERT VALUE #( group = is_t16fs-frggr
                          strategy = is_t16fs-frgsx
                          level = sy-index
                          code = <field>  ) INTO TABLE rt_data.
        ELSE.
          " Se sale porque ya no hay m??s aprobadores.
          EXIT.
        ENDIF.
      ELSE.
        EXIT.
      ENDIF.
    ENDDO.

  ENDMETHOD.

  METHOD query_classification.
    DATA lt_r_objek TYPE RANGE OF ausp-objek.
    DATA lt_r_atinn TYPE RANGE OF ausp-atinn.
    DATA lt_r_atwrt TYPE RANGE OF ausp-atwrt.

    CLEAR: et_data.

    " Paso los par??metros pasados a ranges
    zcl_rel_utilities=>conv_params_2_ranges( EXPORTING iv_name = zif_rel_data=>cs_strategy-classification-query-fields-objek
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_objek ).
    zcl_rel_utilities=>conv_params_2_ranges( EXPORTING iv_name = zif_rel_data=>cs_strategy-classification-query-fields-atinn
                                                           it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_atinn ).
    zcl_rel_utilities=>conv_params_2_ranges( EXPORTING iv_name = zif_rel_data=>cs_strategy-classification-query-fields-atwrt
                                                       it_params_sl = it_params_sl
                                             IMPORTING et_r_ranges = lt_r_atwrt ).


    SELECT objek, atinn, atwrt, atflv, atawe, atflb, ataw1, atcod INTO TABLE @DATA(lt_ausp)
           FROM ausp
           WHERE objek IN @lt_r_objek
                 AND atinn IN @lt_r_atinn
                 AND atwrt IN @lt_r_atwrt
                 AND klart = @zif_rel_data=>cs_strategy-classification-klart
           ORDER BY objek.

    IF sy-subrc = 0.
      " Convierto los valores las caracter??sticas de cantidad/importe a formato char
      LOOP AT lt_ausp ASSIGNING FIELD-SYMBOL(<ls_ausp>).
        INSERT VALUE #( objek = <ls_ausp>-objek
                  atinn = <ls_ausp>-atinn
                  value = <ls_ausp>-atwrt ) INTO TABLE et_data ASSIGNING FIELD-SYMBOL(<ls_data>).

        " La caracter??stica de precio se convierte
        IF <ls_ausp>-atinn = zif_rel_data=>cs_strategy-classification-fields_charac-gnetw.
          zcl_rel_utilities=>ctbp_convert_value_int_to_ext(
            EXPORTING
              iv_charctinn         = <ls_data>-atinn
              iv_value_from        = <ls_ausp>-atflv
              iv_value_to          = <ls_ausp>-atflb
              iv_value_relation    = <ls_ausp>-atcod
            IMPORTING
              ev_value_formatted   = <ls_data>-value
              ev_value_amount_from = <ls_data>-value_amount
              ev_value_amount_to = <ls_data>-value_amount2
              ev_value_operand = <ls_data>-value_operand
              ev_currency = <ls_data>-value_currency ).
        ENDIF.

      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD get_pgroup_from_dept_subs.
    DATA lt_r_dept_subs TYPE zif_rel_data=>tt_r_dept_subs.

    IF iv_dept_subs IS NOT INITIAL.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = iv_dept_subs ) INTO TABLE lt_r_dept_subs.
    ELSE.
      lt_r_dept_subs = it_r_dept_subs.
    ENDIF.


    SELECT a~dept_subs, c~description AS dept_subs_desc, a~ekgrp AS purchase_group, b~eknam AS purchase_group_desc  INTO TABLE @et_purchase_group
           FROM zrel_t003 AS a INNER JOIN t024 AS b ON
                b~ekgrp = a~ekgrp LEFT OUTER JOIN zrel_t002t AS c ON
                c~dept_subs = a~dept_subs
                AND c~spras = @mv_langu
           WHERE a~dept_subs IN @lt_r_dept_subs
           ORDER BY a~dept_subs, a~ekgrp.

  ENDMETHOD.

  METHOD get_strate_data_from_pgroup.

    CLEAR: et_strategy_data.

    " Obtenemos el grupo y estrateg??a de los grupos de compra pasados por par??metro
    get_group_strag_from_pgroup( EXPORTING iv_purchase_group = iv_purchase_group
                                           it_r_purchase_group = it_r_purchase_group
                                 IMPORTING et_group_strag = DATA(lt_group_strategy)  ).

    IF lt_group_strategy IS NOT INITIAL.
      " Sacamos la info completa del sistema de clasificaci??n a partir del grupo y estrateg??a
      query_classification( EXPORTING it_params_sl = VALUE #( FOR <wa1> IN lt_group_strategy ( selname = zif_rel_data=>cs_strategy-classification-query-fields-objek
                                                        kind = 'S'
                                                        sign = 'I'
                                                        option = 'EQ'
                                                        low = |{ <wa1>-group }{ <wa1>-strategy }| ) )
                                IMPORTING et_data = DATA(lt_classif_complete) ).

      " Ahora hay que sacar los aprobadores.
      get_approvers_strategy(
        EXPORTING
          it_group_strag = CORRESPONDING #( lt_group_strategy )
        IMPORTING
          et_approvers    = DATA(lt_approvers) ).


      " Ahora lo combinamos todo.
      LOOP AT lt_group_strategy ASSIGNING FIELD-SYMBOL(<ls_group_strategy_dummy>)
                                GROUP BY ( purchase_group = <ls_group_strategy_dummy>-purchase_group )
                                ASSIGNING FIELD-SYMBOL(<group>).

        INSERT VALUE #( purchase_group = <group>-purchase_group )
               INTO TABLE et_strategy_data
               ASSIGNING FIELD-SYMBOL(<ls_strategy_data>).

        LOOP AT GROUP <group> ASSIGNING FIELD-SYMBOL(<ls_group_strategy>).
          DATA(lv_key) = |{ <ls_group_strategy>-group }{ <ls_group_strategy>-strategy }|.

          <ls_strategy_data>-purchase_group_desc = <ls_group_strategy>-purchase_group_desc.

          INSERT CORRESPONDING #( <ls_group_strategy> ) INTO TABLE <ls_strategy_data>-strategies ASSIGNING FIELD-SYMBOL(<ls_strategies>).
          " Sacamos el importe
          READ TABLE lt_classif_complete ASSIGNING FIELD-SYMBOL(<ls_classif_complete>)
                                         WITH KEY objek = lv_key
                                                  atinn = zif_rel_data=>cs_strategy-classification-fields_charac-gnetw.
          IF sy-subrc = 0.
            <ls_strategies>-amount = <ls_classif_complete>-value_amount.
            <ls_strategies>-amount2 = <ls_classif_complete>-value_amount2.
            <ls_strategies>-amount_char = <ls_classif_complete>-value.
            <ls_strategies>-amount_operand = <ls_classif_complete>-value_operand.
            <ls_strategies>-currency = <ls_classif_complete>-value_currency.
          ENDIF.

          " Ahora los aprobadores
          LOOP AT lt_approvers ASSIGNING FIELD-SYMBOL(<ls_approvers>) WHERE group = <ls_group_strategy>-group
                                                                            AND strategy = <ls_group_strategy>-strategy.

            INSERT CORRESPONDING #( <ls_approvers> ) INTO TABLE <ls_strategies>-approvers ASSIGNING FIELD-SYMBOL(<ls_strag_approv>).


          ENDLOOP.

        ENDLOOP.
        " Ahora ordenamos las estrategias por los valores de los importes y operandos.
        sort_strategies( CHANGING ct_strategies = <ls_strategy_data>-strategies ).
      ENDLOOP.
    ENDIF.

  ENDMETHOD.

  METHOD get_approvers_strategy.
    DATA lt_r_group TYPE zif_rel_data=>tt_r_liberation_group.
    DATA lt_r_code TYPE zif_rel_data=>tt_r_liberation_code.
    CLEAR: et_approvers.

    " Saco los datos en bruto de las estrategias de liberacion
    SELECT * INTO TABLE @DATA(lt_t16fs)
          FROM t16fs
            FOR ALL ENTRIES IN @it_group_strag
          WHERE frggr = @it_group_strag-group
                AND frgsx = @it_group_strag-strategy.
    IF sy-subrc = 0.

      LOOP AT lt_t16fs ASSIGNING FIELD-SYMBOL(<ls_t16fs>).
        INSERT VALUE #( sign = 'I' option = 'EQ' low = <ls_t16fs>-frggr ) INTO TABLE lt_r_group.

        DATA(lt_strategy) = convert_t16fs_row_data( EXPORTING is_t16fs = <ls_t16fs> ).

        lt_r_code = VALUE #( BASE lt_r_code FOR <wa> IN lt_strategy ( sign = 'I' option = 'EQ' low = <wa>-code ) ).

        et_approvers = CORRESPONDING #( BASE ( et_approvers ) lt_strategy ).

      ENDLOOP.

      " Ahora saco los usuarios asociados a los c??digos
      get_users_from_lib_group_code(
        EXPORTING
          it_r_code         = lt_r_code
          it_r_group        = lt_r_group
        IMPORTING
          et_user_lib_group = DATA(lt_user_lib_group) ).
      IF lt_user_lib_group IS NOT INITIAL.
        LOOP AT et_approvers ASSIGNING FIELD-SYMBOL(<ls_approvers>).
          READ TABLE lt_user_lib_group ASSIGNING FIELD-SYMBOL(<ls_user_lib_group>)
                                       WITH KEY code = <ls_approvers>-code
                                                group = <ls_approvers>-group.
          IF sy-subrc = 0.
            <ls_approvers>-username = <ls_user_lib_group>-username.
            <ls_approvers>-username_desc = <ls_user_lib_group>-username_desc.
          ENDIF.

        ENDLOOP.
      ENDIF.

    ENDIF.
  ENDMETHOD.


  METHOD get_group_strag_from_pgroup.
    DATA lt_r_ekgrp TYPE zif_rel_data=>tt_r_purchase_group.

    CLEAR: et_group_strag.

    " Sacamos el grupo y estrateg??a de liberaci??n a partir del grupo de compras
    DATA(lt_params_sl) = VALUE pivb_rsparamsl_255_t( ( selname = zif_rel_data=>cs_strategy-classification-query-fields-atinn
                                                       kind = 'P'
                                                       low = zif_rel_data=>cs_strategy-classification-fields_charac-ekgrp )  ).

    IF iv_purchase_group IS NOT INITIAL.
      lt_params_sl = VALUE #( BASE lt_params_sl ( selname = zif_rel_data=>cs_strategy-classification-query-fields-atwrt
                                                  kind = 'S'
                                                  sign = 'I'
                                                  option = 'EQ'
                                                  low = iv_purchase_group ) ).
      INSERT VALUE #( sign = 'I' option = 'EQ' low = iv_purchase_group ) INTO TABLE lt_r_ekgrp.
    ELSE.
      lt_params_sl = VALUE #( BASE lt_params_sl FOR <wa> IN it_r_purchase_group ( selname = zif_rel_data=>cs_strategy-classification-query-fields-atwrt
                                                  kind = 'S'
                                                  sign = <wa>-sign
                                                  option = <wa>-option
                                                  low = <wa>-low
                                                  high = <wa>-high  ) ).
      lt_r_ekgrp = it_r_purchase_group.
    ENDIF.

    query_classification( EXPORTING it_params_sl = lt_params_sl
                          IMPORTING et_data = DATA(lt_classif_pgroup) ).

    IF lt_classif_pgroup IS NOT INITIAL.

      " Buscamos las descripciones de las grupos de compra
      IF iv_get_desc = abap_true.
        SELECT ekgrp, eknam INTO TABLE @DATA(lt_t024)
               FROM t024
               WHERE ekgrp IN @lt_r_ekgrp.

      ENDIF.

      LOOP AT lt_classif_pgroup ASSIGNING FIELD-SYMBOL(<ls_classif_pgroup>).
        INSERT VALUE #( group = <ls_classif_pgroup>-objek(2)
                        strategy = <ls_classif_pgroup>-objek+2(2)
                        purchase_group = <ls_classif_pgroup>-value ) INTO TABLE et_group_strag ASSIGNING FIELD-SYMBOL(<ls_group_strag>).

        IF iv_get_desc = abap_true.
          READ TABLE lt_t024 ASSIGNING FIELD-SYMBOL(<ls_t024>) WITH KEY ekgrp = <ls_classif_pgroup>-value.
          IF sy-subrc = 0.
            <ls_group_strag>-purchase_group_desc = <ls_t024>-eknam.
          ENDIF.
        ENDIF.
      ENDLOOP.

      " Leemos los grupos de liberaci??n que se tienen que excluir
      SELECT 'I' AS sign, 'EQ' AS option, frggr AS low, frggr AS high
            INTO TABLE @DATA(lt_r_excl_group)
            FROM zrel_t025.
      IF sy-subrc = 0.
        DELETE et_group_strag WHERE group IN lt_r_excl_group.
      ENDIF.



    ENDIF.
  ENDMETHOD.

  METHOD get_users_from_lib_group_code.
    DATA lt_r_code TYPE zif_rel_data=>tt_r_liberation_code.
    DATA lt_r_group TYPE zif_rel_data=>tt_r_liberation_group.

    IF iv_code IS NOT INITIAL.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = iv_code ) INTO TABLE lt_r_code.
    ELSE.
      lt_r_code = it_r_code.
    ENDIF.

    IF iv_group IS NOT INITIAL.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = iv_group ) INTO TABLE lt_r_group.
    ELSE.
      lt_r_group = it_r_group.
    ENDIF.

    SELECT objid AS username frggr AS group frgco AS code INTO CORRESPONDING FIELDS OF TABLE et_user_lib_group
           FROM t16fw
           WHERE frggr IN lt_r_group
                 AND frgco IN lt_r_code.

    IF sy-subrc = 0.
      " Saco las descripciones si se ha indicado
      IF iv_get_desc = abap_true.

        complete_lib_code_desc( CHANGING ct_user_lib_group = et_user_lib_group ).

      ENDIF.

    ENDIF.


  ENDMETHOD.


  METHOD complete_lib_code_desc.

    IF ct_user_lib_group IS INITIAL. EXIT. ENDIF.


    " Leo las denominaciones por si el usuario no existe en el sistema y no se encuentra su nombre
    SELECT * INTO TABLE @DATA(lt_lib_group_desc)
           FROM t16fd
           FOR ALL ENTRIES IN @ct_user_lib_group
           WHERE frggr = @ct_user_lib_group-group
                 AND frgco = @ct_user_lib_group-code.

    LOOP AT ct_user_lib_group ASSIGNING FIELD-SYMBOL(<ls_user_lib_group>).
      " Primero del maestro de usuario
      <ls_user_lib_group>-username_desc = get_username_desc( <ls_user_lib_group>-username ).

      " Si no existe de las denominaciones del codigo de liberaci??n
      IF <ls_user_lib_group>-username_desc IS INITIAL.
        " Primero buscamos por el idioma global
        READ TABLE lt_lib_group_desc ASSIGNING FIELD-SYMBOL(<ls_lib_group_desc>)
                                     WITH KEY spras = mv_langu
                                              frggr = <ls_user_lib_group>-group
                                              frgco = <ls_user_lib_group>-code.
        IF sy-subrc NE 0.
          " Si no existe por el ingles
          READ TABLE lt_lib_group_desc ASSIGNING <ls_lib_group_desc>
                                       WITH KEY spras = 'E'
                                                frggr = <ls_user_lib_group>-group
                                                frgco = <ls_user_lib_group>-code.
          IF sy-subrc NE 0.
            " Si no existe por el primero que se encuentre
            READ TABLE lt_lib_group_desc ASSIGNING <ls_lib_group_desc>
                                         WITH KEY frggr = <ls_user_lib_group>-group
                                                  frgco = <ls_user_lib_group>-code.
          ENDIF.
        ENDIF.

        " Si he encontrado texto del codigo se lo informo. Sino, pongo como texto el
        " c??digo de usuario
        IF <ls_lib_group_desc> IS ASSIGNED.
          <ls_user_lib_group>-username_desc = <ls_lib_group_desc>-frgct.
        ELSE.
          <ls_user_lib_group>-username_desc = <ls_user_lib_group>-username.
        ENDIF.

        UNASSIGN <ls_lib_group_desc>.
      ENDIF.


    ENDLOOP.

  ENDMETHOD.


  METHOD sort_strategies.
    DATA lt_strategies TYPE zif_rel_data=>tt_strategy_data .

    " Primero voy a ordenar por importe de menor a mayor. No servir?? casi nunca
    " porque generalmente los importes son iguales. Pero para los casos que no lo son
    " ya tengo una ordenaci??n previa.
    SORT ct_strategies BY amount.

    " Ahora vamos a recorrer las estrategias para ver si el siguiente registro es igual. Si lo es
    " vamos a mirar el operando. El proceso se realizar?? hasta que este todo ordenado.
    DO.
      DATA(lv_change_made) = abap_false.

      LOOP AT ct_strategies ASSIGNING FIELD-SYMBOL(<ls_strategies>).
        DATA(lv_tabix) = sy-tabix + 1.

        READ TABLE ct_strategies ASSIGNING FIELD-SYMBOL(<ls_strategies_next>) INDEX lv_tabix.
        IF sy-subrc = 0.
          IF <ls_strategies>-amount = <ls_strategies_next>-amount.

            " Si el registro tiene el operndo de >/>= y el siguiente tiene los operandos de </<=/= entonces
            " se a??ade el registro siguiente antes que el actual.
            " Nota Iv??n: Tal como he visto la configuraci??n de las estrategias, si tengo el >/>= el registro actual el siguiente no puede
            " tener tambi??n el mismo operando, tendr?? que tener los operados de inferioridad o igual, pero por si acaso, a??ado el control.
            IF ( <ls_strategies>-amount_operand = '>' OR <ls_strategies>-amount_operand = '>=' )
               AND  ( <ls_strategies_next>-amount_operand = '<' OR <ls_strategies_next>-amount_operand = '<='
                      OR <ls_strategies_next>-amount_operand = '=' ).
              INSERT <ls_strategies_next> INTO TABLE lt_strategies.

              lv_change_made = abap_true.

              " Si el registro actual es un igual y el siguiente es menor tambi??n se hace el cambio.
            ELSEIF ( <ls_strategies>-amount_operand = '=' )
             AND  ( <ls_strategies_next>-amount_operand = '<' ).

              INSERT <ls_strategies_next> INTO TABLE lt_strategies.
              lv_change_made = abap_true.

              " En el registro actual tenemos que es > o >= y el siguiente registro tenemos un intervalo.
              " En ese caso miramos si el importe es superior o igual al importe hasta del siguiente. Si es as??,
              " se hace el cambio.
            ELSEIF ( <ls_strategies>-amount_operand = '>' OR <ls_strategies>-amount_operand = '>=' )
                   AND <ls_strategies_next>-amount_operand = '-'.
              IF <ls_strategies>-amount >= <ls_strategies_next>-amount2.
                INSERT <ls_strategies_next> INTO TABLE lt_strategies.
                lv_change_made = abap_true.
              ENDIF.
              " Si tenemos en el actual un < o <= y el siguiente registro un intervalo, miramos si el importe actual
              " es superior al importe hasta del intervalo es menor al actual. Si es as?? se hace el cambio
            ELSEIF ( <ls_strategies_next>-amount_operand = '<' OR <ls_strategies_next>-amount_operand = '<=' )
                   AND <ls_strategies>-amount_operand = '-'.
              IF <ls_strategies_next>-amount <= <ls_strategies>-amount.
                INSERT <ls_strategies_next> INTO TABLE lt_strategies.
                lv_change_made = abap_true.
              ENDIF.
            ENDIF.

          ENDIF.

        ENDIF.

        " El registro actual siempre se inserta da igual si se ordena, o no. Si se ordena, se a??adir??
        " primero el siguiente y luego el actual.
        " Pero hay que tener en cuenta en que el registro que este procesando no se haya insertado cuando
        " se ha hecho la ordenaci??n en registros anteriores.
        READ TABLE lt_strategies TRANSPORTING NO FIELDS WITH KEY group = <ls_strategies>-group
                                                                 strategy = <ls_strategies>-strategy.
        IF sy-subrc NE 0.
          INSERT <ls_strategies> INTO TABLE lt_strategies.
        ENDIF.


      ENDLOOP.

      " Volcamos los registros al par??metro de salida
      ct_strategies = lt_strategies.
      CLEAR: lt_strategies.

      " Si no se ha hecho ningun cambio es que esta todo ordenado y se sale.
      IF lv_change_made = abap_false. EXIT. ENDIF.

    ENDDO.

    "Ponemos el nivel
    LOOP AT ct_strategies ASSIGNING <ls_strategies>.
      <ls_strategies>-level = sy-tabix.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_depart_from_users_auth.

    CLEAR: et_dept_subs.

    " Se instancia la clase que har?? el control de autorizaciones
    DATA(lo_auth) = NEW zcl_rel_user_authorizations( iv_user = iv_user ).

    DATA(lv_user_desc) = get_username_desc( iv_user ).

    " Ahora los departamentos.
    SELECT a~dept_subs, b~description AS dept_subs_desc INTO TABLE @DATA(lt_departments)
           FROM zrel_t002 AS a LEFT OUTER JOIN zrel_t002t AS b ON
                b~dept_subs = a~dept_subs
                AND b~spras = @mv_langu.

    IF sy-subrc = 0.

      LOOP AT lt_departments ASSIGNING FIELD-SYMBOL(<ls_departaments>).
        " Preparo la estructura a insertar
        DATA(ls_dept_subs) = VALUE ts_user_depart_subs( username = iv_user
                                                        username_desc = lv_user_desc
                                                        dept_subs = <ls_departaments>-dept_subs
                                                        dept_subs_desc = <ls_departaments>-dept_subs_desc ).

        " Se mira que autorizaci??n tiene a nivel de departamento
        IF lo_auth->authority_check( iv_actvt = zif_rel_auth_data=>cs_actvt_auth_check-edit iv_dept_subs = <ls_departaments>-dept_subs ).
          ls_dept_subs-can_edit = abap_true.
          INSERT ls_dept_subs INTO TABLE et_dept_subs.

        ELSEIF lo_auth->authority_check( iv_actvt = zif_rel_auth_data=>cs_actvt_auth_check-view iv_dept_subs = <ls_departaments>-dept_subs ).
          ls_dept_subs-can_edit = abap_false.
          INSERT ls_dept_subs INTO TABLE et_dept_subs.

        ENDIF.

      ENDLOOP.

    ENDIF.

  ENDMETHOD.

  METHOD get_pgroup_info.
    DATA lt_r_purchase_group TYPE zif_rel_data=>tt_r_dept_subs.

    CLEAR: et_info.

    IF iv_purchase_group IS NOT INITIAL.
      INSERT VALUE #( sign = 'I' option = 'EQ' low = iv_purchase_group ) INTO TABLE lt_r_purchase_group.
    ELSE.
      lt_r_purchase_group = it_r_purchase_group.
    ENDIF.

    SELECT ekgrp AS purchase_group eknam AS purchase_group_desc INTO TABLE et_info
           FROM t024
           WHERE ekgrp IN lt_r_purchase_group.
  ENDMETHOD.

  METHOD get_release_group_from_pgroup.
    CLEAR: rv_group.

    " Para obtenerlo lo haremos en varios pasos.

    " El primer es mirar si el grupo ya esta asignado algun grupo de liberaci??n. Para ello hay que ir al sistema de clasificaci??n.
    DATA(lt_params_sl) = VALUE pivb_rsparamsl_255_t( ( selname = zif_rel_data=>cs_strategy-classification-query-fields-atinn
                                                       kind = 'P'
                                                       low = zif_rel_data=>cs_strategy-classification-fields_charac-ekgrp )
                                                      ( selname = zif_rel_data=>cs_strategy-classification-query-fields-atwrt
                                                        kind = 'S'
                                                        sign = 'I'
                                                        option = 'EQ'
                                                        low = iv_purchase_group )   ).

    query_classification( EXPORTING it_params_sl = lt_params_sl
                          IMPORTING et_data = DATA(lt_classif_pgroup) ).

    IF lt_classif_pgroup IS NOT INITIAL.
      rv_group = lt_classif_pgroup[ 1 ]-objek(2).
    ELSE.
      " No tengo el grupo en ningun grupo asignado.

      " PENDIENTE DEFINIR

    ENDIF.

  ENDMETHOD.

  METHOD get_strategys_from_group.
    DATA lt_r_group TYPE zif_rel_data=>tt_r_liberation_code.
    DATA lv_langu TYPE sylangu.

    CLEAR: rt_values.

    IF iv_group IS NOT INITIAL.
      lt_r_group = VALUE #( ( sign = 'I' option = 'EQ' low = iv_group ) ).
    ELSE.
      lt_r_group = it_r_group.
    ENDIF.

    " Si hay idioma pasado por par??metro tiene prefierencia sobre el global.
    " Este m??todo tiene idioma porque se usa en la creaci??n de nuevas estrategias y necesito
    " poder recuperar textos en idiomas distintos al del constructor, o buscarlos todos.
    IF iv_langu IS NOT INITIAL.
      lv_langu = iv_langu.
    ELSE.
      lv_langu = mv_langu.
    ENDIF.

    SELECT a~frggr a~frgsx b~frgxt
           a~frgc1 a~frgc2 a~frgc3 a~frgc4 a~frgc5
           a~frgc6 a~frgc7 a~frgc8 a~frgex
           INTO TABLE rt_values
           FROM t16fs AS a LEFT OUTER JOIN t16ft AS b ON
                b~frggr = a~frggr
                AND b~frgsx = a~frgsx
                AND b~spras = lv_langu
           WHERE a~frggr IN lt_r_group.


  ENDMETHOD.

  METHOD get_release_group_from_company.

    CLEAR: rv_group.

    SELECT SINGLE b~frggr INTO rv_group
           FROM t001 AS a INNER JOIN zrel_t024 AS b ON
                b~land1 = a~land1
           WHERE a~bukrs = iv_company.

  ENDMETHOD.

  METHOD check_exist_strateg_classif.
    rv_exist = abap_false.

    DATA(lv_objectkey) = CONV objnum( |{ iv_group }{ iv_strategy }| ).

    SELECT @abap_true INTO @rv_exist
           FROM ausp UP TO 1 ROWS
           WHERE objek = @lv_objectkey
                 AND klart = @zif_rel_data=>cs_strategy-classification-klart.
    ENDSELECT.



  ENDMETHOD.

  METHOD get_descriptions_release_code.
    DATA lt_r_group TYPE zif_rel_data=>tt_r_liberation_group.
    DATA lt_r_code TYPE zif_rel_data=>tt_r_liberation_code.
    DATA lt_r_langu TYPE RANGE OF sy-langu.

    CLEAR: rt_descriptions.

    IF iv_group IS NOT INITIAL.
      lt_r_group = VALUE #( ( sign = 'I' option = 'EQ' low = iv_group ) ).
    ELSE.
      lt_r_group = it_r_group.
    ENDIF.

    IF iv_code IS NOT INITIAL.
      lt_r_code = VALUE #( ( sign = 'I' option = 'EQ' low = iv_code ) ).
    ELSE.
      lt_r_code = it_r_code.
    ENDIF.

    " El idioma si no viene informado no se pasar?? el global, porque aqu?? me interesa que se buscar por todos.
    IF iv_langu IS NOT INITIAL.
      lt_r_langu = VALUE #( ( sign = 'I' option = 'EQ' low = iv_langu ) ).
    ENDIF.

    SELECT * INTO TABLE rt_descriptions
           FROM t16fd
           WHERE spras IN lt_r_langu
                 AND frggr IN lt_r_group
                 AND frgco IN lt_r_code.

  ENDMETHOD.

  METHOD get_liberation_statuses.
    DATA lt_r_group TYPE zif_rel_data=>tt_r_liberation_group.
    DATA lt_r_strategy TYPE zif_rel_data=>tt_r_strategy_code.

    CLEAR:  et_t16fk, et_t16fv.

    IF iv_group IS NOT INITIAL.
      lt_r_group = VALUE #( ( sign = 'I' option = 'EQ' low = iv_group ) ).
    ELSE.
      lt_r_group = it_r_group.
    ENDIF.

    IF iv_strategy IS NOT INITIAL.
      lt_r_strategy = VALUE #( ( sign = 'I' option = 'EQ' low = iv_strategy ) ).
    ELSE.
      lt_r_strategy = it_r_strategy.
    ENDIF.

    " Solo se buscan si tabla se ha pasado por par??metro.
    " Estados de liberaci??n
    IF et_t16fk IS SUPPLIED.
      SELECT * INTO TABLE et_t16fk
             FROM t16fk
             WHERE frggr IN lt_r_group
                   AND frgsx IN lt_r_strategy.
    ENDIF.

    " Estados de liberaci??n
    IF et_t16fv IS SUPPLIED.
      SELECT * INTO TABLE et_t16fv
             FROM t16fv
             WHERE frggr IN lt_r_group
                   AND frgsx IN lt_r_strategy.
    ENDIF.

  ENDMETHOD.

  METHOD get_lib_group_from_pgroup.
    CLEAR: rv_group.

    " El grupo de liberaci??n primero voy a obtener del grupo de compras. Si no es posible entonces crear?? uno ficticio.
    rv_group = get_release_group_from_pgroup( iv_purchase_group ).
    " Si no tengo grupo es que estoy en una creaci??n por lo tanto sacar?? el grupo en base a la sociedad.
    IF rv_group IS INITIAL AND iv_company IS NOT INITIAL..
      rv_group = get_release_group_from_company( iv_company ).
    ENDIF.

  ENDMETHOD.

  METHOD get_liberation_group_list.

    CLEAR: rt_list.

    SELECT 'E' AS sign, 'EQ' AS option, frggr AS low, frggr AS high INTO TABLE @DATA(lt_r_excl)
           FROM zrel_t025.

    SELECT a~frggr AS group b~frggt AS group_desc
           INTO TABLE rt_list
           FROM t16fg AS a LEFT OUTER JOIN t16fh AS b ON
                b~frggr = a~frggr
                AND b~spras = mv_langu
           WHERE a~frgkl = zif_rel_data=>cs_strategy-classification-klasse
                 AND a~frggr IN lt_r_excl.

  ENDMETHOD.

ENDCLASS.
